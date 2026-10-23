# Plot function to use in run.R script

make_cpi_plots <- function(con,
                           output_dir = "output/figures",
                           print_plots = interactive()) {

  monthly_plot <- dbGetQuery(
    con,
    "
    SELECT Bestedingscategorieen, Perioden, CPI_1
    FROM monthly_cpi
    WHERE Perioden BETWEEN ? AND ?
    ORDER BY Perioden
    ",
    params = list("1996MM01", "2025MM12")
  )

  quarterly_plot <- dbGetQuery(
    con,
    "
    SELECT *
    FROM quarterly_changes
    WHERE verslagperiode BETWEEN ? AND ?
    ORDER BY verslagperiode
    ",
    params = list("1996Q2", "2025Q4")
  )

  # Give products readable names
  product_label <- function(x) {
    case_when(
      x == "CPI011830" ~ "Chocolade",
      x == "CPI045100" ~ "Elektriciteit",
      TRUE ~ x
    )
  }

  # Monthly plot data
  monthly_plot <- monthly_plot %>%
    mutate(
      date = as.Date(paste0(
        substr(Perioden, 1, 4), "-",
        substr(Perioden, 7, 8), "-01"
      )),
      product = product_label(Bestedingscategorieen),
      date,
      CPI_1 = round(CPI_1,2)
    )
  
  # Quarterly plot data
  quarterly_plot <- quarterly_plot %>%
    mutate(
      year = as.integer(substr(verslagperiode, 1, 4)),
      quarter = as.integer(sub(".*Q", "", verslagperiode)),
      date = as.Date(sprintf(
        "%04d-%02d-01",
        year,
        (quarter - 1) * 3 + 1
      )),
      product = product_label(Bestedingscategorieen),
      quarterly_change_pct = round(quarterly_change_pct,2),
      current_cpi = round(current_cpi,2), 
      previous_cpi = round(previous_cpi,2)
    )

  # Consistent colours
  kleuren <- c(
    "Chocolade" = "#89564A",
    "Elektriciteit" = "#187F9B"
  )

  # Reusable theme for all figures
  theme_cbs <- theme_minimal(base_size = 13) +
    theme(
      # Center titles and subtitles
      plot.title = element_text(
        size = 19,
        face = "bold",
        hjust = 0.5,
        colour = "#263746",
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        size = 11,
        hjust = 0.5,
        colour = "#687785",
        margin = margin(b = 18)
      ),

      # Center source captions
      plot.caption = element_text(
        size = 9,
        hjust = 0.5,
        colour = "#778591",
        margin = margin(t = 18)
      ),

      plot.title.position = "plot",
      plot.caption.position = "plot",

      # Axis styling
      axis.title = element_text(
        size = 11,
        colour = "#465564"
      ),
      axis.text = element_text(
        size = 10,
        colour = "#63717D"
      ),

      # Subtle grid
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        colour = "#E7EBEF",
        linewidth = 0.35
      ),

      # Facet styling
      strip.text = element_text(
        size = 12,
        face = "bold",
        colour = "#344554"
      ),
      strip.background = element_rect(
        fill = "#F2F5F7",
        colour = NA
      ),
      panel.spacing = grid::unit(1.3, "lines"),

      legend.position = "bottom",
      legend.title = element_blank(),

      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      plot.margin = margin(20, 25, 15, 25)
    )


  # Generates the monthly CPI plot
  p1 <- monthly_plot %>%
    ggplot(aes(x = date, y = CPI_1, colour = product)) +

    geom_hline(
      yintercept = 100,
      linetype = "dashed",
      colour = "#B9C1C8",
      linewidth = 0.8
    ) +

    geom_line(
      linewidth = 0.9,
      lineend = "round",
      na.rm = TRUE
    ) +

    facet_wrap(
      ~ product,
      ncol = 1,
      scales = "free_y"
    ) +

    scale_colour_manual(values = kleuren) +

    scale_x_date(
      date_breaks = "5 years",
      date_labels = "%Y",
      expand = expansion(mult = c(0.01, 0.02))
    ) +

    scale_y_continuous(
      labels = scales::label_number(
        decimal.mark = ",",
        big.mark = "."
      )
    ) +

    labs(
      title = "Ontwikkeling van de consumentenprijzen",
      subtitle = "Maandelijkse prijsindex van chocolade en elektriciteit | 1996–2025",
      x = "Jaar",
      y = "Consumentenprijsindex (2015 = 100)",
      caption = paste(
        "Stippellijn: basisjaar 2015 = 100 |",
        "Verticale schalen verschillen per product"
      )
    ) +

    theme_cbs +
    theme(legend.position = "none")

  if (isTRUE(print_plots)) print(p1)

  # Generates the Quarterly CPI plot
  p2 <- quarterly_plot %>%
    ggplot(aes(x = date, y = current_cpi, colour = product)) +

    geom_hline(
      yintercept = 100,
      linetype = "dashed",
      colour = "#B9C1C8",
      linewidth = 0.8
    ) +

    geom_line(
      linewidth = 0.9,
      lineend = "round"
    ) +

    geom_point(
      size = 0.9,
      alpha = 0.75
    ) +

    facet_wrap(
      ~ product,
      ncol = 1,
      scales = "free_y"
    ) +

    scale_colour_manual(values = kleuren) +

    scale_x_date(
      date_breaks = "5 years",
      date_labels = "%Y",
      expand = expansion(mult = c(0.01, 0.02))
    ) +

    scale_y_continuous(
      labels = scales::label_number(
        decimal.mark = ",",
        big.mark = "."
      )
    ) +

    labs(
      title = "Gemiddelde consumentenprijsindex per kwartaal",
      subtitle = "Berekend als het gemiddelde van drie maandelijkse CPI-cijfers",
      x = "Jaar",
      y = "Gemiddelde CPI (2015 = 100)",
      caption = paste(
        "Verticale schalen verschillen per product"
      )
    ) +

    theme_cbs +
    theme(legend.position = "none")

  if (isTRUE(print_plots)) print(p2)


  # Generates the Quarterly change in CPI plot
  p3 <- quarterly_plot %>%
    ggplot(
      aes(
        x = date,
        y = quarterly_change_pct,
        fill = product
      )
    ) +

    geom_hline(
      yintercept = 0,
      colour = "#344554",
      linewidth = 0.8
    ) +

    geom_col(
      width = 75,
      alpha = 0.9
    ) +

    facet_wrap(
      ~ product,
      ncol = 1,
      scales = "free_y"
    ) +

    scale_fill_manual(values = kleuren) +

    scale_x_date(
      date_breaks = "5 years",
      date_labels = "%Y",
      expand = expansion(mult = c(0.01, 0.02))
    ) +

    scale_y_continuous(
      labels = scales::label_number(
        suffix = "%",
        decimal.mark = ",",
        accuracy = 0.1
      )
    ) +

    labs(
      title = "Kwartaalmutatie van de consumentenprijzen",
      subtitle = "Procentuele verandering ten opzichte van het voorgaande kwartaal",
      x = "Jaar",
      y = "Kwartaalmutatie (%)",
      caption = paste(
        "Verticale schalen verschillen per product"
      )
    ) +

    theme_cbs +
    theme(legend.position = "none")

  if (isTRUE(print_plots)) print(p3)

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  ggsave(
    file.path(output_dir, "monthly_cpi.png"),
    plot = p1,
    width = 10,
    height = 5,
    dpi = 300
  )

  ggsave(
    file.path(output_dir, "quarterly_cpi.png"),
    plot = p2,
    width = 10,
    height = 5,
    dpi = 300
  )

  ggsave(
    file.path(output_dir, "quarterly_change.png"),
    plot = p3,
    width = 10,
    height = 7,
    dpi = 300
  )

  invisible(list(
    monthly_cpi = p1,
    quarterly_cpi = p2,
    quarterly_change = p3
  ))
}
