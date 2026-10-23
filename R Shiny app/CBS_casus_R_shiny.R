# CBS Prijsmonitor — intuïtieve periodekeuze met jaar, kwartaal en snelkeuzes
# Open vanuit de projecthoofdmap met shiny::runApp("app")
# Sla dit bestand op als app/app.R

library(shiny)
library(bslib)
library(plotly)
library(ggplot2)
library(dplyr)
library(DBI)
library(RSQLite)
library(DT)
library(magrittr)

# 1. Locate the existing SQLite database

# The working directory is app/ when using shiny::runApp("app").
# Support the project's main data/ folder and older layouts.
db_candidates <- c(
file.path( "data", "cbs_cpi.sqlite")
)

db_path <- db_candidates[file.exists(db_candidates)][1]

if (is.na(db_path)) {
  stop(
    "Kan cbs_cpi.sqlite niet vinden. Zet de database in ",
    "app/data/, data/ of R/data/ en start de app vanuit app/."
  )
}

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = db_path,
  flags = RSQLite::SQLITE_RO
)

onStop(function() {
  if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
})

required_tables <- c("monthly_cpi", "quarterly_changes")
if (!all(required_tables %in% DBI::dbListTables(con))) {
  stop("SQLite mist monthly_cpi of quarterly_changes. Bereken en sla eerst de kwartaalresultaten op.")
}

# Check table schemas before rendering; otherwise errors appear only when opening a tab.
required_fields <- list(
  monthly_cpi = c("Bestedingscategorieen", "Perioden", "CPI_1"),
  quarterly_changes = c(
    "Bestedingscategorieen", "verslagperiode",
    "previous_cpi", "current_cpi", "quarterly_change_pct"
  )
)
for (table_name in names(required_fields)) {
  missing_fields <- setdiff(
    required_fields[[table_name]], DBI::dbListFields(con, table_name)
  )
  if (length(missing_fields)) {
    stop("Kolommen ontbreken in ", table_name, ": ",
         paste(missing_fields, collapse = ", "))
  }
}

# 2. Restrict the dashboard to chocolate and electricity.
product_labels <- c(
  "CPI011830" = "Chocolade",
  "CPI045100" = "Elektriciteit"
)

label_product <- function(codes) {
  codes <- as.character(codes)
  labels <- unname(product_labels[codes])
  labels[is.na(labels)] <- codes[is.na(labels)]
  labels
}

monthly_codes <- DBI::dbGetQuery(
  con,
  "SELECT DISTINCT Bestedingscategorieen FROM monthly_cpi"
)$Bestedingscategorieen

quarterly_codes <- DBI::dbGetQuery(
  con,
  "SELECT DISTINCT Bestedingscategorieen FROM quarterly_changes"
)$Bestedingscategorieen

wanted_codes <- names(product_labels)
product_codes <- wanted_codes[
  wanted_codes %in% monthly_codes & wanted_codes %in% quarterly_codes
]

if (!length(product_codes)) {
  stop(
    "De SQLite-database bevat geen complete maand- en kwartaalgegevens ",
    "voor chocolade (CPI011830) of elektriciteit (CPI045100)."
  )
}

# A single-select input: chocolate, electricity, or both (if available).
product_choices <- setNames(product_codes, label_product(product_codes))
if (length(product_codes) == 2) {
  product_choices <- c("Beide producten" = "both", product_choices)
}
default_product <- if (length(product_codes) == 2L) "both" else product_codes[1]
missing_codes <- setdiff(wanted_codes, product_codes)

# Quarter boundaries come from stored quarterly results, not the monthly table.
placeholders <- paste(rep("?", length(product_codes)), collapse = ", ")
bounds <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT MIN(verslagperiode) AS first_quarter, ",
    "MAX(verslagperiode) AS last_quarter ",
    "FROM quarterly_changes WHERE Bestedingscategorieen IN (",
    placeholders, ")"
  ),
  params = as.list(product_codes)
)

first_quarter <- bounds$first_quarter[1]
last_quarter <- bounds$last_quarter[1]
if (is.na(first_quarter) || is.na(last_quarter) ||
    !grepl("^[0-9]{4}Q[1-4]$", first_quarter) ||
    !grepl("^[0-9]{4}Q[1-4]$", last_quarter)) {
  stop("Geen geldige kwartaalperioden gevonden in quarterly_changes.")
}

quarter_id <- function(x) {
  as.integer(substr(x, 1, 4)) * 4 + as.integer(sub(".*Q", "", x)) - 1
}
# The period picker uses a year and a quarter separately.
first_id <- quarter_id(first_quarter)
last_id <- quarter_id(last_quarter)
first_year <- first_id %/% 4L
last_year <- last_id %/% 4L
year_choices <- as.character(seq.int(first_year, last_year))
quarter_months <- c("jan–mrt", "apr–jun", "jul–sep", "okt–dec")
quarter_choices <- setNames(
  as.character(1:4),
  paste0("Q", 1:4, " (", quarter_months, ")")
)

# Only offer quarters for which a result can exist in the stored range.
# Example: if results begin at 1996Q2, Q1 is not offered for 1996.
quarter_choices_for_year <- function(year) {
  quarter_ids <- as.integer(year) * 4 + (1:4) - 1
  quarter_choices[quarter_ids >= first_id & quarter_ids <= last_id]
}

colours <- c(
  "Chocolade" = "#89564A",
  "Elektriciteit" = "#187F9B"
)

# Format hover values and month names in Dutch (without changing SQLite fields).
format_nl <- function(x, digits = 1) {
  formatC(x, format = "f", digits = digits,
          big.mark = ".", decimal.mark = ",")
}

maanden_nl <- c(
  "januari", "februari", "maart", "april", "mei", "juni",
  "juli", "augustus", "september", "oktober", "november", "december"
)

# 3. Shared plot styling.
theme_cbs <- function() {
theme_minimal(base_size = 13) +
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
}

# 4. User interface.
ui <- bslib::page_sidebar(
  title = "CBS Prijsmonitor",
  lang = "nl",
  fillable = FALSE,
  theme = bslib::bs_theme(
    version = 5,
    bg = "#F7F9FC",
    fg = "#263746",
    primary = "#187F9B"
  ),
  sidebar = bslib::sidebar(
    title = "Filters",
    width = 320,
    shiny::selectInput(
      "product", "Product",
      choices = product_choices,
      selected = default_product,
      multiple = FALSE
    ),
    if (length(missing_codes)) {
      tags$p(
        class = "sidebar-note",
        paste0(
          "Niet beschikbaar in beide SQLite-tabellen: ",
          paste(label_product(missing_codes), collapse = ", "), "."
        )
      )
    },
    tags$hr(),
    tags$div(
      class = "period-picker",
      tags$h5("Analyseperiode", class = "period-heading"),
      tags$p(
        class = "sidebar-note",
        paste0(
          "Kies een begin- en eindkwartaal. Alle tussenliggende kwartalen tellen mee. ",
          "Beschikbare kwartaalmutaties: ", first_quarter, " t/m ", last_quarter, "."
        )
      ),
      tags$div(
        class = "period-block",
        tags$div("Van", class = "period-block-title"),
        tags$div(
          class = "period-controls",
          shiny::selectInput(
            "from_year", "Jaar",
            choices = year_choices,
            selected = as.character(first_year), width = "100%"
          ),
          shiny::selectInput(
            "from_quarter", "Kwartaal",
            choices = quarter_choices_for_year(first_year),
            selected = as.character(first_id %% 4L + 1L), width = "100%"
          )
        )
      ),
      tags$div(
        class = "period-block",
        tags$div("Tot en met", class = "period-block-title"),
        tags$div(
          class = "period-controls",
          shiny::selectInput(
            "to_year", "Jaar",
            choices = year_choices,
            selected = as.character(last_year), width = "100%"
          ),
          shiny::selectInput(
            "to_quarter", "Kwartaal",
            choices = quarter_choices_for_year(last_year),
            selected = as.character(last_id %% 4L + 1L), width = "100%"
          )
        )
      ),
      tags$div("Snel kiezen", class = "period-presets-title"),
      tags$div(
        class = "period-presets",
        shiny::actionButton("preset_all", "Alles", class = "period-preset"),
        shiny::actionButton("preset_4", "Laatste 4 kwartalen", class = "period-preset"),
        shiny::actionButton("preset_20", "5 jaar", class = "period-preset"),
        shiny::actionButton("preset_40", "10 jaar", class = "period-preset")
      ),
      tags$div(
        class = "period-range-badge",
        tags$div("Geselecteerde periode", class = "period-range-label"),
        shiny::uiOutput("selected_period")
      )
    )
  ),
  tags$style(HTML("
    .bslib-page-sidebar {background: #F7F9FC;}
    .card {border: 1px solid #E4EAF0; border-radius: 15px; box-shadow: 0 4px 16px rgba(30,48,70,.035);}
    .card-header {background: white; border-bottom: 1px solid #E4EAF0;}
    .chart-title {text-align: center; font-size: 1.1rem; font-weight: 700; margin: 16px 8px 4px;}
    .chart-caption {text-align: center; color: #64748B; font-size: .82rem; margin: 6px 20px 20px;}
    .sidebar-note {color: #63758A; font-size: .85rem; line-height: 1.5;}
    .period-heading {font-weight: 750; font-size: 1.05rem; margin: 0 0 7px; color: #263746;}
    .period-block {padding: 12px 12px 4px; border: 1px solid #DFE8F0;
      background: white; border-radius: 12px; margin: 12px 0;}
    .period-block-title {font-size: .91rem; font-weight: 700; margin-bottom: 8px; color: #2C4154;}
    .period-controls {display: grid; grid-template-columns: 1.15fr 1fr; gap: 10px;}
    .period-controls .shiny-input-container {width: 100% !important;}
    .period-controls .form-group {margin-bottom: 9px;}
    .period-controls label {font-size: .82rem; color: #657589;}
    .period-presets-title {font-weight: 650; font-size: .88rem; margin: 15px 0 8px;}
    .period-presets {display: flex; flex-wrap: wrap; gap: 7px;}
    .period-preset {border: 1px solid #D7E3EB; background: #FFFFFF;
      border-radius: 999px; padding: 6px 11px; font-size: .78rem; color: #2E5263;}
    .period-preset:hover {background: #EAF2F5; border-color: #91BECD;}
    .period-range-badge {background: #EAF2F5; color: #24566A; border-radius: 11px;
      padding: 13px; margin-top: 16px; text-align: center;}
    .period-range-label {font-size: .74rem; font-weight: 600; color: #648293; margin-bottom: 5px;}
    .period-range-badge strong {font-size: .96rem;}
    .period-range-badge small {display: block; margin-top: 5px; font-size: .79rem; color: #527B89;}
    .period-range-badge .period-error {color: #A63232; font-size: .83rem; font-weight: 650;}
    .data-toolbar {display: flex; flex-wrap: wrap; align-items: end; gap: 14px; margin: 16px 0;}
  ")),
  tags$p(
    style = "color:#64748B; margin: 0 0 16px 0;",
    "CBS StatLine 83131NED · Consumentenprijsindex (2015 = 100)"
  ),
  bslib::layout_columns(
    bslib::value_box("Geselecteerde producten", shiny::textOutput("n_products"), theme = "light"),
    bslib::value_box("Maandobservaties", shiny::textOutput("n_monthly"), theme = "light"),
    bslib::value_box("Kwartaalobservaties", shiny::textOutput("n_quarterly"), theme = "light"),
    col_widths = c(4, 4, 4)
  ),
  bslib::navset_card_tab(
    full_screen = TRUE,
    bslib::nav_panel(
      "Maandelijkse CPI",
      div(class = "chart-title", "Ontwikkeling van de consumentenprijzen per maand"),
      plotly::plotlyOutput("monthly_chart", height = "570px"),
      div(class = "chart-caption",
          "CPI 2015 = 100 ·Verticale schalen verschillen per product")
    ),
    bslib::nav_panel(
      "Kwartaal-CPI",
      div(class = "chart-title", "Gemiddelde consumentenprijsindex per kwartaal"),
      plotly::plotlyOutput("quarterly_chart", height = "570px")
    ),
    bslib::nav_panel(
      "Kwartaalmutatie",
      div(class = "chart-title", "Procentuele verandering ten opzichte van het voorgaande kwartaal"),
      plotly::plotlyOutput("changes_chart", height = "570px")
      ),
    bslib::nav_panel(
      "Gegevens",
      div(class = "data-toolbar",
          shiny::selectInput(
            "table_type", "Gegevens weergeven",
            choices = c("Maandcijfers" = "monthly", "Kwartaalcijfers" = "quarterly"),
            selected = "quarterly", width = "230px"
          ),
          shiny::downloadButton("download_csv", "Download CSV")
      ),
      DT::DTOutput("data_table"),
      div(class = "chart-caption", "Alle getoonde cijfers komen uit de lokale SQLite-database.")
    )
  )
)

# 5. Server: filter inside SQLite, then render interactive graphics.
server <- function(input, output, session) {

  selected_products <- shiny::reactive({
    shiny::req(input$product)
    if (identical(input$product, "both")) product_codes else input$product
  })

  # Prevent selecting a quarter outside the available range when a year changes.
  shiny::observeEvent(input$from_year, {
    choices <- quarter_choices_for_year(input$from_year)
    shiny::req(length(choices) > 0)
    selected <- shiny::isolate(input$from_quarter)
    if (is.null(selected) || !(selected %in% unname(choices))) {
      selected <- unname(choices)[1]
    }
    shiny::updateSelectInput(
      session, "from_quarter", choices = choices, selected = selected
    )
  }, ignoreInit = FALSE)

  shiny::observeEvent(input$to_year, {
    choices <- quarter_choices_for_year(input$to_year)
    shiny::req(length(choices) > 0)
    selected <- shiny::isolate(input$to_quarter)
    if (is.null(selected) || !(selected %in% unname(choices))) {
      selected <- tail(unname(choices), 1)
    }
    shiny::updateSelectInput(
      session, "to_quarter", choices = choices, selected = selected
    )
  }, ignoreInit = FALSE)

  # Compute numeric quarter IDs from the separate year/quarter selectors.
  selected_ids <- shiny::reactive({
    shiny::req(input$from_year, input$from_quarter,
               input$to_year, input$to_quarter)
    list(
      from = as.integer(input$from_year) * 4L + as.integer(input$from_quarter) - 1L,
      to = as.integer(input$to_year) * 4L + as.integer(input$to_quarter) - 1L
    )
  })

  # Quick selections always end at the latest quarter available in SQLite.
  set_period <- function(from_id, to_id = last_id) {
    from_id <- max(first_id, as.integer(from_id))
    to_id <- min(last_id, as.integer(to_id))
    shiny::updateSelectInput(session, "from_year", selected = as.character(from_id %/% 4))
    shiny::updateSelectInput(session, "from_quarter", selected = as.character(from_id %% 4 + 1))
    shiny::updateSelectInput(session, "to_year", selected = as.character(to_id %/% 4))
    shiny::updateSelectInput(session, "to_quarter", selected = as.character(to_id %% 4 + 1))
  }

  shiny::observeEvent(input$preset_all, {
    set_period(first_id)
  })
  shiny::observeEvent(input$preset_4, {
    set_period(last_id - 3)
  })
  shiny::observeEvent(input$preset_20, {
    set_period(last_id - 19)
  })
  shiny::observeEvent(input$preset_40, {
    set_period(last_id - 39)
  })

  # Clear feedback for invalid ranges, without silently altering user choices.
  output$selected_period <- shiny::renderUI({
    ids <- selected_ids()

    if (ids$from > ids$to) {
      return(tags$span(class = "period-error",
                       "Het begin ligt na het einde."))
    }
    if (ids$from < first_id || ids$to > last_id) {
      return(tags$span(class = "period-error",
                       paste0("Beschikbaar: ", first_quarter, " t/m ", last_quarter)))
    }

    from_label <- paste0(ids$from %/% 4, " Q", ids$from %% 4 + 1)
    to_label <- paste0(ids$to %/% 4, " Q", ids$to %% 4 + 1)
    count <- ids$to - ids$from + 1
    tags$div(
      tags$strong(paste0(from_label, " – ", to_label)),
      tags$small(paste0(count, if (count == 1) " kwartaal" else " kwartalen"))
    )
  })

  periods <- shiny::reactive({
    ids <- selected_ids()
    shiny::validate(shiny::need(
      ids$from <= ids$to,
      "Het beginkwartaal moet vóór of gelijk aan het eindkwartaal liggen."
    ))
    shiny::validate(shiny::need(
      ids$from >= first_id && ids$to <= last_id,
      paste0("Beschikbare periode: ", first_quarter, " t/m ", last_quarter)
    ))

    q_from <- ids$from %% 4L + 1L
    q_to <- ids$to %% 4L + 1L
    year_from <- ids$from %/% 4L
    year_to <- ids$to %/% 4L

    list(
      month_from = sprintf("%04dMM%02d", year_from, (q_from - 1) * 3 + 1),
      month_to = sprintf("%04dMM%02d", year_to, q_to * 3),
      quarter_from = sprintf("%04dQ%d", year_from, q_from),
      quarter_to = sprintf("%04dQ%d", year_to, q_to),
      years_apart = year_to - year_from
    )
  })

  # Parameterised SQL query; product values are never concatenated into SQL.
  query_filtered <- function(table, date_col, from, to, products) {
    placeholders <- paste(rep("?", length(products)), collapse = ", ")
    sql <- paste0(
      "SELECT * FROM ", table,
      " WHERE ", date_col, " BETWEEN ? AND ?",
      " AND Bestedingscategorieen IN (", placeholders, ")",
      " ORDER BY ", date_col, ", Bestedingscategorieen"
    )
    DBI::dbGetQuery(con, sql, params = c(list(from, to), as.list(products)))
  }

  monthly_data <- shiny::reactive({
    p <- periods()
    query_filtered("monthly_cpi", "Perioden", p$month_from, p$month_to, selected_products()) %>%
      mutate(
        date = as.Date(sprintf("%s-%s-01", substr(Perioden, 1, 4), substr(Perioden, 7, 8))),
        product = label_product(Bestedingscategorieen),
        CPI_1 = round(CPI_1,2)
      )
  })

  quarterly_data <- shiny::reactive({
    p <- periods()
    query_filtered("quarterly_changes", "verslagperiode", p$quarter_from, p$quarter_to, selected_products()) %>%
      mutate(
        year = as.integer(substr(verslagperiode, 1, 4)),
        quarter = as.integer(sub(".*Q", "", verslagperiode)),
        date = as.Date(sprintf("%04d-%02d-01", year, (quarter - 1) * 3 + 1)),
        product = label_product(Bestedingscategorieen),
        quarterly_change_pct = round(quarterly_change_pct,2),
        current_cpi = round(current_cpi,2),
        previous_cpi = round(previous_cpi,2)
      )
  })

  output$n_products <- shiny::renderText({
    length(selected_products())
  })
  output$n_monthly <- shiny::renderText({
    format(nrow(monthly_data()), trim = TRUE)
  })
  output$n_quarterly <- shiny::renderText({
    format(nrow(quarterly_data()), trim = TRUE)
  })

  x_breaks <- shiny::reactive({
    n_years <- periods()$years_apart
    if (n_years >= 15) "5 years" else if (n_years >= 5) "2 years" else "1 year"
  })

  # Show only our readable Dutch hover text, never raw column names.
  to_interactive <- function(p) {
    plotly::ggplotly(p, tooltip = "text") %>%
      plotly::layout(
        showlegend = FALSE,
        hovermode = "closest",
        hoverlabel = list(
          bgcolor = "#FFFFFF",
          bordercolor = "#D8E2EA",
          font = list(family = "Arial", size = 13, color = "#263746")
        )
      ) %>%
      plotly::config(displaylogo = FALSE, responsive = TRUE)
  }

  output$monthly_chart <- plotly::renderPlotly({
    d <- monthly_data()
    shiny::validate(shiny::need(nrow(d) > 0, "Geen maandcijfers beschikbaar voor deze selectie."))

    d <- d %>%
      mutate(
        hover_text = paste0(
          "<b>", product, "</b>",
          "<br>Maand: ", maanden_nl[as.integer(substr(Perioden, 7, 8))],
          " ", substr(Perioden, 1, 4),
          "<br>Consumentenprijsindex: <b>", format_nl(CPI_1), "</b>"
        )
      )

    p <- ggplot(d, aes(x = date, y = CPI_1,
                       colour = product, text = hover_text)) +
      geom_hline(yintercept = 100, linetype = "dashed", colour = "#B9C1C8", linewidth = 0.4) +
      geom_point(size = 0.9, na.rm = TRUE) +
      facet_wrap(~product, ncol = 1, scales = "free_y") +
      scale_colour_manual(values = colours) +
      scale_x_date(date_breaks = x_breaks(), date_labels = "%Y") +
      labs(x = "Jaar", y = "Maandelijkse CPI") +
      theme_cbs()
    
    

    to_interactive(p)
  })

  output$quarterly_chart <- plotly::renderPlotly({
    d <- quarterly_data()
    shiny::validate(shiny::need(nrow(d) > 0, "Geen kwartaalcijfers beschikbaar voor deze selectie."))

    d <- d %>%
      mutate(
        hover_text = paste0(
          "<b>", product, "</b>",
          "<br>Kwartaal: Q", quarter, " ", year,
          "<br>Gemiddelde CPI: <b>", format_nl(current_cpi), "</b>",
          "<br>Vorig kwartaal: ", format_nl(previous_cpi)
        )
      )

    p <- ggplot(d, aes(x = date, y = current_cpi,
                       colour = product, text = hover_text)) +
      geom_hline(yintercept = 100, linetype = "dashed", colour = "#B9C1C8", linewidth = 0.4) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.2) +
      facet_wrap(~product, ncol = 1, scales = "free_y") +
      scale_colour_manual(values = colours) +
      scale_x_date(date_breaks = x_breaks(), date_labels = "%Y") +
      labs(x = "Jaar", y = "Gemiddelde CPI per kwartaal") +
      theme_cbs()

    to_interactive(p)
  })

  output$changes_chart <- plotly::renderPlotly({
    d <- quarterly_data()
    shiny::validate(shiny::need(nrow(d) > 0, "Geen kwartaalmutaties beschikbaar voor deze selectie."))

    d <- d %>%
      mutate(
        hover_text = paste0(
          "<b>", product, "</b>",
          "<br>Kwartaal: Q", quarter, " ", year,
          "<br>Kwartaalmutatie: <b>",
          ifelse(quarterly_change_pct > 0, "+", ""),
          format_nl(quarterly_change_pct, digits = 2L), "%</b>",
          "<br>CPI vorig kwartaal: ", format_nl(previous_cpi),
          "<br>CPI huidig kwartaal: ", format_nl(current_cpi)
        )
      )

    p <- ggplot(d, aes(x = date, y = quarterly_change_pct,
                       fill = product, text = hover_text)) +
      geom_hline(yintercept = 0, colour = "#334155", linewidth = 0.4) +
      geom_col(width = 75, alpha = 0.9) +
      facet_wrap(~product, ncol = 1, scales = "free_y") +
      scale_fill_manual(values = colours) +
      scale_x_date(date_breaks = x_breaks(), date_labels = "%Y") +
      labs(x = "Jaar", y = "Kwartaalmutatie (%)") +
      theme_cbs()

    to_interactive(p)
  })

  table_data <- shiny::reactive({
    if (identical(input$table_type, "monthly")) {
      monthly_data() %>%
        transmute(Product = product, Maand = Perioden, CPI = CPI_1)
    } else {
      quarterly_data() %>%
        transmute(
          Product = product,
          Kwartaal = verslagperiode,
          `CPI vorig kwartaal` = previous_cpi,
          `CPI huidig kwartaal` = current_cpi,
          `Kwartaalmutatie (%)` = quarterly_change_pct
        )
    }
  })

  output$data_table <- DT::renderDT({
    DT::datatable(
      table_data(),
      rownames = FALSE,
      options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", ordering = TRUE)
    )
  }, server = TRUE)

  output$download_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("cbs_", input$table_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      utils::write.csv2(table_data(), file, row.names = FALSE, na = "")
    }
  )
}

shiny::shinyApp(ui, server)
