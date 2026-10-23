# CBS StatLine API: ophalen van metadata en maandelijkse CPI-data.
# Originele functies uit R_rmd_CBS_casus(1).Rmd.

get_cbs_data <- function(endpoint,base_url =  "https://opendata.cbs.nl/ODataApi/OData/83131NED" ,
                         top = NULL) {

  # Step 1: Create and configure API request
  req <- request(
    paste0(base_url, "/", endpoint)
  ) %>%
    req_timeout(30) %>%
    req_retry(max_tries = 3)

  # Step 2: Optionally limit observations
  if (!is.null(top)) {
    req <- req %>%
      req_url_query(`$top` = top)
  }

  # Step 3: Execute request
  response <- req %>%
    req_perform()

  # Step 4: Convert JSON response
  result <- response %>%
    resp_body_json(simplifyVector = TRUE)

  # Step 5: Check response
  if (is.null(result$value)) {
    stop("No data returned by CBS.")
  }

  # Step 6: Return data frame
  as.data.frame(result$value)
}


select_categories <- function(categories, products) {

  selected_categories <- categories %>%
    filter(
      grepl(
        paste(products, collapse = "|"),
        Title,
        ignore.case = TRUE
      )
    ) %>%
    select(Key, Title)

  return(selected_categories)
}


get_cpi_data <- function(
    product_codes,
    period = NULL,
    start_period = NULL,
    end_period = NULL,
    start_year = NULL,
    start_month = NULL,
    end_year = NULL,
    end_month = NULL,
    on_page = NULL,
    selected_columns = c("Bestedingscategorieen", "Perioden", "CPI_1"),
    base_url = "https://opendata.cbs.nl/ODataApi/OData/83131NED") {

  # PERIOD PRIORITY:
  # 1. period overrides all other date inputs.
  # 2. start_period/end_period override corresponding year/month inputs.
  # 3. Years define the range; months refine it.
  # 4. NULL means unused; default months are January/December.

  # Step 1: Convert month names or numbers
  parse_month <- function(x, default) {

    if (is.null(x)) return(default)

    lookup <- c(
      setNames(1:12, tolower(month.name)),
      setNames(1:12, tolower(month.abb)),
      setNames(1:12, c(
        "januari", "februari", "maart", "april",
        "mei", "juni", "juli", "augustus",
        "september", "oktober", "november", "december"
      ))
    )

    x <- tolower(trimws(as.character(x)))

    if (length(x) != 1 || is.na(x)) {
      stop("Provide one valid month.")
    }

    month <- if (grepl("^[0-9]{1,2}$", x)) {
      as.integer(x)
    } else {
      unname(lookup[x])
    }

    if (is.na(month) || month < 1 || month > 12) {
      stop("Invalid month: ", x)
    }

    as.integer(month)
  }

  # Step 2: Convert year/month to CBS period
  make_period <- function(year, month) {

    if (length(year) != 1 || is.na(year) ||
        !grepl("^[0-9]{4}$", as.character(year))) {
      stop("Year must contain four digits.")
    }

    sprintf("%04dMM%02d", as.integer(year), month)
  }

  # Step 3: Validate CBS period codes
  validate_period <- function(x) {

    if (length(x) != 1 || is.na(x) ||
        !grepl("^[0-9]{4}MM(0[1-9]|1[0-2])$", x)) {
      stop("Invalid period. Use YYYYMMmm, e.g. 2022MM01.")
    }

    x
  }

  # Step 4: Determine start and end periods
  if (!is.null(period)) {

    from <- validate_period(period)
    to <- from

  } else {

    if (!is.null(start_period)) {
      from <- validate_period(start_period)
    } else {

      if (is.null(start_year)) {
        stop("Provide a period or start_year.")
      }

      from <- make_period(
        start_year,
        parse_month(start_month, 1)
      )
    }

    if (!is.null(end_period)) {

      to <- validate_period(end_period)

    } else if (!is.null(end_year)) {

      to <- make_period(
        end_year,
        parse_month(end_month, 12)
      )

    } else if (!is.null(end_month)) {

      to <- make_period(
        substr(from, 1, 4),
        parse_month(end_month, 12)
      )

    } else if (is.null(start_period) &&
               is.null(start_month)) {

      to <- make_period(start_year, 12)

    } else {

      to <- from
    }
  }

  if (from > to) {
    stop("Start period must not exceed end period.")
  }

  # Step 5: Generate all monthly CBS period codes
  from_date <- as.Date(paste0(
    substr(from, 1, 4), "-",
    substr(from, 7, 8), "-01"
  ))

  to_date <- as.Date(paste0(
    substr(to, 1, 4), "-",
    substr(to, 7, 8), "-01"
  ))

  period_codes <- seq(
    from_date, to_date, by = "month"
  ) %>%
    format("%YMM%m")

  # Split into yearly batches to keep API requests small
  period_batches <- split(
    period_codes,
    substr(period_codes, 1, 4)
  )

  # Step 6: Construct product filter
  if (length(product_codes) == 0 || anyNA(product_codes)) {
    stop("Provide at least one product code.")
  }

  product_filter <- paste0(
    "Bestedingscategorieen eq '",
    gsub("'", "''", product_codes, fixed = TRUE),
    "'"
  ) %>%
    paste(collapse = " or ")

 
  # Step 7: Retrieve data with server-side filtering
  if (!is.null(on_page) && !is.function(on_page)) {
    stop("on_page must be a function or NULL.")
  }

  pages <- list()
  total_rows <- 0L

  for (year in names(period_batches)) {

    months_this_year <- period_batches[[year]]

    # Full year: use startswith()
    # Partial year: use exact monthly periods
    if (length(months_this_year) == 12L) {

      period_filter <- paste0(
        "startswith(Perioden,'", year, "MM')"
      )

    } else {

      period_filter <- paste0(
        "Perioden eq '", months_this_year, "'"
      ) %>%
        paste(collapse = " or ")
    }

    cpi_filter <- paste0(
      "(", product_filter, ") and (",
      period_filter, ")"
    )

    req <- request(
      paste0(base_url, "/TypedDataSet")
    ) %>%
      req_url_query(`$filter` = cpi_filter) %>%
      req_timeout(30) %>%
      req_retry(max_tries = 3)

    if (!is.null(selected_columns)) {
      req <- req %>%
        req_url_query(
          `$select` = paste(selected_columns, collapse = ",")
        )
    }

    # Step 8: Follow pagination links
    repeat {

      result <- req %>%
        req_perform() %>%
        resp_body_json(simplifyVector = TRUE)

      if (is.null(result$value)) {
        stop("Invalid CBS response for ", year)
      }

      page <- as.data.frame(result$value)

      if (nrow(page) > 0) {

        if (is.null(on_page)) {
          # Small data: collect in memory
          pages[[length(pages) + 1L]] <- page
        } else {
          # Large data: process immediately
          on_page(page)
        }

        total_rows <- total_rows + nrow(page)
      }

      next_url <- result[["odata.nextLink"]]

      if (is.null(next_url)) {
        next_url <- result[["@odata.nextLink"]]
      }

      if (is.null(next_url) || !nzchar(next_url)) {
        break
      }

      req <- request(next_url) %>%
        req_timeout(30) %>%
        req_retry(max_tries = 3)
    }

    message("Completed ", year)
  }

  # Step 9: Return results
  message("Total observations processed: ", total_rows)

  if (!is.null(on_page)) {
    return(invisible(total_rows))
  }

  if (length(pages) == 0L) {
    return(data.frame())
  }

  return(dplyr::bind_rows(pages))
}
