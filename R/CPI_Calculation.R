# CPI Calculation: kwartaalgemiddelden en kwartaal-op-kwartaalmutaties.
# Originele berekeningsfunctie uit R_rmd_CBS_casus(1).Rmd.

calculate_quarterly_change <- function(
    con,
    verslagperiode = NULL,
    start_verslagperiode = NULL,
    eind_verslagperiode = NULL,
    table_name = "monthly_cpi",
    product_col = "Bestedingscategorieen",
    period_col = "Perioden",
    cpi_col = "CPI_1",
    output_period_col = "verslagperiode",
    output_product_col = product_col,
    previous_cpi_col = "previous_cpi",
    current_cpi_col = "current_cpi",
    change_col = "quarterly_change_pct") {

  # Step 1: Validate column names
  input_cols <- c(product_col, period_col, cpi_col)

  output_cols <- c(
    output_period_col,
    output_product_col,
    previous_cpi_col,
    current_cpi_col,
    change_col
  )

  valid_names <- function(x) {
    all(!is.na(x)) &&
      all(nzchar(trimws(x))) &&
      !anyDuplicated(x)
  }

  if (!valid_names(input_cols) ||
      !valid_names(output_cols) ||
      length(table_name) != 1 ||
      is.na(table_name) ||
      !nzchar(trimws(table_name))) {
    stop("Invalid table or column names.")
  }

  if (!DBI::dbExistsTable(con, table_name)) {
    stop("Source table does not exist.")
  }

  if (!all(input_cols %in% DBI::dbListFields(con, table_name))) {
    stop("Missing required source columns.")
  }

  # Step 2: Validate and convert reporting quarters
  quarter_id <- function(x) {

    if (length(x) != 1 || is.na(x) ||
        !grepl("^[0-9]{4}(Q|KW)[1-4]$", x)) {
      stop("Invalid quarter. Use YYYYQn or YYYYKWn.")
    }

    year <- as.integer(substr(x, 1, 4))
    quarter <- as.integer(substr(x, nchar(x), nchar(x)))

    year * 4L + quarter - 1L
  }

  # Step 3: Determine quarter range
  if (!is.null(verslagperiode)) {

    if (!is.null(start_verslagperiode) ||
        !is.null(eind_verslagperiode)) {
      stop("Use a single period OR a start/end period.")
    }

    from <- quarter_id(verslagperiode)
    to <- from

  } else {

    if (is.null(start_verslagperiode) ||
        is.null(eind_verslagperiode)) {
      stop("Provide both start and end reporting periods.")
    }

    from <- quarter_id(start_verslagperiode)
    to <- quarter_id(eind_verslagperiode)
  }

  if (from > to) {
    stop("Start quarter must not exceed end quarter.")
  }

  # Step 4: Convert quarters to monthly CBS periods
  first_month <- function(id) {
    sprintf(
      "%04dMM%02d",
      id %/% 4L,
      (id %% 4L) * 3L + 1L
    )
  }

  last_month <- function(id) {
    sprintf(
      "%04dMM%02d",
      id %/% 4L,
      (id %% 4L) * 3L + 3L
    )
  }

  start_month <- first_month(from - 1L)
  end_month <- last_month(to)

  # Step 5: Construct dynamic SQL queries
  quote_id <- function(x) {
    as.character(DBI::dbQuoteIdentifier(con, x))
  }

  table_sql <- quote_id(table_name)
  product_sql <- quote_id(product_col)
  period_sql <- quote_id(period_col)
  cpi_sql <- quote_id(cpi_col)

  monthly_sql <- paste0(
    "SELECT ",
    product_sql, " AS product, ",
    period_sql, " AS period, ",
    cpi_sql, " AS cpi ",
    "FROM ", table_sql, " ",
    "WHERE ", period_sql, " BETWEEN ? AND ?"
  )

  products_sql <- paste0(
    "SELECT DISTINCT ",
    product_sql, " AS product ",
    "FROM ", table_sql
  )

  # Step 6: Retrieve required monthly observations
  monthly_data <- DBI::dbGetQuery(
    con,
    monthly_sql,
    params = list(start_month, end_month)
  )

  products <- DBI::dbGetQuery(
    con,
    products_sql
  )$product

  if (length(products) == 0) {
    stop("The source table is empty.")
  }

  if (!is.numeric(monthly_data$cpi)) {
    stop("The CPI column must contain numeric values.")
  }

  # Step 7: Calculate quarterly averages
  quarterly_data <- monthly_data %>%
    dplyr::mutate(
      year = as.integer(substr(period, 1, 4)),
      month = as.integer(substr(period, 7, 8)),
      quarter_id = year * 4L + (month - 1L) %/% 3L
    ) %>%
    dplyr::group_by(product, quarter_id) %>%
    dplyr::summarise(
      n_months = dplyr::n(),
      n_valid = sum(is.finite(cpi)),
      quarterly_cpi = mean(cpi),
      .groups = "drop"
    )

  # Step 8: Validate completeness
  expected_n <- to - from + 2L

  validation <- quarterly_data %>%
    dplyr::group_by(product) %>%
    dplyr::summarise(
      n_quarters = dplyr::n(),
      first_quarter = min(quarter_id),
      last_quarter = max(quarter_id),
      complete = all(
        n_months == 3L &
        n_valid == 3L &
        is.finite(quarterly_cpi) &
        quarterly_cpi > 0
      ),
      .groups = "drop"
    )

  if (
    !setequal(validation$product, products) ||
    any(
      validation$n_quarters != expected_n |
      validation$first_quarter != (from - 1L) |
      validation$last_quarter != to |
      !validation$complete
    )
  ) {
    stop("Incomplete or invalid quarterly CPI data.")
  }

  # Step 9: Calculate quarterly percentage changes
  result <- quarterly_data %>%
    dplyr::arrange(product, quarter_id) %>%
    dplyr::group_by(product) %>%
    dplyr::mutate(
      previous_cpi = dplyr::lag(quarterly_cpi),
      quarterly_change_pct = (
        quarterly_cpi / previous_cpi - 1
      ) * 100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      quarter_id >= from,
      quarter_id <= to
    ) %>%
    dplyr::transmute(
      verslagperiode = paste0(
        quarter_id %/% 4,
        "Q",
        quarter_id %% 4 + 1
      ),
      product,
      previous_cpi,
      current_cpi = quarterly_cpi,
      quarterly_change_pct
    )

  # Step 10: Apply requested output column names
  names(result) <- output_cols

  return(result)
}
