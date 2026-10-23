# SQL database: SQLite-connectie en opslag van maand- en kwartaalresultaten.
# Originele functies uit R_rmd_CBS_casus(1).Rmd.

connect_cpi_db <- function(
    db_path = "data/cbs_cpi.sqlite") {

  # Create directory if needed
  dir.create(
    dirname(db_path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Connect to SQLite
  con <- dbConnect(SQLite(), db_path)

  # Create table if it does not exist
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS monthly_cpi (
      Bestedingscategorieen TEXT NOT NULL,
      Perioden TEXT NOT NULL,
      CPI_1 REAL,
      PRIMARY KEY (Bestedingscategorieen, Perioden)
    )
  ")

  return(con)
}


save_cpi_data <- function(
    cpi_data,
    con,
    table_name = "monthly_cpi",
    product_col = "Bestedingscategorieen",
    period_col = "Perioden",
    cpi_col = "CPI_1") {

  # Step 1: Define and validate column names
  key_cols <- c(product_col, period_col)
  required <- c(key_cols, cpi_col)

  if (length(required) != 3 ||
      anyNA(required) ||
      anyDuplicated(required) ||
      any(!nzchar(trimws(required)))) {
    stop("Invalid column definitions.")
  }

  if (length(table_name) != 1 ||
      is.na(table_name) ||
      !nzchar(trimws(table_name))) {
    stop("Invalid table name.")
  }

  # Step 2: Validate input data
  if (!is.data.frame(cpi_data) ||
      !all(required %in% names(cpi_data))) {
    stop("Missing required CPI columns.")
  }

  if (nrow(cpi_data) == 0) {
    return(invisible(0L))
  }

  data <- cpi_data[, required, drop = FALSE]

  data[[product_col]] <- as.character(data[[product_col]])
  data[[period_col]] <- as.character(data[[period_col]])

  if (anyNA(data[key_cols])) {
    stop("Missing product codes or reporting periods.")
  }

  if (any(!nzchar(trimws(data[[product_col]])))) {
    stop("Invalid product codes.")
  }

  if (any(!grepl(
    "^[0-9]{4}MM(0[1-9]|1[0-2])$",
    data[[period_col]]
  ))) {
    stop("Invalid monthly reporting periods.")
  }

  if (anyDuplicated(data[key_cols])) {
    stop("Duplicate product-period combinations.")
  }

  # Step 3: Validate CPI values
  original_cpi <- data[[cpi_col]]

  data[[cpi_col]] <- suppressWarnings(
    as.numeric(as.character(original_cpi))
  )

  if (any(!is.na(original_cpi) & is.na(data[[cpi_col]])) ||
      any(!is.na(data[[cpi_col]]) &
          (!is.finite(data[[cpi_col]]) |
           data[[cpi_col]] <= 0))) {
    stop("Invalid CPI values.")
  }

  # Step 4: Quote SQL identifiers
  quote_id <- function(x) {
    as.character(DBI::dbQuoteIdentifier(con, x))
  }

  table_sql <- quote_id(table_name)
  product_sql <- quote_id(product_col)
  period_sql <- quote_id(period_col)
  cpi_sql <- quote_id(cpi_col)

  # Step 5: Construct table definition
  create_sql <- paste0(
    "CREATE TABLE IF NOT EXISTS ", table_sql, " (",
    product_sql, " TEXT NOT NULL, ",
    period_sql, " TEXT NOT NULL, ",
    cpi_sql, " REAL, ",
    "PRIMARY KEY (",
    product_sql, ", ", period_sql,
    "))"
  )

  # Step 6: Construct dynamic UPSERT
  insert_sql <- paste0(
    "INSERT INTO ", table_sql,
    " (", paste(quote_id(required), collapse = ", "), ")",
    " VALUES (?, ?, ?)",
    " ON CONFLICT (",
    product_sql, ", ", period_sql,
    ") DO UPDATE SET ",
    cpi_sql, " = excluded.", cpi_sql
  )

  # Step 7: Create table and save observations
  DBI::dbWithTransaction(con, {

    DBI::dbExecute(con, create_sql)

    statement <- DBI::dbSendStatement(con, insert_sql)

    tryCatch(
      DBI::dbBind(
        statement,
        unname(as.list(data))
      ),
      finally = DBI::dbClearResult(statement)
    )
  })

  message("Processed ", nrow(data), " CPI observations.")

  invisible(nrow(data))
}


save_quarterly_results <- function(
    data,
    con,
    table_name = "quarterly_changes",
    period_col = "verslagperiode",
    product_col = "Bestedingscategorieen",
    numeric_cols = c(
      "previous_cpi",
      "current_cpi",
      "quarterly_change_pct"
    )) {

  # Step 1: Define columns and primary keys
  key_cols <- c(product_col, period_col)
  required <- c(key_cols, numeric_cols)

  if (length(numeric_cols) == 0 ||
      anyNA(required) ||
      anyDuplicated(required) ||
      any(!nzchar(required))) {
    stop("Invalid column definitions.")
  }

  # Step 2: Validate input
  if (!is.data.frame(data) ||
      !all(required %in% names(data))) {
    stop("Missing required columns.")
  }

  if (nrow(data) == 0) {
    stop("No quarterly results to save.")
  }

  data <- data[, required, drop = FALSE]

  data[[period_col]] <- as.character(data[[period_col]])
  data[[product_col]] <- as.character(data[[product_col]])

  if (anyNA(data)) {
    stop("Quarterly results contain missing values.")
  }

  if (any(!grepl(
    "^[0-9]{4}Q[1-4]$",
    data[[period_col]]
  ))) {
    stop("Invalid reporting period.")
  }

  if (any(!nzchar(trimws(data[[product_col]])))) {
    stop("Invalid product identifiers.")
  }

  if (anyDuplicated(data[key_cols])) {
    stop("Duplicate product-quarter combinations.")
  }

  valid_numeric <- vapply(
    data[numeric_cols],
    function(x) {
      is.numeric(x) && all(is.finite(x))
    },
    logical(1)
  )

  if (!all(valid_numeric)) {
    stop("Invalid numeric values.")
  }

  # Step 3: Quote SQL identifiers
  quote_id <- function(x) {
    as.character(DBI::dbQuoteIdentifier(con, x))
  }

  table_sql <- quote_id(table_name)
  key_sql <- quote_id(key_cols)
  numeric_sql <- quote_id(numeric_cols)
  columns_sql <- quote_id(required)

  # Step 4: Dynamically construct table definition
  definitions <- c(
    paste(key_sql, "TEXT NOT NULL"),
    paste(numeric_sql, "REAL NOT NULL"),
    paste0(
      "PRIMARY KEY (",
      paste(key_sql, collapse = ", "),
      ")"
    )
  )

  create_sql <- paste0(
    "CREATE TABLE IF NOT EXISTS ", table_sql,
    " (", paste(definitions, collapse = ", "), ")"
  )

  # Step 5: Dynamically construct UPSERT
  placeholders <- paste(
    rep("?", length(required)),
    collapse = ", "
  )

  updates <- paste(
    paste0(
      numeric_sql,
      " = excluded.",
      numeric_sql
    ),
    collapse = ", "
  )

  insert_sql <- paste0(
    "INSERT INTO ", table_sql,
    " (", paste(columns_sql, collapse = ", "), ")",
    " VALUES (", placeholders, ")",
    " ON CONFLICT (",
    paste(key_sql, collapse = ", "),
    ") DO UPDATE SET ",
    updates
  )

  # Step 6: Create table and save results
  DBI::dbWithTransaction(con, {

    DBI::dbExecute(con, create_sql)

    statement <- DBI::dbSendStatement(con, insert_sql)

    tryCatch(
      DBI::dbBind(
        statement,
        unname(as.list(data))
      ),
      finally = DBI::dbClearResult(statement)
    )
  })

  message("Saved ", nrow(data), " quarterly results.")

  invisible(nrow(data))
}
