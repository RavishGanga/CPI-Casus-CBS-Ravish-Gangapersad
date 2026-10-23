# CBS CPI-casus: voer dit script uit vanuit de projectmap met:
# Rscript run.R
#
# Input      : CBS StatLine OData (zonder CSV/Excel-bestanden)
# Throughput : kwartaalgemiddelden en kwartaalmutaties
# Output     : SQLite-database en drie PNG-grafieken

required_packages <- c(
  "httr2", "magrittr", "dplyr", "DBI", "RSQLite", "ggplot2", "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Installeer eerst deze packages: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

# Start vanuit de hoofdmap van het project.
suppressPackageStartupMessages({
  library(httr2)
  library(magrittr)
  library(dplyr)
  library(DBI)
  library(RSQLite)
  library(ggplot2)
})

source(file.path("R", "StatlineAPI.R"))
source(file.path("R", "CPI_Calculation.R"))
source(file.path("R", "SQL_database.R"))
source(file.path("R", "CPI_plots.R"))

# INPUT: controleer de API en zoek productcategorieën.
base_url <- "https://opendata.cbs.nl/ODataApi/OData/83131NED"

cpi_sample <- get_cbs_data(endpoint = "TypedDataSet", top = 1)
print(names(cpi_sample))

data_properties <- get_cbs_data(endpoint = "DataProperties")

categories <- get_cbs_data(endpoint = "Bestedingscategorieen")
print(head(categories))

sel_categories <- select_categories(
  categories = categories,
  products = c("chocolade", "elektriciteit")
)
print(sel_categories)

#Twee productcodes 
product_codes <- c("CPI011830", "CPI045100")

con <- connect_cpi_db()

tryCatch({
  # INPUT: maandgegevens via de API direct naar SQLite streamen.
  get_cpi_data(
    product_codes = product_codes,
    start_year = 1996,
    end_year = 2025,
    on_page = function(page) {
      save_cpi_data(page, con)
    }
  )

  print(dbGetQuery(con, "
    SELECT
      Bestedingscategorieen,
      COUNT(*) AS observations,
      MIN(Perioden) AS first_period,
      MAX(Perioden) AS last_period
    FROM monthly_cpi
    GROUP BY Bestedingscategorieen
  "))

  # THROUGHPUT: kwartaalmutatie op basis van drie maandcijfers.
  quarterly_results <- calculate_quarterly_change(
    start_verslagperiode = "1996Q2",
    eind_verslagperiode = "2025Q4",
    con = con
  )

  # OUTPUT: kwartaaluitkomsten in de lokale database bewaren.
  save_quarterly_results(
    data = quarterly_results,
    con = con
  )

  # OUTPUT: drie figuren genereren en opslaan als PNG.
  make_cpi_plots(con, output_dir = "output/figures")

}, finally = {
  dbDisconnect(con)
})

message("Gereed: data/cbs_cpi.sqlite en output/figures/*.png")
