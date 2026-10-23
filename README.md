# CBS StatLine | CPI-kwartaalmutaties

Een kleine, reproduceerbare **R-pipeline** die maandelijkse consumentenprijsindexcijfers (CPI) rechtstreeks uit **CBS StatLine** ophaalt, daaruit kwartaalmutaties berekent en de resultaten opslaat in **SQLite** en als **grafieken**.

Het project analyseert twee bestedingscategorieën: **chocolade** (`CPI011830`) en **elektriciteit** (`CPI045100`), voor de periode **1996–2025**.

**Taal / Language:** [Nederlands](#nederlands) · [English](#english)

---

<a id="nederlands"></a>
## 🇳🇱 Nederlands

### 1. Snel starten

**Benodigd:** R-packages: `httr2`, `magrittr`, `dplyr`, `DBI`, `RSQLite`, `ggplot2` en `scales`.

**Stap 1 — Installeer de packages** :

```r
install.packages(c(
  "httr2", "magrittr", "dplyr", "DBI",
  "RSQLite", "ggplot2", "scales"
))
```

**Stap 2 — Voer de pipeline uit** vanuit de hoofdmap van het project:

```bash
Rscript run.R
```

**Stap 3 — Bekijk de resultaten:**

- `data/cbs_cpi.sqlite` — lokale database met maandcijfers en kwartaalmutaties.
- `output/figures/` — drie PNG-grafieken.

> Er zijn **geen externe CSV- of Excel-bestanden** nodig. De input wordt rechtstreeks via de CBS API opgehaald; de database en figuren worden lokaal aangemaakt.

### 2. Projectstructuur

```text
CPI-Casus-CBS-Ravish-Gangapersad/
├── Casus/
│   └── Casus data engineer.pdf             # Oorspronkelijke opdracht
├── Handleiding CBS/
│   └── 2014-handleiding-cbs-open-data-services.pdf
├── R Shiny app/
│   └── CBS_casus_R_shiny.R                # Interactief Shiny-dashboard
├── R/
│   ├── StatlineAPI.R                      # CBS API: metadata en maandelijkse CPI
│   ├── CPI_Calculation.R                  # Kwartaalgemiddelden en -mutaties
│   ├── SQL_database.R                     # SQLite: verbinden, opslaan en bijwerken
│   └── CPI_plots.R                        # Grafieken genereren en exporteren
├── data/
│   └── cbs_cpi.sqlite                     # Lokale SQLite-database
├── output/
│   └── figures/
│       ├── monthly_cpi.png
│       ├── quarterly_cpi.png
│       └── quarterly_change.png
├── run.R                                  # Start de volledige pipeline
└── README.md                              # Documentatie
```

`run.R` is het **enige script dat je hoeft te starten**. De vier bestanden in `R/` bevatten de afzonderlijke functies.

### 3. Hoe werkt de pipeline?

**INPUT → THROUGHPUT → OUTPUT**

| Fase | Wat gebeurt er? | Belangrijkste bestand |
| --- | --- | --- |
| **Input** | Maandelijkse CPI-cijfers en productmetadata ophalen via de CBS StatLine OData API. API-pagina's worden direct in SQLite opgeslagen. | `StatlineAPI.R` |
| **Throughput** | Per product het gemiddelde van de drie maandcijfers per kwartaal berekenen; vervolgens de procentuele verandering ten opzichte van het vorige kwartaal bepalen. | `CPI_Calculation.R` |
| **Output** | Maand- en kwartaalresultaten bewaren in SQLite en drie figuren genereren. | `SQL_database.R`, `CPI_plots.R` |

**Bron:** [CBS StatLine, tabel 83131NED](https://opendata.cbs.nl/statline/#/CBS/nl/dataset/83131NED/table), CPI met **2015 = 100**.

#### Berekening van de kwartaalmutatie

Eerst wordt per product het **rekenkundig gemiddelde van drie maandelijkse CPI-cijfers** berekend. Daarna geldt:

```text
Kwartaalmutatie (%) = 100 × (gemiddelde CPI huidig kwartaal
                          / gemiddelde CPI vorig kwartaal − 1)
```

De functie `calculate_quarterly_change()` accepteert:

- één verslagperiode, bijvoorbeeld `verslagperiode = "2024Q2"`; of
- een begin- en eindperiode, bijvoorbeeld `start_verslagperiode = "1996Q2"` en `eind_verslagperiode = "2025Q4"`.

De eerste berekende mutatie is **1996Q2**, omdat voor een kwartaal-op-kwartaalvergelijking ook de cijfers van **1996Q1** nodig zijn. De functie controleert of elk benodigd kwartaal drie geldige maandcijfers heeft.

### 4. Output

**SQLite-database:** `data/cbs_cpi.sqlite`

| Tabel | Inhoud |
| --- | --- |
| `monthly_cpi` | Opgehaalde maandcijfers per product en verslagmaand. |
| `quarterly_changes` | Gemiddelde CPI van het huidige en vorige kwartaal en de berekende procentuele mutatie. |

De opslagfuncties gebruiken **UPSERT**. Daardoor kunnen bestaande product-periodecombinaties bij opnieuw uitvoeren worden bijgewerkt zonder dubbele records te creëren.

**Grafieken:** `output/figures/`

| Bestand | Wat zie je? |
| --- | --- |
| `monthly_cpi.png` | Ontwikkeling van de maandelijkse CPI (2015 = 100). |
| `quarterly_cpi.png` | Gemiddelde CPI per kwartaal, berekend uit maandcijfers. |
| `quarterly_change.png` | Procentuele kwartaal-op-kwartaalmutatie. |


### 5. Aanpassen en reproduceren

- **Producten of periode wijzigen?** Pas `product_codes`, `start_year` en `end_year` aan in `run.R`. Pas bij een gewijzigde analyseperiode ook de verslagperioden voor de kwartaalberekening aan. De grafiekfunctie bevat daarnaast een vaste periode-afbakening, die je zo nodig aanpast in `R/CPI_plots.R`.
- **Alleen een kwartaal berekenen?** Gebruik `calculate_quarterly_change(con = con, verslagperiode = "2024Q2")` nadat de database is gevuld en `con` een open SQLite-verbinding is.
- **Opnieuw uitvoeren?** Start weer `Rscript run.R`. De database wordt bijgewerkt en de PNG-bestanden worden opnieuw opgeslagen.

**Praktische aandachtspunten:**

- Start vanuit de **projectmap**. Anders worden de relatieve paden naar de R-scripts niet gevonden.
- Ontbrekende of ongeldige maandcijfers kunnen ervoor zorgen dat de controle op volledige kwartalen stopt met een foutmelding.

### 6. Interactieve R Shiny-prijsmonitor (extra)

Naast de pipeline is een **R Shiny-dashboard** ontwikkeld om de CPI-resultaten interactief te verkennen. Het dashboard (`CBS_casus_R_shiny.R`) gebruikt de eerder aangemaakte **lokale SQLite-database** als databron; de CBS API hoeft hiervoor niet opnieuw te worden aangeroepen.

Met de prijsmonitor kun je:

- **Chocolade, elektriciteit of beide producten** selecteren.
- Een **begin- en eindkwartaal** kiezen, of snel de hele periode, de laatste 4 kwartalen, 5 jaar of 10 jaar bekijken.
- De **maandelijkse CPI, gemiddelde kwartaal-CPI en kwartaalmutatie** in interactieve Plotly-grafieken bekijken, inclusief Nederlandstalige informatie bij aanwijzen.
- De onderliggende maand- of kwartaalgegevens in een **doorzoekbare tabel** bekijken en de geselecteerde gegevens als CSV downloaden.

**Dashboard starten:** installeer eerst de aanvullende packages:

```r
install.packages(c("shiny", "bslib", "plotly", "DT"))
```

Voer eerst de reguliere pipeline uit (`Rscript run.R`), zodat `data/cbs_cpi.sqlite` bestaat. Start daarna **vanuit de projecthoofdmap** in R of RStudio:

```r
app <- source("R Shiny app/CBS_casus_R_shiny.R", local = TRUE)$value
shiny::runApp(app)
```

> De dashboard is ook te vinden via https://ravis.shinyapps.io/Shiny_CBS_CPI_Casus/

---

<a id="english"></a>
## 🇬🇧 English

### 1. Quick start

**Requirements:** R packages: `httr2`, `magrittr`, `dplyr`, `DBI`, `RSQLite`, `ggplot2`, and `scales`.

**Step 1 — Install the packages** :

```r
install.packages(c(
  "httr2", "magrittr", "dplyr", "DBI",
  "RSQLite", "ggplot2", "scales"
))
```

**Step 2 — Run the pipeline** from the project's root directory:

```bash
Rscript run.R
```

Alternatively, open `run.R` in RStudio and run it with the **project root as your working directory**.

**Step 3 — Explore the outputs:**

- `data/cbs_cpi.sqlite` — a local database with monthly CPI observations and quarterly changes.
- `output/figures/` — three PNG charts.

> **No external CSV or Excel files** are required. Input data are retrieved directly from the CBS API; the database and charts are generated locally.

### 2. Project structure

```text
CPI-Casus-CBS-Ravish-Gangapersad/
├── Casus/
│   └── Casus data engineer.pdf             # Original assignment
├── Handleiding CBS/
│   └── 2014-handleiding-cbs-open-data-services.pdf
├── R Shiny app/
│   └── CBS_casus_R_shiny.R                # Interactive Shiny dashboard
├── R/
│   ├── StatlineAPI.R                      # CBS API: metadata and monthly CPI
│   ├── CPI_Calculation.R                  # Quarterly averages and changes
│   ├── SQL_database.R                     # SQLite: connect, save, and update
│   └── CPI_plots.R                        # Generate and export charts
├── data/
│   └── cbs_cpi.sqlite                     # Local SQLite database
├── output/
│   └── figures/
│       ├── monthly_cpi.png
│       ├── quarterly_cpi.png
│       └── quarterly_change.png
├── run.R                                  # Runs the complete pipeline
└── README.md                              # Documentation
```

`run.R` is the **only script you need to execute**. The four files in `R/` contain the individual functions.

### 3. How does the pipeline work?

**INPUT → PROCESSING → OUTPUT**

| Stage | What happens? | Main file |
| --- | --- | --- |
| **Input** | Retrieve monthly CPI observations and product metadata through the CBS StatLine OData API. API pages are written directly to SQLite. | `StatlineAPI.R` |
| **Processing** | Calculate the mean of three monthly CPI values for each product and quarter, then calculate the percentage change from the previous quarter. | `CPI_Calculation.R` |
| **Output** | Store monthly and quarterly results in SQLite and generate three charts. | `SQL_database.R`, `CPI_plots.R` |

**Data source:** [CBS StatLine, table 83131NED](https://opendata.cbs.nl/statline/#/CBS/nl/dataset/83131NED/table), CPI with **2015 = 100**. The analysis is deliberately limited to **1996–2025**.

#### Quarterly change calculation

For each product, the **arithmetic mean of three monthly CPI observations** is calculated first. The quarterly change is then:

```text
Quarterly change (%) = 100 × (average CPI in current quarter
                           / average CPI in previous quarter − 1)
```

The `calculate_quarterly_change()` function accepts:

- a single reporting period, e.g. `verslagperiode = "2024Q2"`; or
- a start and end period, e.g. `start_verslagperiode = "1996Q2"` and `eind_verslagperiode = "2025Q4"`.

The first calculated change is **1996Q2**, since a quarter-on-quarter comparison requires data from **1996Q1**. The function validates that each required quarter contains three valid monthly observations.

### 4. Outputs

**SQLite database:** `data/cbs_cpi.sqlite`

| Table | Contents |
| --- | --- |
| `monthly_cpi` | Retrieved monthly observations by product and reporting month. |
| `quarterly_changes` | Average CPI for the current and previous quarters, plus the calculated percentage change. |

Database writes use **UPSERT**, allowing existing product-period records to be updated when the pipeline is rerun without creating duplicates.

**Charts:** `output/figures/`

| File | Description |
| --- | --- |
| `monthly_cpi.png` | Monthly CPI trends (2015 = 100). |
| `quarterly_cpi.png` | Quarterly mean CPI calculated from monthly observations. |
| `quarterly_change.png` | Quarter-on-quarter percentage changes. |


### 5. Customization and reproducibility

- **Change products or dates:** Edit `product_codes`, `start_year`, and `end_year` in `run.R`. If you change the analysis window, also update the quarter range passed to the calculation function. The plotting function uses a fixed date range, which can be updated in `R/CPI_plots.R`.
- **Calculate an individual quarter:** Call `calculate_quarterly_change(con = con, verslagperiode = "2024Q2")` after the database has been populated and `con` is an open SQLite connection.
- **Run again:** Execute `Rscript run.R` again. Database records are updated and the PNG charts are regenerated.

**Troubleshooting:**

- Run the script from the **project root** so relative paths to the R files resolve correctly.
- Missing or invalid monthly data may cause the quarterly completeness check to stop with an error.

### 6. Interactive R Shiny price monitor (optional)

An **R Shiny dashboard** is also included to explore the CPI results interactively. The dashboard (`CBS_casus_R_shiny.R`) reads from the **local SQLite database** generated by the pipeline; it does not need to retrieve the CBS API data again.

The dashboard lets you:

- Select **chocolate, electricity, or both products**.
- Choose a **start and end quarter**, or use quick selections for the full period, last 4 quarters, 5 years, or 10 years.
- Explore **monthly CPI, average quarterly CPI, and quarter-on-quarter changes** in interactive Plotly charts with informative Dutch hover labels.
- View the underlying monthly or quarterly observations in a **searchable table** and download the selected data as CSV.

**Run the dashboard:** first install the additional R packages:

```r
install.packages(c("shiny", "bslib", "plotly", "DT"))
```

Run the standard pipeline first (`Rscript run.R`) to create `data/cbs_cpi.sqlite`. Then, **from the project root**, run the following in R or RStudio:

```r
app <- source("R Shiny app/CBS_casus_R_shiny.R", local = TRUE)$value
shiny::runApp(app)
```

> The dashboard is an **optional visualization layer** on top of the existing pipeline. It can also be found published on https://ravis.shinyapps.io/Shiny_CBS_CPI_Casus/
