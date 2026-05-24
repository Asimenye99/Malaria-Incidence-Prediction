# =============================================================================
# Malaria Incidence Data Cleaning Script
# Input : Incidence_2015_2025.xlsx  (one sheet per year, 2015–2025)
# Output: malaria_incidence_long.csv  (District | Month | Year | Cases)
# =============================================================================

# ── 1. Install / load packages ────────────────────────────────────────────────
if (!requireNamespace("readxl",  quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr",   quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr",   quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("purrr",   quietly = TRUE)) install.packages("purrr")

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ── 2. File path – change this to match your local path ──────────────────────
file_path <- "Incidence_2015_2025.xlsx"   # place the file in your working dir,
                                           # or supply the full path, e.g.:
                                           # file_path <- "C:/Users/You/Documents/Incidence_2015_2025.xlsx"

# ── 3. Identify the year sheets (skip "Sheet1" summary tab) ──────────────────
all_sheets  <- excel_sheets(file_path)
year_sheets <- all_sheets[grepl("Mal_Cases", all_sheets, ignore.case = TRUE)]
cat("Sheets to process:", paste(year_sheets, collapse = ", "), "\n")

# ── 4. Function to clean a single year sheet ─────────────────────────────────
clean_sheet <- function(sheet_name) {

  # --- 4a. Read raw data (no column-name coercion) ---------------------------
  raw <- read_excel(
    path      = file_path,
    sheet     = sheet_name,
    col_names = FALSE,   # we handle headers ourselves
    col_types = "text"   # read everything as text first; avoids type guessing
  )

  # --- 4b. Drop the header row and empty / grand-total rows ------------------
  # Row 1 is always the column-header row; drop it
  raw <- raw[-1, ]

  # Keep only rows that have a value in column 2 (the Period column)
  # This removes blank rows, subtotal rows ("Balaka-DHO Total"), and
  # the Grand Total row at the bottom
  raw <- raw[!is.na(raw[[2]]), ]

  # Period column contains strings like "January 2015" – keep only those
  raw <- raw[grepl("^[A-Za-z]+ \\d{4}$", raw[[2]]), ]

  # --- 4c. Rename the three columns we need ----------------------------------
  names(raw)[1:3] <- c("District_raw", "Period", "Cases")

  # --- 4d. Forward-fill the District column ----------------------------------
  # In the source file the district name only appears on the first month row;
  # subsequent months for the same district have NA in that column.
  raw <- raw %>%
    mutate(District_raw = na_if(District_raw, "")) %>%  # "" → NA if present
    fill(District_raw, .direction = "down")

  # --- 4e. Parse Month and Year out of the Period string ---------------------
  raw <- raw %>%
    mutate(
      Month = str_extract(Period, "^[A-Za-z]+"),
      Year  = as.integer(str_extract(Period, "\\d{4}"))
    )

  # --- 4f. Clean Cases: remove commas, spaces, trailing dots, coerce numeric -
  raw <- raw %>%
    mutate(
      Cases = str_replace_all(Cases, "[,\\s]", ""),  # strip commas & spaces
      Cases = str_replace(Cases, "\\.$", ""),         # strip trailing dot
      Cases = as.numeric(Cases)
    )

  # --- 4g. Remove any rows with NA cases (formulas, etc.) --------------------
  raw <- raw %>% filter(!is.na(Cases))

  # --- 4h. Select and rename final columns -----------------------------------
  raw %>%
    select(
      District = District_raw,
      Month,
      Year,
      Cases
    )
}

# ── 5. Apply the function across all year sheets and stack the results ────────
malaria_long <- map_dfr(year_sheets, clean_sheet)

# ── 6. Final tidying ──────────────────────────────────────────────────────────
# Order months correctly (factor)
month_order <- c("January","February","March","April","May","June",
                 "July","August","September","October","November","December")

malaria_long <- malaria_long %>%
  mutate(
    Month = factor(Month, levels = month_order, ordered = TRUE)
  ) %>%
  arrange(Year, Month, District)

# ── 7. Inspect the result ─────────────────────────────────────────────────────
cat("\nDimensions:", nrow(malaria_long), "rows x", ncol(malaria_long), "cols\n")
cat("Years covered:", paste(sort(unique(malaria_long$Year)), collapse = ", "), "\n")
cat("Districts (n =", n_distinct(malaria_long$District), "):\n")
print(sort(unique(malaria_long$District)))
cat("\nFirst 10 rows:\n")
print(head(malaria_long, 10))

# ── 8. Export to CSV ──────────────────────────────────────────────────────────
output_file <- "malaria_incidence_long.csv"
write.csv(malaria_long, output_file, row.names = TRUE)
cat("\nSaved to:", output_file, "\n")
