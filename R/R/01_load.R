# 01_load.R - load raw data files and first look
library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(here)
# ---- Load CSV files ----

retailers <- read_csv(here("data", "raw", "retailers.csv"))

complaints <- read_csv(
  here("data", "raw", "complaints_records.csv"),
  col_types = cols(postcode = col_character(),
                   date_received = col_character())
)

disconnections <- read_csv(
  here("data", "raw", "disconnections_records.csv"),
  col_types = cols(postcode = col_character())
)

postcodes <- read_csv(
  here("data", "raw", "postcode_demographics.csv"),
  col_types = cols(postcode = col_character())
)

trial <- read_csv(here("data", "raw", "outreach_trial.csv"))
# ---- Excel submissions: explore structure ----

# Sheet names in the FY2021 workbook
excel_sheets(here("data", "raw", "retail_submissions_FY2021.xlsx"))

# Read the first sheet exactly as it is
fy21_raw <- read_excel(
  here("data", "raw", "retail_submissions_FY2021.xlsx"),
  sheet = 1,
  col_names = FALSE
)

View(fy21_raw)
# Sheet names in the FY2024 workbook
excel_sheets(here("data", "raw", "retail_submissions_FY2024.xlsx"))

# Read the first sheet exactly as it is
fy24_raw <- read_excel(
  here("data", "raw", "retail_submissions_FY2024.xlsx"),
  sheet = 1,
  col_names = FALSE
)

View(fy24_raw)
# ---- Load Excel submissions ----

# Test: read one sheet properly
fy21_q1 <- read_excel(
  here("data", "raw", "retail_submissions_FY2021.xlsx"),
  sheet = 1,
  skip = 4
)

View(fy21_q1)
# FY2021: read all four sheets and combine
fy21_path <- here("data", "raw", "retail_submissions_FY2021.xlsx")

fy21_sheets <- excel_sheets(fy21_path)
fy21_sheets

fy21 <- map(fy21_sheets, function(s) {
  read_excel(fy21_path, sheet = s, skip = 4, col_types = "text") |>
    mutate(sheet = s)
}) |>
  list_rbind()

View(fy21)

# Function: read all sheets of one workbook
read_workbook <- function(path, skip_rows) {
  sheets <- excel_sheets(path)
  map(sheets, function(s) {
    read_excel(path, sheet = s, skip = skip_rows, col_types = "text") |>
      mutate(sheet = s, file = basename(path))
  }) |>
    list_rbind()
}

# Old template: FY2021-FY2023
old_files <- here("data", "raw",
                  c("retail_submissions_FY2021.xlsx",
                    "retail_submissions_FY2022.xlsx",
                    "retail_submissions_FY2023.xlsx"))

subs_old <- map(old_files, read_workbook, skip_rows = 4) |> list_rbind()

# New template: FY2024-FY2026
new_files <- here("data", "raw",
                  c("retail_submissions_FY2024.xlsx",
                    "retail_submissions_FY2025.xlsx",
                    "retail_submissions_FY2026.xlsx"))

subs_new <- map(new_files, read_workbook, skip_rows = 5) |> list_rbind()

# Standard column names (same order as the templates)
std_names_old <- c(
  "retailer", "fuel", "res_customers", "sme_customers",
  "res_standing_offer", "res_market_offer",
  "customers_entering_assistance", "customers_receiving_assistance",
  "avg_arrears_assisted", "customers_arrears_no_assistance",
  "payment_deferrals", "concession_customers",
  "urg_applications", "urg_approved",
  "calls_received", "calls_answered_30s", "calls_abandoned",
  "complaints_total", "complaints_billing", "complaints_credit",
  "complaints_service", "complaints_marketing", "complaints_transfer",
  "complaints_other",
  "disconnections", "reconnections_7days", "avg_arrears_at_disconnection",
  "best_offer_saving", "notes", "sheet", "file"
)

# New template: same, plus life support before notes
std_names_new <- append(std_names_old, "life_support_customers", after = 28)

# Check the mapping before renaming
tibble(original = names(subs_old), new_name = std_names_old) |> View()
tibble(original = names(subs_new), new_name = std_names_new) |> View()

names(subs_old) <- std_names_old
names(subs_new) <- std_names_new

submissions <- bind_rows(subs_old, subs_new)

dim(submissions)
View(submissions)

# ---- Add period columns ----

submissions <- submissions |>
  mutate(
    fy_end = as.integer(str_extract(file, "\\d{4}")),
    financial_year = paste0(fy_end - 1, "-", str_sub(fy_end, 3, 4)),
    quarter = str_extract(sheet, "Q[1-4]"),
    quarter_start = case_when(
      quarter == "Q1" ~ make_date(fy_end - 1, 7, 1),
      quarter == "Q2" ~ make_date(fy_end - 1, 10, 1),
      quarter == "Q3" ~ make_date(fy_end, 1, 1),
      quarter == "Q4" ~ make_date(fy_end, 4, 1)
    )
  )

# Check: should be 24 periods
submissions |> count(financial_year, quarter, quarter_start)

# ---- Tidy up helper objects ----
rm(list = setdiff(ls(), c("retailers", "submissions", "complaints",
                          "disconnections", "postcodes", "trial",
                          "read_workbook")))

# ---- Save combined (not yet cleaned) submissions ----
saveRDS(submissions, here("data", "processed", "submissions_combined.rds"))

# ----  look: retailers ----
View(retailers)
# retailers: 19 NA in market_exit = retailers still operating 
#(expected, not an issue)

