# 02_quality.R - data quality checks
# ---- Check 1: duplicates in complaints ----

nrow(complaints)
n_distinct(complaints$complaint_id)
# ---- Check 1: duplicates in complaints ----
tibble(
  total_rows  = nrow(complaints),
  unique_ids  = n_distinct(complaints$complaint_id),
  unique_rows = nrow(distinct(complaints))
)
# ---- Check 1: duplicates in disconnections ----
tibble(
  total_rows  = nrow(disconnections),
  unique_ids  = n_distinct(disconnections$disconnection_id),
  unique_rows = nrow(distinct(disconnections))
)
# ---- Check 1: duplicates in trial ----
tibble(
  total_rows  = nrow(trial),
  unique_ids  = n_distinct(trial$participant_id),
  unique_rows = nrow(distinct(trial))
)
# ---- Check 1: duplicates in postcodes ----
tibble(
  total_rows  = nrow(postcodes),
  unique_ids  = n_distinct(postcodes$postcode),
  unique_rows = nrow(distinct(postcodes))
)
# ---- Check 1: duplicates in retailers ----
tibble(
  total_rows  = nrow(retailers),
  unique_ids  = n_distinct(retailers$retailer_id),
  unique_rows = nrow(distinct(retailers))
)
# ---- Check 1: duplicates in submissions ----
tibble(
  total_rows  = nrow(submissions),
  unique_ids  = nrow(distinct(submissions, retailer, fuel, financial_year, quarter)),
  unique_rows = nrow(distinct(submissions))
)
# ---- Check 2a: retailer names in complaints ----
setdiff(unique(complaints$retailer), retailers$retailer_name)
complaints |>
  filter(!retailer %in% retailers$retailer_name) |>
  count(retailer)

setdiff(unique(disconnections$retailer), retailers$retailer_name)

# ---- Check 2c: retailer names in submissions ----
setdiff(unique(submissions$retailer), retailers$retailer_name)

submissions |>
  filter(!retailer %in% retailers$retailer_name) |>
  count(retailer, financial_year)
# ---- Check 2d: complaint categories ----
complaints |> count(category)

# ---- Check 2e: escalated_to_ombudsman values ----
complaints |> count(escalated_to_ombudsman)

# ---- Check 2f: complaint status values ----
complaints |> count(status)

# ---- Check 2g: complaint fuel values ----
complaints |> count(fuel)

# ---- Check 2h: text columns in disconnections ----
disconnections |>
  select(fuel, assistance_offered, on_payment_plan_before,
         concession_holder, life_support_registered,
         reconnected_within_7_days) |>
  map(table)

# ---- Check 2i: text columns in trial ----
trial |>
  select(retailer, letter_group, concession_holder, region, letter_status) |>
  map(table)

# ---- Check 2j: text columns in retailers and postcodes ----
retailers |> select(size_band, sells_electricity, sells_gas) |> map(table)
postcodes |> count(region)

# ---- Check 3: text in number columns (submissions) ----
submissions |>
  select(-c(retailer, fuel, notes, sheet, file,
            fy_end, financial_year, quarter, quarter_start)) |>
  pivot_longer(everything(), names_to = "column", values_to = "value") |>
  filter(!is.na(value), is.na(suppressWarnings(as.numeric(value)))) |>
  group_by(column) |>
  summarise(rows = n(), examples = paste(head(unique(value), 3), collapse = " | "))

submissions |>
  filter(str_detect(best_offer_saving, "\\$")) |>
  count(financial_year)

# ---- Check 4: impossible values ----

# Temporary numeric copy of submissions (for checking only)
subs_num <- submissions |>
  mutate(across(-c(retailer, fuel, notes, sheet, file,
                   fy_end, financial_year, quarter, quarter_start),
                ~ suppressWarnings(parse_number(.x))))

subs_num |>
  pivot_longer(-c(retailer, fuel, notes, sheet, file,
                  fy_end, financial_year, quarter, quarter_start),
               names_to = "column", values_to = "value") |>
  filter(value < 0) |>
  select(retailer, fuel, financial_year, quarter, column, value)

# ---- Check 4b: implausible values (far from the retailer's usual level) ----
flags <- subs_num |>
  pivot_longer(-c(retailer, fuel, notes, sheet, file,
                  fy_end, financial_year, quarter, quarter_start),
               names_to = "column", values_to = "value") |>
  filter(!is.na(value), value > 0) |>
  group_by(retailer, fuel, column) |>
  mutate(usual = median(value), ratio = value / usual) |>
  ungroup() |>
  filter(usual >= 20, ratio > 5 | ratio < 0.2)

flags |> count(column, sort = TRUE)

flags |>
  filter(column %in% c("disconnections", "reconnections_7days")) |>
  count(financial_year, quarter, ratio_type = if_else(ratio < 1, "too low", "too high"))

flags |>
  filter(!column %in% c("disconnections", "reconnections_7days")) |>
  select(retailer, fuel, financial_year, quarter, column, value, usual, ratio)

# ---- Check 4c: days_to_resolve in complaints ----
summary(complaints$days_to_resolve)
complaints |> filter(days_to_resolve < 0) |> nrow()

# ---- Check 4d: number columns in disconnections ----
summary(disconnections$arrears_amount)
summary(disconnections$days_in_arrears)

# ---- Check 4e: number columns in trial and postcodes ----
summary(trial$arrears_amount)
summary(trial$days_to_contact)

postcodes |>
  select(seifa_irsd_decile, median_weekly_household_income,
         pct_aged_65_plus, population) |>
  summary()

# ---- Check 5: totals that don't add up ----
totals_check <- subs_num |>
  mutate(
    category_sum = complaints_billing + complaints_credit + complaints_service +
      complaints_marketing + complaints_transfer + complaints_other,
    offer_sum = res_standing_offer + res_market_offer,
    complaints_ok = category_sum == complaints_total,
    offers_ok = offer_sum == res_customers
  )

totals_check |> count(complaints_ok)
totals_check |> count(offers_ok)


# Complaints: categories vs total
totals_check |>
  filter(complaints_ok == FALSE) |>
  select(retailer, fuel, financial_year, quarter, category_sum, complaints_total) |>
  mutate(difference = category_sum - complaints_total)

# Offers: standing + market vs residential customers
totals_check |>
  filter(offers_ok == FALSE) |>
  select(retailer, fuel, financial_year, quarter, offer_sum, res_customers) |>
  mutate(difference = offer_sum - res_customers)

totals_check |>
  filter(complaints_ok == FALSE) |>
  select(retailer, fuel, financial_year, quarter, category_sum, complaints_total) |>
  mutate(difference = category_sum - complaints_total)

# ---- Check 6: date formats in complaints ----
complaints |>
  mutate(date_format = case_when(
    str_detect(date_received, "^\\d{4}-\\d{2}-\\d{2}$")      ~ "2023-03-12 (yyyy-mm-dd)",
    str_detect(date_received, "^\\d{2}/\\d{2}/\\d{4}$")      ~ "12/03/2023 (dd/mm/yyyy)",
    str_detect(date_received, "^\\d{2}-[A-Za-z]{3}-\\d{4}$") ~ "12-Mar-2023 (dd-Mon-yyyy)",
    TRUE ~ "other"
  )) |>
  count(date_format)

# Test converting all three formats to proper dates
test_dates <- as_date(parse_date_time(complaints$date_received,
                                      orders = c("Ymd", "dmY", "dbY")))
summary(test_dates)


# ---- Check 6b: dates in disconnections ----
summary(as_date(disconnections$disconnection_date))
summary(as_date(disconnections$reconnection_date))

disconnections |>
  filter(as_date(reconnection_date) < as_date(disconnection_date)) |>
  nrow()

disconnections |>
  filter(as_date(reconnection_date) > as_date("2026-06-30")) |>
  nrow()

# ---- Check 7: postcodes ----
complaints |>
  filter(!is.na(postcode)) |>
  filter(!str_detect(postcode, "^3\\d{3}$")) |>
  count(postcode, sort = TRUE)

disconnections |>
  filter(!is.na(postcode)) |>
  filter(!str_detect(postcode, "^3\\d{3}$")) |>
  count(postcode, sort = TRUE)

# ---- Check 8: postcodes that don't link to the postcodes table ----
complaints |>
  filter(str_detect(postcode, "^3\\d{3}$")) |>
  anti_join(postcodes, by = "postcode") |>
  count(postcode, sort = TRUE)

disconnections |>
  filter(str_detect(postcode, "^3\\d{3}$")) |>
  anti_join(postcodes, by = "postcode") |>
  count(postcode, sort = TRUE)

# ---- Check 9: row count differences ----
q2 <- submissions |> filter(financial_year == "2021-22", quarter == "Q2")
q3 <- submissions |> filter(financial_year == "2021-22", quarter == "Q3")

anti_join(q2, q3, by = c("retailer", "fuel")) |> select(retailer, fuel)

q2_23 <- submissions |> filter(financial_year == "2022-23", quarter == "Q2")
q3_23 <- submissions |> filter(financial_year == "2022-23", quarter == "Q3")

anti_join(q3_23, q2_23, by = c("retailer", "fuel")) |> select(retailer, fuel)


q4_24 <- submissions |> filter(financial_year == "2023-24", quarter == "Q4")
q1_25 <- submissions |> filter(financial_year == "2024-25", quarter == "Q1")

anti_join(q4_24, q1_25, by = c("retailer", "fuel")) |> select(retailer, fuel)