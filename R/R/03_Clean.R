# 03_clean.R - apply issues log decisions and save clean tables
# ---- Clean complaints ----

# Issue 10: remove exact duplicate rows
complaints_clean <- complaints |>
  distinct()

nrow(complaints) - nrow(complaints_clean)

# Issue 11: map retailer name variants to official names
complaints_clean <- complaints_clean |>
  mutate(retailer = case_match(
    retailer,
    "Everly"                               ~ "Everly Energy",
    "Bright Wave Energy"                   ~ "Brightwave Energy",
    c("CORELINE POWER PTY LTD", "Coreline") ~ "Coreline Power",
    "NEXA ENERGY"                          ~ "Nexa Energy",
    .default = retailer
  ))

# Check: any names still not on the official list?
setdiff(unique(complaints_clean$retailer), retailers$retailer_name)



# Issue 11: map retailer name variants to official names
complaints_clean <- complaints_clean |>
  mutate(retailer = case_when(
    retailer == "Everly"                                 ~ "Everly Energy",
    retailer == "Bright Wave Energy"                     ~ "Brightwave Energy",
    retailer %in% c("CORELINE POWER PTY LTD", "Coreline") ~ "Coreline Power",
    retailer == "NEXA ENERGY"                            ~ "Nexa Energy",
    .default = retailer
  ))


# Issue 13: standardise category to title case
complaints_clean <- complaints_clean |>
  mutate(category = str_to_sentence(str_trim(category)))

complaints_clean |> count(category)


complaints_clean <- complaints_clean |>
  mutate(category = str_to_sentence(str_trim(category)))

complaints_clean |> count(category)

# Issue 14: recode Y/N to Yes/No
complaints_clean <- complaints_clean |>
  mutate(escalated_to_ombudsman = case_when(
    escalated_to_ombudsman == "Y" ~ "Yes",
    escalated_to_ombudsman == "N" ~ "No",
    .default = escalated_to_ombudsman
  ))

complaints_clean |> count(escalated_to_ombudsman)

# Issue 21: convert date_received (3 formats) to a proper date
complaints_clean <- complaints_clean |>
  mutate(date_received = as_date(parse_date_time(date_received,
                                                 orders = c("Ymd", "dmY", "dbY"))))

summary(complaints_clean$date_received)

# Issue 18: negative days_to_resolve set to NA
complaints_clean <- complaints_clean |>
  mutate(days_to_resolve = if_else(days_to_resolve < 0, NA_real_, days_to_resolve))

summary(complaints_clean$days_to_resolve)


# Issue 23: invalid postcodes set to NA
complaints_clean <- complaints_clean |>
  mutate(postcode = if_else(str_detect(postcode, "^3\\d{3}$"), postcode, NA_character_))

# Check: any invalid postcodes left?
complaints_clean |>
  filter(!is.na(postcode), !str_detect(postcode, "^3\\d{3}$")) |>
  nrow()

sum(is.na(complaints_clean$postcode))

# Save clean complaints
saveRDS(complaints_clean, here("data", "processed", "complaints_clean.rds"))

# ---- Clean disconnections ----

# Issue 24: invalid postcodes set to NA
disconnections_clean <- disconnections |>
  mutate(postcode = if_else(str_detect(postcode, "^3\\d{3}$"), postcode, NA_character_))

# Check
disconnections_clean |>
  filter(!is.na(postcode), !str_detect(postcode, "^3\\d{3}$")) |>
  nrow()

sum(is.na(disconnections_clean$postcode))

# Issue 22: first check the 7-day flag for reconnection dates after extract date
disconnections_clean |>
  filter(as_date(reconnection_date) > as_date("2026-06-30")) |>
  count(reconnected_within_7_days)


# Issue 22: future reconnection dates -> NA; unverifiable "Yes" flags -> NA
disconnections_clean <- disconnections_clean |>
  mutate(
    future = !is.na(reconnection_date) & as_date(reconnection_date) > as_date("2026-06-30"),
    reconnected_within_7_days = if_else(future & reconnected_within_7_days == "Yes",
                                        NA_character_, reconnected_within_7_days),
    reconnection_date = if_else(future, as_date(NA), as_date(reconnection_date))
  ) |>
  select(-future)

# Checks
summary(disconnections_clean$reconnection_date)
disconnections_clean |> count(reconnected_within_7_days)

# Save clean disconnections
saveRDS(disconnections_clean, here("data", "processed", "disconnections_clean.rds"))

# ---- Clean submissions ----

# Issue 27: remove gas rows for electricity-only retailers
submissions_clean <- submissions |>
  filter(is.na(notes) | notes != "Does not retail gas")

nrow(submissions) - nrow(submissions_clean)


# Issue 28: remove Summit Energy's original submission, keep the revised one
submissions_clean <- submissions_clean |>
  filter(!(retailer == "Summit Energy" & fuel == "E" &
             financial_year == "2022-23" & quarter == "Q2" & is.na(notes)))

# Check: no duplicate retailer-fuel-quarter combinations left
submissions_clean |>
  count(retailer, fuel, financial_year, quarter) |>
  filter(n > 1)

# Issue 12: map retailer name variants to official names
submissions_clean <- submissions_clean |>
  mutate(retailer = case_when(
    retailer == "Bright Wave Energy"     ~ "Brightwave Energy",
    retailer == "Coreline Power Pty Ltd" ~ "Coreline Power",
    .default = retailer
  ))

# Check
setdiff(unique(submissions_clean$retailer), retailers$retailer_name)

# Issue 3: standardise fuel codes
submissions_clean <- submissions_clean |>
  mutate(fuel = case_when(
    fuel == "E" ~ "Electricity",
    fuel == "G" ~ "Gas",
    .default = fuel
  ))

submissions_clean |> count(fuel)


# Issue 15 (and all indicator columns): convert text to numbers
submissions_clean <- submissions_clean |>
  mutate(across(-c(retailer, fuel, notes, sheet, file,
                   fy_end, financial_year, quarter, quarter_start),
                parse_number))

# Check: missing values per column after converting
colSums(is.na(submissions_clean))


# Issue 26: Harbour Energy customer numbers reported in thousands -> multiply by 1,000
submissions_clean <- submissions_clean |>
  mutate(
    in_thousands = !is.na(notes) & notes == "Customer numbers in '000",
    across(c(res_customers, sme_customers, res_standing_offer, res_market_offer),
           ~ if_else(in_thousands, .x * 1000, .x))
  ) |>
  select(-in_thousands)

# Check
submissions_clean |>
  filter(retailer == "Harbour Energy", fuel == "Electricity", financial_year == "2020-21") |>
  select(quarter, res_customers, sme_customers)

# Issue 16: negative payment_deferrals -> NA
submissions_clean <- submissions_clean |>
  mutate(payment_deferrals = if_else(payment_deferrals < 0, NA_real_, payment_deferrals))

# Check: any negatives left?
sum(submissions_clean$payment_deferrals < 0, na.rm = TRUE)

# Issue 17: Lumen Energy avg_arrears_assisted 10x too big -> divide by 10
submissions_clean <- submissions_clean |>
  mutate(avg_arrears_assisted = if_else(
    retailer == "Lumen Energy" & fuel == "Electricity" &
      financial_year == "2024-25" & quarter == "Q2",
    avg_arrears_assisted / 10,
    avg_arrears_assisted
  ))

# Check: Lumen electricity values in 2024-25
submissions_clean |>
  filter(retailer == "Lumen Energy", fuel == "Electricity", financial_year == "2024-25") |>
  select(quarter, avg_arrears_assisted)

# Issues 19 and 20: flag rows whose totals don't add up
submissions_clean <- submissions_clean |>
  mutate(
    flag_offer_mismatch = (res_standing_offer + res_market_offer) != res_customers,
    flag_category_mismatch = (complaints_billing + complaints_credit + complaints_service +
                                complaints_marketing + complaints_transfer + complaints_other) !=
      complaints_total
  )

submissions_clean |> count(flag_offer_mismatch)
submissions_clean |> count(flag_category_mismatch)

# Save clean submissions
saveRDS(submissions_clean, here("data", "processed", "submissions_clean.rds"))

# ---- Save tables that needed no cleaning ----
saveRDS(retailers, here("data", "processed", "retailers.rds"))
saveRDS(postcodes, here("data", "processed", "postcodes.rds"))
saveRDS(trial, here("data", "processed", "trial.rds"))