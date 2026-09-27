# 04_indicators.R - build indicators from clean data

library(tidyverse)
library(here)

submissions <- readRDS(here("data", "processed", "submissions_clean.rds"))
retailers   <- readRDS(here("data", "processed", "retailers.rds"))

# ---- Indicators: Q1 assistance for customers in debt ----
indicators <- submissions |>
  mutate(
    assistance_rate     = customers_receiving_assistance / res_customers * 100,
    assistance_gap_rate = customers_arrears_no_assistance / res_customers * 100
  )

indicators |>
  select(assistance_rate, assistance_gap_rate) |>
  summary()

# ---- Indicators: Q2 disconnections ----
indicators <- indicators |>
  mutate(
    disconnection_rate      = disconnections / res_customers * 100,
    quick_reconnection_rate = if_else(disconnections > 0,
                                      reconnections_7days / disconnections * 100,
                                      NA_real_)
  )

indicators |>
  select(disconnection_rate, quick_reconnection_rate) |>
  summary()


# ---- Indicators: Q3 concessions and grants ----
indicators <- indicators |>
  mutate(
    concession_rate        = concession_customers / res_customers * 100,
    grant_application_rate = urg_applications / customers_receiving_assistance * 100,
    grant_approval_rate    = if_else(urg_applications > 0,
                                     urg_approved / urg_applications * 100,
                                     NA_real_)
  )

indicators |>
  select(concession_rate, grant_application_rate, grant_approval_rate) |>
  summary()

# ---- Indicators: Q4 best offer and contract type ----
indicators <- indicators |>
  mutate(
    standing_offer_share = if_else(flag_offer_mismatch,
                                   NA_real_,
                                   res_standing_offer / res_customers * 100)
  )

indicators |>
  select(best_offer_saving, standing_offer_share) |>
  summary()

# ---- Indicators: Q5 customer service ----
indicators <- indicators |>
  mutate(
    complaint_rate      = complaints_total / res_customers * 100,
    calls_answered_rate = calls_answered_30s / calls_received * 100,
    abandonment_rate    = calls_abandoned / calls_received * 100
  )

indicators |>
  select(complaint_rate, calls_answered_rate, abandonment_rate) |>
  summary()


# Q1 note: assistance gap max 7.6 vs typical ~1.9 - which retailer? Definition change from 2023-24 Q1
# Q2 note: disconnection rate 0 during COVID pause; quick reconnection 100% = small numbers
# Q3 note: concession rate min 7.3% and grant application min 3.8 far below typical - which retailer?
# Q4 note: best offer saving max $579 and standing share max 23% far above typical - same retailer?
# Q5 note: calls answered min 42% and abandonment max 21% - likely Coreline call-centre problem


# ---- Industry averages (per quarter and fuel) ----
industry <- indicators |>
  group_by(financial_year, quarter, quarter_start, fuel) |>
  summarise(
    ind_assistance_rate     = sum(customers_receiving_assistance) / sum(res_customers) * 100,
    ind_assistance_gap_rate = sum(customers_arrears_no_assistance) / sum(res_customers) * 100,
    ind_disconnection_rate  = sum(disconnections) / sum(res_customers) * 100,
    ind_concession_rate     = sum(concession_customers) / sum(res_customers) * 100,
    ind_complaint_rate      = sum(complaints_total) / sum(res_customers) * 100,
    ind_abandonment_rate    = sum(calls_abandoned[!is.na(calls_received)]) /
      sum(calls_received, na.rm = TRUE) * 100,
    .groups = "drop"
  )

industry |> filter(fuel == "Electricity") |> print(n = 24)

# Industry: assistance rate rising from 2022-23 (~2.5 -> ~3.8) - cost of living
# Industry: disconnections near zero in COVID pause, then nearly doubled 2021-22 to 2024-26; winter (Q1) peaks
# Industry: concession rate stable ~18.5%
# Industry: complaints bump 2023-24 Q2-Q4 - check Coreline

# ---- Add industry averages and retailer size ----
indicators <- indicators |>
  left_join(industry, by = c("financial_year", "quarter", "quarter_start", "fuel")) |>
  left_join(retailers |> select(retailer_name, size_band),
            by = c("retailer" = "retailer_name"))

# Checks
nrow(indicators)
sum(is.na(indicators$size_band))
sum(is.na(indicators$ind_complaint_rate))

# ---- Save indicators ----
saveRDS(indicators, here("data", "processed", "indicators.rds"))
saveRDS(industry, here("data", "processed", "industry.rds"))

write_csv(indicators, here("data", "processed", "indicators.csv"))
write_csv(industry, here("data", "processed", "industry.csv"))
