# 05_eda.R - exploratory data analysis

library(tidyverse)
library(here)

indicators <- readRDS(here("data", "processed", "indicators.rds"))
industry   <- readRDS(here("data", "processed", "industry.rds"))

# ---- EDA 1: assistance gap by retailer ----
gap_by_retailer <- indicators |>
  mutate(gap_vs_industry = assistance_gap_rate / ind_assistance_gap_rate) |>
  group_by(retailer, size_band) |>
  summarise(gap_vs_industry = median(gap_vs_industry), .groups = "drop") |>
  arrange(desc(gap_vs_industry))

gap_by_retailer |> print(n = 20)

# Chart: assistance gap vs industry

p_gap <- ggplot(gap_by_retailer, aes(x = gap_vs_industry,
                                     y = reorder(retailer, gap_vs_industry),
                                     fill = size_band)) +
  geom_col() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(title = "Nexa Energy's assistance gap is about 3 times the industry average",
       x = "Assistance gap (times industry average)", y = NULL, fill = "Retailer size") +
  theme_minimal()

p_gap

dir.create(here("outputs", "figures"), showWarnings = FALSE)
ggsave(here("outputs", "figures", "eda1_assistance_gap.png"), p_gap,
       width = 8, height = 5, dpi = 300)


# ---- EDA 2: disconnections by retailer (excluding 2020-21 COVID pause) ----
disc_by_retailer <- indicators |>
  filter(financial_year != "2020-21") |>
  mutate(disc_vs_industry = disconnection_rate / ind_disconnection_rate) |>
  group_by(retailer, size_band) |>
  summarise(
    disc_vs_industry        = median(disc_vs_industry),
    quick_reconnection_rate = median(quick_reconnection_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(disc_vs_industry))

disc_by_retailer |> print(n = 20)


# Chart: quick reconnection rate by retailer (Brightwave highlighted)
p_reconnect <- disc_by_retailer |>
  mutate(highlight = retailer == "Brightwave Energy") |>
  ggplot(aes(x = quick_reconnection_rate,
             y = reorder(retailer, quick_reconnection_rate),
             fill = highlight)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "#d95f02", "FALSE" = "grey70"), guide = "none") +
  labs(title = "Brightwave reconnects over half of disconnected customers within a week",
       subtitle = "Median % reconnected within 7 days, 2021-22 to 2025-26",
       x = "Reconnected within 7 days (%)", y = NULL) +
  theme_minimal()

p_reconnect

ggsave(here("outputs", "figures", "eda2_quick_reconnections.png"), p_reconnect,
       width = 8, height = 5, dpi = 300)

# ---- EDA 3: concessions and grants by retailer ----
conc_by_retailer <- indicators |>
  mutate(conc_vs_industry = concession_rate / ind_concession_rate) |>
  group_by(retailer, size_band) |>
  summarise(
    conc_vs_industry       = median(conc_vs_industry),
    grant_application_rate = median(grant_application_rate),
    grant_approval_rate    = median(grant_approval_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(conc_vs_industry)

conc_by_retailer |> print(n = 20)

# Chart: concessions and grant applications by retailer (Summit highlighted)
p_conc_trend <- indicators |>
  filter(retailer == "Summit Energy", fuel == "Electricity") |>
  ggplot(aes(x = quarter_start)) +
  geom_line(aes(y = concession_rate, colour = "Summit Energy"), linewidth = 1) +
  geom_line(aes(y = ind_concession_rate, colour = "Industry"), linewidth = 1,
            linetype = "dashed") +
  scale_colour_manual(values = c("Summit Energy" = "#d95f02", "Industry" = "grey40")) +
  scale_y_continuous(limits = c(0, 25)) +
  labs(title = "Summit's concession rate has been low throughout",
       subtitle = "Electricity customers receiving concessions, %",
       x = NULL, y = "Customers with concessions (%)", colour = NULL) +
  theme_minimal()

p_conc_trend

ggsave(here("outputs", "figures", "eda3_concessions_trend.png"), p_conc_trend,
       width = 8, height = 5, dpi = 300)

# ---- EDA 4: best offer saving and standing offers by retailer ----
offer_by_retailer <- indicators |>
  group_by(retailer, size_band) |>
  summarise(
    best_offer_saving    = median(best_offer_saving),
    standing_offer_share = median(standing_offer_share, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(best_offer_saving))

offer_by_retailer |> print(n = 20)

# Chart: best offer saving vs standing offer share
p_offer <- ggplot(offer_by_retailer, aes(x = standing_offer_share, y = best_offer_saving,
                                         colour = size_band)) +
  geom_point(size = 3) +
  geom_text(data = filter(offer_by_retailer, retailer == "Everly Energy"),
            aes(label = retailer), hjust = 1.15, size = 3.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(0, 23)) +
  scale_y_continuous(limits = c(0, 470)) +
  labs(title = "Everly's customers could save the most by switching to its best offer",
       subtitle = "Median 2020-21 to 2025-26; larger retailers have more standing-offer customers",
       x = "Customers on standing offers (%)",
       y = "Average annual saving from best offer ($)", colour = "Retailer size") +
  theme_minimal()

p_offer

ggsave(here("outputs", "figures", "eda4_best_offer.png"), p_offer,
       width = 8, height = 5, dpi = 300)

# EDA 4 finding: Everly best offer saving $432 (vs ~$200 other large), standing share 20% (vs ~9%)
# EDA 4 pattern: large > medium > small for both saving and standing offer share

# ---- EDA 5: customer service by retailer ----
# ---- EDA 5: customer service by retailer ----
service_by_retailer <- indicators |>
  mutate(complaint_vs_industry = complaint_rate / ind_complaint_rate) |>
  group_by(retailer, size_band) |>
  summarise(
    worst_abandonment     = max(abandonment_rate, na.rm = TRUE),
    complaint_vs_industry = median(complaint_vs_industry),
    calls_answered_rate   = median(calls_answered_rate, na.rm = TRUE),
    abandonment_rate      = median(abandonment_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(complaint_vs_industry))

service_by_retailer |>
  select(retailer, abandonment_rate, worst_abandonment) |>
  arrange(desc(worst_abandonment))

# Chart: Coreline call abandonment over time vs industry
p_coreline <- indicators |>
  filter(retailer == "Coreline Power", fuel == "Electricity") |>
  ggplot(aes(x = quarter_start)) +
  geom_line(aes(y = abandonment_rate, colour = "Coreline Power"), linewidth = 1) +
  geom_line(aes(y = ind_abandonment_rate, colour = "Industry"), linewidth = 1,
            linetype = "dashed") +
  scale_colour_manual(values = c("Coreline Power" = "#d95f02", "Industry" = "grey40")) +
  labs(title = "Coreline's call abandonment spiked from late 2023 to late 2024",
       subtitle = "Electricity calls abandoned before being answered, %",
       x = NULL, y = "Calls abandoned (%)", colour = NULL) +
  theme_minimal()

p_coreline

ggsave(here("outputs", "figures", "eda5_coreline_abandonment.png"), p_coreline,
       width = 8, height = 5, dpi = 300)

# EDA 5 pattern: small retailers worse on service (complaints ~1.5x, 68% answered, 9% abandonment)
# EDA 5 finding: Coreline abandonment typical 5% but worst 21% - temporary spike (check dates on chart)

# ---- EDA 6: disconnection records by retailer ----
disconnections <- readRDS(here("data", "processed", "disconnections_clean.rds"))

disc_records <- disconnections |>
  group_by(retailer) |>
  summarise(
    disconnections     = n(),
    median_arrears     = median(arrears_amount, na.rm = TRUE),
    pct_no_assistance  = mean(assistance_offered == "No") * 100,
    median_days_arrears = median(days_in_arrears),
    life_support       = sum(life_support_registered == "Yes"),
    .groups = "drop"
  ) |>
  arrange(median_arrears)

disc_records |> print(n = 20)

# Chart: % disconnected without assistance offered
p_noassist <- disc_records |>
  mutate(highlight = retailer %in% c("Brightwave Energy", "Nexa Energy")) |>
  ggplot(aes(x = pct_no_assistance, y = reorder(retailer, pct_no_assistance),
             fill = highlight)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "#d95f02", "FALSE" = "grey70"), guide = "none") +
  labs(title = "Brightwave disconnected almost half its customers without offering help",
       subtitle = "% of disconnections where tailored assistance was not offered, 2020-21 to 2025-26",
       x = "Disconnected without assistance offered (%)", y = NULL) +
  theme_minimal()

p_noassist

ggsave(here("outputs", "figures", "eda6_no_assistance.png"), p_noassist,
       width = 8, height = 5, dpi = 300)

# EDA 6 finding: Brightwave median arrears $425 (vs ~$1,240), 75 days in arrears (vs ~160),
#   48% no assistance (vs ~8%), 3 life support disconnections
# EDA 6 finding: Nexa 21% no assistance, 1 life support disconnection

# ---- EDA 7: complaint records by retailer ----
complaints <- readRDS(here("data", "processed", "complaints_clean.rds"))

comp_records <- complaints |>
  group_by(retailer) |>
  summarise(
    complaints         = n(),
    median_days        = median(days_to_resolve, na.rm = TRUE),
    pct_escalated      = mean(escalated_to_ombudsman == "Yes") * 100,
    pct_billing        = mean(category == "Billing") * 100,
    .groups = "drop"
  ) |>
  arrange(desc(median_days))

comp_records |> print(n = 20)

# Coreline: billing vs other complaints over time
p_coreline_comp <- complaints |>
  filter(retailer == "Coreline Power") |>
  mutate(month = floor_date(date_received, "month"),
         type = if_else(category == "Billing", "Billing", "All other categories")) |>
  count(month, type) |>
  ggplot(aes(x = month, y = n, colour = type)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("Billing" = "#d95f02", "All other categories" = "grey50")) +
  labs(title = "Coreline's billing complaints surged in the same period as its call-centre problem",
       subtitle = "Coreline Power complaints per month",
       x = NULL, y = "Complaints per month", colour = NULL) +
  theme_minimal()

p_coreline_comp
ggsave(here("outputs", "figures", "eda7_coreline_billing.png"), p_coreline_comp,
       width = 8, height = 5, dpi = 300)

# ---- EDA 8: disconnections by disadvantage decile ----
postcodes <- readRDS(here("data", "processed", "postcodes.rds"))

# Population per decile
pop_by_decile <- postcodes |>
  group_by(seifa_irsd_decile) |>
  summarise(population = sum(population), .groups = "drop")

# Disconnections per decile, then rate per 1,000 people
disc_by_decile <- disconnections |>
  filter(!is.na(postcode)) |>
  left_join(postcodes, by = "postcode") |>
  count(seifa_irsd_decile, name = "disconnections") |>
  left_join(pop_by_decile, by = "seifa_irsd_decile") |>
  mutate(rate_per_1000 = disconnections / population * 1000)

disc_by_decile

# Chart: disconnection rate by disadvantage decile
p_decile <- ggplot(disc_by_decile, aes(x = factor(seifa_irsd_decile), y = rate_per_1000)) +
  geom_col(fill = "#d95f02") +
  labs(title = "Disconnections are over 4 times higher in the most disadvantaged areas",
       subtitle = "Disconnections per 1,000 people, 2020-21 to 2025-26, by SEIFA decile",
       x = "Disadvantage decile (1 = most disadvantaged, 10 = least)",
       y = "Disconnections per 1,000 people") +
  theme_minimal()

p_decile

ggsave(here("outputs", "figures", "eda8_disconnections_decile.png"), p_decile,
       width = 8, height = 5, dpi = 300)

# EDA 8 finding: disconnections 31.8 per 1,000 in decile 1 vs 6.9 in decile 10 (~4.6x); steady gradient

# ---- EDA 9: outreach trial first look ----
trial <- readRDS(here("data", "processed", "trial.rds"))

# 1. Balance: undeliverable letters by group
trial |> count(letter_group, letter_status)

# 2. Outcome: % who sought assistance (delivered letters only)
trial |>
  filter(letter_status == "Delivered") |>
  group_by(letter_group) |>
  summarise(
    customers     = n(),
    sought_help   = sum(sought_assistance_30d),
    pct_sought    = mean(sought_assistance_30d) * 100,
    .groups = "drop"
  )

# EDA 9: undeliverable letters balanced (62 vs 56)
# EDA 9 finding: redesigned letter 22.3% sought help vs standard 17.9% (+4.4 points) - test formally

# Chart: outreach trial - % who sought help by letter group
trial_summary <- trial |>
  filter(letter_status == "Delivered") |>
  group_by(letter_group) |>
  summarise(pct_sought = mean(sought_assistance_30d) * 100, .groups = "drop")

p_trial <- ggplot(trial_summary, aes(x = letter_group, y = pct_sought, fill = letter_group)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(round(pct_sought, 1), "%")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Redesigned letter" = "#d95f02", "Standard letter" = "grey70"),
                    guide = "none") +
  scale_y_continuous(limits = c(0, 26)) +
  labs(title = "More customers sought help after receiving the redesigned letter",
       subtitle = "% of customers in arrears who contacted their retailer within 30 days (delivered letters)",
       x = NULL, y = "Sought assistance within 30 days (%)") +
  theme_minimal()

p_trial

ggsave(here("outputs", "figures", "eda9_trial.png"), p_trial,
       width = 7, height = 5, dpi = 300)

# ---- EDA SUMMARY ----
# Market-wide:
#   - Assistance rate rising from 2022-23 (~2.5 -> ~3.8 per 100): cost of living
#   - Disconnections nearly doubled after COVID pause; winter peaks
#   - Disconnections ~4.6x higher in most vs least disadvantaged areas
#   - Small retailers worse on service (complaints, calls, resolution time)
#   - Large retailers: more standing-offer customers, bigger best-offer savings
# Retailers:
#   - Nexa: assistance gap ~2.9x; 21% disconnected without help; slow complaints; 1 life support
#   - Brightwave: disconnects ~1.9x; small debts ($425); 48% no help; 56% quick reconnection; 3 life support
#   - Summit: concessions ~0.43x; few grant applications
#   - Everly: 20% standing offers; $432 best-offer saving
#   - Coreline: billing problem late 2023 - late 2024 (billing complaints + call abandonment)
# Trial:
#   - Redesigned letter 22.3% vs standard 17.9% sought help

