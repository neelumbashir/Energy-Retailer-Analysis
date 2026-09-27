# 06_statistics.R - statistical tests of EDA findings

library(tidyverse)
library(here)

indicators     <- readRDS(here("data", "processed", "indicators.rds"))
disconnections <- readRDS(here("data", "processed", "disconnections_clean.rds"))
complaints     <- readRDS(here("data", "processed", "complaints_clean.rds"))
postcodes      <- readRDS(here("data", "processed", "postcodes.rds"))
trial          <- readRDS(here("data", "processed", "trial.rds"))


# ---- Test 1: A/B test - redesigned vs standard letter ----
ab_table <- trial |>
  filter(letter_status == "Delivered") |>
  group_by(letter_group) |>
  summarise(sought = sum(sought_assistance_30d), n = n(), .groups = "drop")

ab_table

ab_test <- prop.test(x = ab_table$sought, n = ab_table$n)
ab_test

# Test 1 result: redesigned 22.3% vs standard 17.9%; diff 4.4 pts (95% CI 2.3-6.4); p < 0.001


# ---- Test 2: Brightwave - disconnections without assistance ----
assist_table <- disconnections |>
  mutate(group = if_else(retailer == "Brightwave Energy", "Brightwave", "All other retailers")) |>
  group_by(group) |>
  summarise(no_assistance = sum(assistance_offered == "No"), n = n(), .groups = "drop")

assist_table

assist_test <- prop.test(x = assist_table$no_assistance, n = assist_table$n)
assist_test

# Test 2 result: Brightwave 47.9% vs others 8.2% disconnected without assistance;
#   diff 39.7 pts (95% CI 38.7-40.8); p < 0.001


# ---- Test 3: Brightwave - arrears at disconnection ----
arrears_data <- disconnections |>
  filter(!is.na(arrears_amount)) |>
  mutate(group = if_else(retailer == "Brightwave Energy", "Brightwave", "All other retailers"))

arrears_data |>
  group_by(group) |>
  summarise(median_arrears = median(arrears_amount), n = n(), .groups = "drop")

wilcox.test(arrears_amount ~ group, data = arrears_data, conf.int = TRUE)

# Test 3 result: Brightwave arrears at disconnection ~$780 lower than others
#   (Hodges-Lehmann, 95% CI $769-$792); Wilcoxon p < 0.001

# ---- Test 4a: funnel plot - disconnection rate, 2024-25 ----
funnel_disc <- indicators |>
  filter(financial_year == "2024-25") |>
  group_by(retailer) |>
  summarise(
    disconnections = sum(disconnections),
    customers      = sum(res_customers) / 4,
    .groups = "drop"
  ) |>
  mutate(rate = disconnections / customers * 100)

# Industry rate and funnel limits
p0 <- sum(funnel_disc$disconnections) / sum(funnel_disc$customers)
limits <- tibble(customers = seq(min(funnel_disc$customers), max(funnel_disc$customers),
                                 length.out = 200)) |>
  mutate(se = sqrt(p0 * (1 - p0) / customers),
         lower95 = (p0 - 1.96 * se) * 100, upper95 = (p0 + 1.96 * se) * 100,
         lower998 = (p0 - 3.09 * se) * 100, upper998 = (p0 + 3.09 * se) * 100)

# Which retailers are outside the 99.8% limits?
funnel_disc <- funnel_disc |>
  mutate(se = sqrt(p0 * (1 - p0) / customers),
         outlier = rate > (p0 + 3.09 * se) * 100 | rate < (p0 - 3.09 * se) * 100)

funnel_disc |> filter(outlier) |> select(retailer, customers, rate)

# Test 4a result: disconnection funnel 2024-25 - Brightwave (1.51) and Nexa (0.96) above 99.8% limits;
#   Coreline, Everly, Tidewell slightly below (large n, narrow limits - not practically important)

# Funnel plot: disconnection rate 2024-25
high_outliers <- funnel_disc |> filter(outlier, rate > p0 * 100)

p_funnel_disc <- ggplot() +
  geom_line(data = limits, aes(x = customers, y = upper95), linetype = "dashed", colour = "grey50") +
  geom_line(data = limits, aes(x = customers, y = lower95), linetype = "dashed", colour = "grey50") +
  geom_line(data = limits, aes(x = customers, y = upper998), colour = "grey30") +
  geom_line(data = limits, aes(x = customers, y = lower998), colour = "grey30") +
  geom_hline(yintercept = p0 * 100, colour = "black") +
  geom_point(data = funnel_disc, aes(x = customers, y = rate), colour = "grey60", size = 3) +
  geom_point(data = high_outliers, aes(x = customers, y = rate), colour = "#d95f02", size = 3.5) +
  geom_text(data = high_outliers, aes(x = customers, y = rate, label = retailer),
            hjust = -0.1, vjust = -0.6, colour = "#d95f02", size = 3.5) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Brightwave and Nexa disconnect significantly more than expected for their size",
       subtitle = "Disconnections per 100 customers, 2024-25. Solid lines: 99.8% limits; dashed: 95%",
       x = "Average residential customers", y = "Disconnections per 100 customers") +
  theme_minimal()

p_funnel_disc

ggsave(here("outputs", "figures", "stats4a_funnel_disconnections.png"), p_funnel_disc,
       width = 8, height = 5, dpi = 300)


# ---- Funnel plot function ----
make_funnel <- function(df, title, y_label, direction = "high") {
  # df needs columns: retailer, count, customers
  df <- df |> mutate(rate = count / customers * 100)
  p0 <- sum(df$count) / sum(df$customers)
  
  limits <- tibble(customers = seq(min(df$customers), max(df$customers), length.out = 200)) |>
    mutate(se = sqrt(p0 * (1 - p0) / customers),
           lower95 = (p0 - 1.96 * se) * 100, upper95 = (p0 + 1.96 * se) * 100,
           lower998 = (p0 - 3.09 * se) * 100, upper998 = (p0 + 3.09 * se) * 100)
  
  df <- df |>
    mutate(se = sqrt(p0 * (1 - p0) / customers),
           outlier = if (direction == "high") rate > (p0 + 3.09 * se) * 100
           else rate < (p0 - 3.09 * se) * 100)
  
  flagged <- df |> filter(outlier)
  print(flagged |> select(retailer, customers, rate))
  
  ggplot() +
    geom_line(data = limits, aes(customers, upper95), linetype = "dashed", colour = "grey50") +
    geom_line(data = limits, aes(customers, lower95), linetype = "dashed", colour = "grey50") +
    geom_line(data = limits, aes(customers, upper998), colour = "grey30") +
    geom_line(data = limits, aes(customers, lower998), colour = "grey30") +
    geom_hline(yintercept = p0 * 100) +
    geom_point(data = df, aes(customers, rate), colour = "grey60", size = 3) +
    geom_point(data = flagged, aes(customers, rate), colour = "#d95f02", size = 3.5) +
    geom_text(data = flagged, aes(customers, rate, label = retailer),
              hjust = -0.1, vjust = -0.6, colour = "#d95f02", size = 3.5) +
    scale_x_log10(labels = scales::comma) +
    labs(title = title,
         subtitle = "2024-25. Solid lines: 99.8% limits; dashed: 95%",
         x = "Average residential customers", y = y_label) +
    theme_minimal()
}


# ---- Test 4b: funnel plot - assistance gap, 2024-25 ----
gap_df <- indicators |>
  filter(financial_year == "2024-25") |>
  group_by(retailer) |>
  summarise(count     = sum(customers_arrears_no_assistance) / 4,
            customers = sum(res_customers) / 4,
            .groups = "drop")

p_funnel_gap <- make_funnel(gap_df,
                            title = "Nexa and Brightwave leave far more customers in arrears without help",
                            y_label = "Customers in arrears without assistance per 100",
                            direction = "high")

ggsave(here("outputs", "figures", "stats4b_funnel_assistance_gap.png"), p_funnel_gap,
       width = 8, height = 5, dpi = 300)
p_funnel_gap

# Test 4b result: assistance gap funnel 2024-25 - Nexa 6.86 (~3x industry) and Brightwave 3.46 above 99.8% limits

# ---- Test 4c: funnel plot - concession rate, 2024-25 ----
conc_df <- indicators |>
  filter(financial_year == "2024-25") |>
  group_by(retailer) |>
  summarise(count     = sum(concession_customers) / 4,
            customers = sum(res_customers) / 4,
            .groups = "drop")

p_funnel_conc <- make_funnel(conc_df,
                             title = "Summit Energy applies far fewer concessions than expected",
                             y_label = "Customers receiving concessions per 100",
                             direction = "low")
p_funnel_conc

ggsave(here("outputs", "figures", "stats4c_funnel_concessions.png"), p_funnel_conc,
       width = 8, height = 5, dpi = 300)

# Test 4c result: concession funnel 2024-25 - Summit 7.99 per 100 vs industry ~18.5; far below 99.8% limits

# ---- Test 5: service by retailer size (Kruskal-Wallis) ----
service_retailer <- indicators |>
  group_by(retailer, size_band) |>
  summarise(
    complaint_vs_industry = median(complaint_rate / ind_complaint_rate),
    calls_answered        = median(calls_answered_rate, na.rm = TRUE),
    .groups = "drop"
  )

# Complaints
kruskal.test(complaint_vs_industry ~ size_band, data = service_retailer)

# Calls answered within 30 seconds
kruskal.test(calls_answered ~ size_band, data = service_retailer)

# Post-hoc: which size bands differ? (pairwise Wilcoxon, Holm correction)
pairwise.wilcox.test(service_retailer$complaint_vs_industry, service_retailer$size_band,
                     p.adjust.method = "holm")

# Test 5 result: Kruskal-Wallis p = 0.0002 for complaints and calls answered by size band;
#   post-hoc (Holm): all pairs differ (p <= 0.003) - small worst, medium middle, large best


# ---- Test 6: disconnections and disadvantage (Poisson regression) ----
# Disconnections per postcode (valid postcodes only)
disc_pc <- disconnections |>
  filter(!is.na(postcode)) |>
  count(postcode, name = "disconnections")

# One row per postcode, including postcodes with no disconnections
model_data <- postcodes |>
  left_join(disc_pc, by = "postcode") |>
  mutate(disconnections = replace_na(disconnections, 0))

pois <- glm(disconnections ~ seifa_irsd_decile + pct_aged_65_plus + region +
              offset(log(population)),
            family = poisson, data = model_data)
summary(pois)

# Overdispersion check (should be close to 1 for Poisson)
sum(residuals(pois, type = "pearson")^2) / df.residual(pois)
# Rate ratios with 95% confidence intervals
exp(cbind(rate_ratio = coef(pois), confint.default(pois)))

# Test 6 result: Poisson (dispersion 0.94, fits well). Decile RR ~0.845 per step (p < 0.001):
#   ~15.5% lower rate per decile; decile 1 ~4.6x decile 10. Age and region not significant.
# Test 6 result: decile RR 0.845 (95% CI 0.843-0.847); age and region CIs include 1 (no effect)

# ---- Test 7: complaint resolution times (survival analysis) ----
library(survival)
retailers <- readRDS(here("data", "processed", "retailers.rds"))

surv_data <- complaints |>
  left_join(retailers |> select(retailer_name, size_band),
            by = c("retailer" = "retailer_name")) |>
  mutate(
    event = if_else(status == "Resolved", 1, 0),
    time  = if_else(status == "Resolved", days_to_resolve,
                    as.numeric(as_date("2026-06-30") - date_received)),
    group = if_else(retailer == "Nexa Energy", "Nexa Energy",
                    paste(size_band, "retailers"))
  ) |>
  filter(!is.na(time))

km <- survfit(Surv(time, event) ~ group, data = surv_data)
km

survdiff(Surv(time, event) ~ group, data = surv_data)

# Kaplan-Meier chart: % of complaints resolved over time
km_df <- broom::tidy(km) |>
  mutate(group = str_remove(strata, "group="))

p_km <- ggplot(km_df, aes(x = time, y = (1 - estimate) * 100, colour = group)) +
  geom_step(linewidth = 1) +
  scale_colour_manual(values = c("Large retailers" = "grey30", "Medium retailers" = "grey55",
                                 "Small retailers" = "grey75", "Nexa Energy" = "#d95f02")) +
  coord_cartesian(xlim = c(0, 60)) +
  labs(title = "Nexa takes three times as long as large retailers to resolve complaints",
       subtitle = "% of complaints resolved by days since received (Kaplan-Meier; open complaints censored)",
       x = "Days since complaint received", y = "Complaints resolved (%)", colour = NULL) +
  theme_minimal()

p_km

ggsave(here("outputs", "figures", "stats7_resolution_km.png"), p_km,
       width = 8, height = 5, dpi = 300)

# Test 7 result: KM medians - large 7, medium 10, small 16, Nexa 21 days (CI 20-23); log-rank p < 0.001


# ---- Test 8: Coreline billing complaints - change-point analysis ----
library(changepoint)

coreline_monthly <- complaints |>
  filter(retailer == "Coreline Power", category == "Billing") |>
  count(month = floor_date(date_received, "month")) |>
  arrange(month)

# Scale by typical month-to-month noise so the method knows normal variation
noise_sd <- mad(diff(coreline_monthly$n)) / sqrt(2)

cp <- cpt.mean(coreline_monthly$n / noise_sd, method = "PELT", penalty = "MBIC")

# Where the changes happen, and the average monthly complaints in each period
coreline_monthly$month[cpts(cp)]
param.est(cp)$mean * noise_sd

# Chart: Coreline billing complaints with change-point levels
seg_lengths <- diff(c(0, cpts(cp), nrow(coreline_monthly)))
coreline_monthly$level <- rep(param.est(cp)$mean * noise_sd, seg_lengths)

p_cp <- ggplot(coreline_monthly, aes(x = month)) +
  geom_line(aes(y = n), colour = "grey60") +
  geom_step(aes(y = level), colour = "#d95f02", linewidth = 1.2) +
  labs(title = "Coreline's billing complaints tripled for a year from Oct 2023",
       subtitle = "Monthly billing complaints (grey) and period averages from change-point analysis (orange)",
       x = NULL, y = "Billing complaints per month") +
  theme_minimal()

p_cp

ggsave(here("outputs", "figures", "stats8_coreline_changepoint.png"), p_cp,
       width = 8, height = 5, dpi = 300)

# Test 8 result: change points after Sep 2023, Jun 2024, Sep 2024. Levels: 125 -> 388 (3.1x) -> 277 -> 129/month
#   Coreline billing problem: Oct 2023 to Sep 2024 (~12 months); peak Oct 2023 - Jun 2024

