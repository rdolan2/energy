library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(broom)
library(tidyr)
library(forcats)
library(stats)
library(knitr)

# --- Load data ---
df <- read_csv("data/esb.csv")

# --- Prepare data ---
df <- df %>%
  mutate(
    ReadDateTime = dmy_hm(`Read Date and End Time`),
    Date = as_date(ReadDateTime),
    Month = factor(month(ReadDateTime, label = TRUE, abbr = TRUE), levels=month.abb),
    DayOfWeek = wday(ReadDateTime, label = TRUE, abbr = FALSE),
    Period = if_else(Date >= as.Date("2024-06-01"), "After June 2024", "Before June 2024"),
    Period = factor(Period, levels = c("Before June 2024", "After June 2024")),
    DayOfWeek = wday(Date, label = TRUE, abbr = FALSE),
    Month = month(Date, label = TRUE, abbr = FALSE)
  )

# Aggregate to daily total usage
daily_usage <- df %>%
  group_by(Date, Month, DayOfWeek, Period) %>%
  summarise(Daily_kWh = sum(`Read Value`), .groups = "drop")%>%
  arrange(Date)

# After reading in daily_usage.csv
daily_usage <- daily_usage %>%
  mutate(
    DayOfWeekShort = wday(Date, label = TRUE, abbr = TRUE)
  )


# --- 1. Time Series Plot with Seasonality + Cutoff ---
ggplot(daily_usage, aes(x = Date, y = Daily_kWh)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "loess", span = 0.2, se = TRUE, color = "blue") +
  geom_vline(xintercept = as.Date("2024-06-01"), linetype = "dashed", color = "red") +
  labs(title = "Daily Electricity Usage Over Time",
       subtitle = "LOESS smoothed trend with June 1, 2024 cutoff",
       y = "Daily kWh", x = "Date") +
  theme_minimal()

# --- 2. Seasonal Decomposition of Daily Usage ---
# Note: STL requires at least two full seasonal cycles (2 years for daily data)
# Uncomment this if you get more data covering 2+ years

# ts_data <- ts(daily_usage$Daily_kWh, frequency = 365, start = c(year(min(daily_usage$Date)), yday(min(daily_usage$Date))))
# decomp <- stl(ts_data, s.window = "periodic")
# plot(decomp, main = "Seasonal Decomposition of Daily Electricity Usage")


# --- 3. Boxplots of Usage by Weekday and Period, faceted by Month ---
# Remove June–September from comparison plot
excluded_months <- c("June", "July", "August", "September")

filtered_weekday_plot <- daily_usage %>%
  filter(!(Month %in% excluded_months)) %>%
  mutate(DayOfWeek = wday(Date, label = TRUE, abbr = TRUE))

ggplot(filtered_weekday_plot, aes(x = DayOfWeek, y = Daily_kWh, fill = Period)) +
  geom_boxplot(position = position_dodge()) +
  facet_wrap(~Month, scales = "free_y") +
  labs(
    title = "Electricity Usage by Weekday and Period (Excludes June–Sept)",
    x = "Day of Week",
    y = "Daily kWh"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# --- 4. Linear Model with Month and DayOfWeek ---
model <- lm(Daily_kWh ~ Period + Month + DayOfWeek, data = daily_usage)
model_summary <- broom::tidy(model, conf.int = TRUE)

# --- 5. Model Predicted Usage Plot ---
daily_usage <- daily_usage %>%
  mutate(predicted = predict(model, newdata = daily_usage))

ggplot(daily_usage, aes(x = Date)) +
  geom_line(aes(y = Daily_kWh), alpha = 0.3, color = "gray") +
  geom_line(aes(y = predicted), color = "blue", linewidth = 1) +
  geom_vline(xintercept = as.Date("2024-06-01"), linetype = "dashed", color = "red") +
  labs(title = "Observed and Seasonality-Adjusted Predicted Daily Usage",
       y = "kWh", x = "Date") +
  theme_minimal()

# --- 6. Effect Size Plot for PeriodAfter June 2024 ---
effect_plot_data <- model_summary %>%
  filter(term == "PeriodAfter June 2024")

ggplot(effect_plot_data, aes(x = term, y = estimate)) +
  geom_point(size = 4, color = "#fc8d62") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Effect of Period After June 2024 on Daily Usage",
       x = NULL,
       y = "Change in Daily Usage (kWh)") +
  theme_minimal()

# --- 7. Difference-in-Differences Plot ---
filtered_data <- daily_usage %>%
  filter(!(Month %in% excluded_months))

ggplot(filtered_data, aes(x = Period, y = Daily_kWh, fill = Period)) +
  stat_summary(fun = mean, geom = "bar", width = 0.5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(
    title = "Adjusted Comparison of Average Daily Usage (Excludes June–Sept)",
    y = "Daily kWh",
    x = "Period"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# --- 8. Print key statistics for reference ---
print(model_summary %>% filter(term == "PeriodAfter June 2024"))

model_summary %>%
  filter(term == "PeriodAfter June 2024") %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2),
    p.value = signif(p.value, 3),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2)
  ) %>%
  rename(
    Term = term,
    Estimate = estimate,
    `Std. Error` = std.error,
    `t-Statistic` = statistic,
    `p-Value` = p.value,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high
  ) %>%
  kable(
    caption = "Effect of Post-June 2024 on Daily Electricity Usage (adjusted for seasonality)",
    format = "markdown"
  )


# Create an 'output' directory if it doesn't exist
if (!dir.exists("output")) dir.create("output")

# Save the daily usage summary (used for plots)
write_csv(daily_usage, "output/daily_usage.csv")

# Save the t-test results by weekday
write_csv(t_test_results, "output/t_test_results.csv")

# Save the linear model summary (seasonally adjusted analysis)
write_csv(model_summary, "output/model_summary.csv")

