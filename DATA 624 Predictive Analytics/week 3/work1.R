# Load necessary libraries
library(tsibble)
library(tsibbledata)
library(fpp3)
library(fpp2)
library(dplyr)
library(lubridate)
library(patchwork)
library(ggplot2)

# Load datasets
data("global_economy")
data("aus_production")
data("aus_livestock")
data("hh_budget")
data("aus_retail")

# 1. Australian Population (global_economy)
# Population data follows a long-term trend, so RW with drift is appropriate
pop_data <- global_economy %>%
  filter(Country == "Australia") %>%
  select(Year, Population) %>%
  as_tsibble(index = Year)

pop_forecast <- pop_data %>%
  model(RW(Population ~ drift())) %>%
  forecast(h = 10) # Forecast for 10 years

# 2. Bricks production (aus_production)
# Brick production has a seasonal pattern, so Seasonal Naïve is appropriate
bricks_data <- aus_production %>%
  filter(Bricks > 0) %>%
  select(Quarter, Bricks) %>%
  as_tsibble(index = Quarter)

bricks_forecast <- bricks_data %>%
  model(SNAIVE(Bricks)) %>%
  forecast(h = "2 years") # Forecast for 2 years


# 3. NSW Lambs (aus_livestock)
# Lamb production has strong seasonal effects, so Seasonal Naïve is appropriate
lambs_data <- aus_livestock %>%
  filter(State == "New South Wales", Animal == "Lambs") %>%
  select(Month, Count) %>%
  as_tsibble(index = Month)

lambs_forecast <- lambs_data %>%
  model(SNAIVE(Count)) %>%
  forecast(h = "1 year") # Forecast for 1 year
autoplot(lambs_forecast) + ggtitle("Forecast: NSW Lambs")



# 4. Household Wealth (hh_budget)
# Wealth data typically follows a trend, so RW with drift is appropriate
wealth_data <- hh_budget %>%
  select(Year, Wealth) %>%
  as_tsibble(index = Year)

wealth_forecast <- wealth_data %>%
  model(RW(Wealth ~ drift())) %>%
  forecast(h = "2 years") # Forecast for 2 years
autoplot(wealth_forecast) + ggtitle("Forecast: Household Wealth") 



# 5. Australian Takeaway Food Turnover (aus_retail)
# Retail sales have strong seasonality, so Seasonal Naïve is appropriate
takeaway_data <- aus_retail %>%
  filter(Industry == "Takeaway food services") %>%
  select(Month, Turnover) %>%
  as_tsibble(index = Month)

takeaway_forecast <- takeaway_data %>%
  model(SNAIVE(Turnover)) %>%
  forecast(h = "1 year") # Forecast for 1 year
autoplot(takeaway_forecast) + ggtitle("Forecast: Takeaway Food Turnover")
autoplot(bricks_forecast) + ggtitle("Forecast: Bricks Production")

autoplot(lambs_forecast) + ggtitle("Forecast: NSW Lambs") +
  autoplot(wealth_forecast) + ggtitle("Forecast: Household Wealth") +
  autoplot(takeaway_forecast) + ggtitle("Forecast: Takeaway Food Turnover")















# Load necessary libraries
library(tsibble)
library(tsibbledata)
library(fpp3)
library(fpp2)
library(dplyr)
library(lubridate)
library(patchwork)
library(ggplot2)

# Load dataset and filter for Facebook stock price
fb_data <- gafa_stock %>%
  filter(Symbol == "FB") %>%
  select(Date, Close) %>%
  mutate(Day = row_number()) %>% update_tsibble(index=Day, regular=TRUE)

# 1. Produce a time plot of the Facebook stock price
fb_data %>%
  autoplot(Close) +
  ggtitle("Facebook Stock Price Over Time") +
  ylab("Stock Price (USD)") +
  xlab("Date")




# 2. Produce forecasts using the drift method and plot them
fb_drift_forecast <- fb_data %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 90) # Forecast for the next 90 days

autoplot(fb_data, Close) +
  autolayer(fb_drift_forecast, level = NULL, color = "blue") +
  ggtitle("Forecasting Facebook Stock Price using Drift Method") +
  ylab("Stock Price (USD)") +
  xlab("Date")



# 3. Show that the forecasts are identical to extending the line drawn 
# between the first and last observations
fb_start <- first(fb_data$Close)
fb_end <- last(fb_data$Close)
fb_days <- as.numeric(difftime(last(fb_data$Date), first(fb_data$Date), units = "days"))

# Compute the slope (drift rate)
drift_slope <- (fb_end - fb_start) / fb_days

# Extend the line
fb_data %>%
  ggplot(aes(x = Date, y = Close)) +
  geom_line() +
  geom_abline(intercept = fb_start, slope = drift_slope, color = "red", linetype = "dashed") +
  ggtitle("Drift Forecast Matches the Extended Line") +
  ylab("Stock Price (USD)") +
  xlab("Date")

# 4. Try using other benchmark functions for forecasting
fb_naive_forecast <- fb_data %>%
  model(NAIVE(Close)) %>%
  forecast(h = 90)

fb_snaive_forecast <- fb_data %>%
  model(SNAIVE(Close)) %>%
  forecast(h = 90)

# Plot comparisons
autoplot(fb_data, Close) +
  autolayer(fb_drift_forecast, level = NULL, color = "blue", linetype = "solid") +
  autolayer(fb_naive_forecast, level = NULL, color = "red", linetype = "dashed") +
  autolayer(fb_snaive_forecast, level = NULL, color = "green", linetype = "dotted") +
  ggtitle("Comparison of Forecasting Methods for Facebook Stock Price") +
  ylab("Stock Price (USD)") +
  xlab("Date") +
  scale_color_manual(
    values = c("blue" = "Drift", "red" = "Naïve", "green" = "Seasonal Naïve")
  )

# 5. Which method is best and why?
# - Drift method assumes stock prices continue moving based on past trends. It is often reasonable for stocks showing a trend.
# - Naïve method assumes the last observed price continues indefinitely. It is reasonable when stock prices fluctuate around a stable level.
# - Seasonal Naïve assumes patterns repeat seasonally, but stock prices often don’t follow clear seasonality.

# Best choice: The **Drift Method** is often preferable for stock prices because they typically follow a long-term trend rather than staying constant or seasonal.



# Extract data of interest
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
# Define and estimate a model
fit <- recent_production |> model(SNAIVE(Beer))
# Look at the residuals
fit |> gg_tsresiduals()
# Look a some forecasts
fit |> forecast() |> autoplot(recent_production)
