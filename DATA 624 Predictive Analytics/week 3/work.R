aus_pop <- global_economy %>% filter(Country=='Australia') %>% select(Population)
aus_pop %>%  model(RW(Population ~ drift())) %>% forecast(h=15) %>% autoplot(aus_pop)

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

aus_pop %>%  model(RW(Population ~ drift())) %>% forecast(h=15) %>% autoplot(aus_pop) + ggtitle("Forecast: Australian Population") +
  autoplot(bricks_forecast) + ggtitle("Forecast: Bricks Production") +

autoplot(lambs_forecast) + ggtitle("Forecast: NSW Lambs") +
  autoplot(wealth_forecast) + ggtitle("Forecast: Household Wealth") +
  autoplot(takeaway_forecast) + ggtitle("Forecast: Takeaway Food Turnover")


install.packages('recticulate')
install.packages('keras')
install.packages('tensorflow')
install.packages("pysparklyr") 
