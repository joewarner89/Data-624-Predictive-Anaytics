library(tsibble)
library(tsibbledata)
library(fpp3)
library(fpp2)
library(dplyr)
library(lubridate)

olympic_running
olympic_running |> distinct(Sex)
PBS
PBS |> filter(ATC2 == "A10")

PBS |> filter(ATC2 == "A10") |>
  select(Month, Concession,Type,Cost)

PBS |> filter(ATC2 =="A10") |> 
  select(Month, Concession,Type, Cost) |> 
  summarize(TotalC = sum(Cost))
# change the Unotes to millions to dollr s
a10<- PBS |> filter(ATC2 == "A10") |> 
  select(Month, Concession,Type,Cost) |> summarize(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC/1e6)

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")
str(prison)
# convert file to tsibble 

prison <- prison |> 
  mutate(Quarter = yearquarter(Date)) |>
  select(-Date) |>
  as_tsibble(key = c(State, Gender, Legal, Indigenous),index = Quarter)
prison


melsyd_economy <- ansett |> 
  filter(Airports == 'MEL-SYD', Class=='Economy') |>
  mutate(Passengers = Passengers/1000) 
autoplot(melsyd_economy, Passengers) + 
  labs(title = 'Ansett airlines economy class', 
       subtitle = 'Melbourne-Sydney',
       y = "Passenger ('000)")

autoplot(a10,Cost) + labs(y= "$ (millions)",
       title = "Australian antidiabetic drug sales")

a10 |> gg_season(Cost, labels = 'both') + 
                   labs(y='$ (million)',
                 title = 'seasonal plot: antidiabetic driug sales')
vic_elec |> gg_season(Demand, period= 'day')+
  theme(legend.position = 'none')+
  labs(y="MWh", title = "electricity demand: Victoria")


vic_elec |> gg_season(Demand, period = 'week') + theme(legend.position = 'none') +
  labs(y='MWh', title ='Electricity deman : Victoria')


vic_elec |> gg_season(Demand, period = "year") +
  labs(y="MWh", title="Electricity demand: Victoria")


a10 |> gg_subseries(Cost) + 
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")

holidays <- tourism |> filter(Purpose == "Holiday") |>
  group_by(State) |> 
  summarise(Trips = sum(Trips))
holidays

autoplot(holidays, Trips)+
  labs(y = 'Overnight trips (000)',
       title = 'Australia domestic Tourism')
gg_season(holidays, Trips) +  
  labs(y='Overnight trip (000)',
       title = "Australian domestic holidays")
holidays |> gg_subseries(Trips) + 
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")


vic_elec |> filter(year(Time) == 2014) |> 
  autoplot(Demand) + labs(y = "GW",
       title = "Half-hourly electricity demand: Victoria")

vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Temperature) +
  labs(
    y = "Degrees Celsius",
    title = "Half-hourly temperatures: Melbourne, Australia"
  )

vic_elec |>
  filter(year(Time) == 2014) |>
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(title="Electricity demand versus Temperature",
       x = "Temperature (degrees Celsius)",
       y = "Electricity demand (GW)")


visitors <- tourism |> group_by(State) |> 
  summarize(Trips = sum(Trips))
visitors <- tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips))
visitors |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Australian domestic tourism",
       y= "Overnight trips ('000)")
  

visitors |> pivot_wider(values_from=Trips,names_from = State) |>
  GGally::ggpairs(columns = 2:9)


# lag plots 

recent_production <- aus_production |> filter(year(Quarter) >= 2000)
recent_production |> 
  gg_lag(Beer, geom = "point") + 
  labs(x = 'lag(Beer, K)')

recent_production |> ACF(Beer, lag_max=9)
recent_production |> ACF(Beer) |> autoplot() |> labs(title = "Australian beer production")

a10 |>
  ACF(Cost, lag_max = 48) |>
  autoplot() +
  labs(title="Australian antidiabetic drug sales")


set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y |> autoplot(wn) + labs(title = "White noise", y = "")


y |>
  ACF(wn) |>
  autoplot() + labs(title = "White noise")
