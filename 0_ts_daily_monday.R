rm(list = ls())

# packages

library("plotly")

library("vars") # for dynlm
library("dynlm")
library("tseries") # for ADF & KPSS tests

library("forecast") # for auto.arima

library("zoo") # for time series

library("here")
library("readxl")
library("tidyverse")
library("dplyr")
library("tsbox")

library("lubridate")

library("kableExtra")

# panel data
library("plm")

options(warn=-1) # for ON: options(warn=0)



# fed rate ----------------------------------------------------------------


ff <- read_delim(paste0(here::here(), "/FF.csv"),
                 col_names = T, delim = ",") %>% 
  rename(time = 1,
         value = 2) %>% 
  mutate(value = as.double(value),
         id = "fed_rate", .before=1) %>% 
  ts_tbl %>% ts_wide() %>% ts_long

df <- ff %>% 
  mutate(year=year(time),
         week = isoweek(time),
         wd = weekdays(time)) %>% 
  rowid_to_column

df %>% filter(51<week | week<2) %>% print(n=30)

df2 <- df %>% 
  select(-time)


# monday function ---------------------------------------------------------


# daily ts
ts_daily <- ts(data = 1, start = c(2013, 1), end = c(2024, 1), frequency = 365) %>% 
  ts_tbl() %>% 
  mutate(time = as.character(time)) %>% 
  mutate(time = substring(time, first = 1, last = 10)) %>% 
  mutate(time = as.Date(time))

# check number of dates per year
ts_daily %>%
  mutate(year = lubridate::isoyear(time),
         week = lubridate::isoweek(time) ) %>% 
  group_by(year) %>% 
  summarize(N = sum(value))

ts_mon <- ts_daily %>% 
  mutate(wd = weekdays(time),
         week = lubridate::isoweek(time),
         year = lubridate::isoyear(time)) %>% 
  filter(wd == "Monday") %>% 
  select(time, year, week)

ts_mon %>% 
  filter(week<2 | week>51) %>% 
  print(n=30)


# join the two time series ------------------------------------------------

left_join(x = df2, y = ts_mon )
df_time <- left_join(x = df2, y = ts_mon, by = join_by(year, week) )

df_time %>% 
  filter(is.na(time))




