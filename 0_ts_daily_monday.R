
# daily ts
ts_daily <- ts(data = 1, start = c(2013, 1), end = c(2024, 1), frequency = 365) %>% 
  ts_tbl() %>% 
  mutate(time = as.character(time)) %>% 
  mutate(time = substring(time, first = 1, last = 10)) %>% 
  mutate(time = as.Date(time))

# check number of dates per year
ts_daily %>%
  mutate(year = year(time),
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