# Header ------------------------------------------------------------------

# R Script to prepare a plot for SIX data to highlight lockerungen im Lockdown

# Name: Lars Schoebitz
# Date: 2020-05-08

# libraries ---------------------------------------------------------------

library(tidyverse)
library(statR)

# load data ---------------------------------------------------------------

## raw data
dat_raw <- read_csv("Economy_SIX.csv")

## manipulated data
dat <- read_csv("dba.csv")



# data manipulation -------------------------------------------------------

## define levels for period variable
levels_period = c('Prä-Lockdown',
                  'Übergangsphase (12.-16. März)',
                  'Lockdown',
                  'Tage vor Ostern',
                  'Erste Lockdown-Lockerung',
                  'Zweite Lockdown-Lockerung',
                  'Feier- und Festtage')


## define levels for weekdays
levels_tag = c('Montage','Dienstage',
               'Mittwoche','Donnerstage',
               'Freitage','Samstage','Sonntage')


dat2 <- dat %>% 
  mutate(Periode = factor(Periode, levels = levels_period)) %>% 
  mutate(tag = factor(tag, levels = levels_tag))

# explore data ------------------------------------------------------------

str(dat2)
str(dat_raw)

names(dat2)
names(dat_raw)

## plot complete time series

dat2 %>% 
  
  ggplot(aes(x = date, y = value, color = variable_long)) +
  geom_point() +
  geom_line()



# plot1  ------------------------------------------------------------------

dat2 %>% 
  filter(variable_short == "stat_einkauf") %>% 
  #filter(Periode != "Feier- und Festtage") %>% 
  group_by(tag, Periode) %>% 
  summarise(
    mean_value = mean(value)
  ) %>% 
  
  ## plot
  
  ggplot(aes(x = tag, y = mean_value, fill = Periode)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~Periode, ncol = 1)


# plot2 -------------------------------------------------------------------

dat2 %>% 
  filter(variable_short == "stat_einkauf") %>% 
  #filter(tag == "Montage") %>% 
  #filter(Periode != "Feier- und Festtage") %>% 
  
  ggplot(aes(x = tag, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Periode), size = 2) +
  
  theme_stat() +
  scale_color_manual(values = zhpal$zhdiagonal) +
  theme(legend.position = "right")


# plot 3 ------------------------------------------------------------------

dat2 %>% 
  mutate(woche = factor(woche)) %>% 
  filter(variable_short == "stat_einkauf") %>% 
  filter(tag == "Montage") %>%
  
  
  ggplot(aes(x = woche, y = value, fill = Periode)) +
  geom_col() +
  scale_fill_manual(values = zhpal$zhdiagonal) +
  theme_stat() +
  theme(legend.position = "right")
  



