# Header ------------------------------------------------------------------

# R Script to prepare a plot for SIX data to highlight lockerungen im Lockdown

# Name: Lars Schoebitz
# Date: 2020-05-08

# libraries ---------------------------------------------------------------

library(tidyverse)
library(statR)
#library(devtools)
#install_github("statistikZH/statR")

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
levels_period2 = c('Prä-Lockdown',
                   'Übergangsphase (12.-16. März)',
                   'Lockdown',
                   'Montag vor Ostern',
                   'Ostermontag',
                   'Erste Lockdown-Lockerung',
                   'Zweite Lockdown-Lockerung')


dat2 %>% 
  mutate(woche = factor(woche)) %>% 
  filter(variable_short == "stat_einkauf") %>% 
  filter(tag == "Montage") %>%
  mutate(Periode=as.character(Periode))%>%
  mutate(Periode= ifelse(date=='2020-04-13','Ostermontag',Periode))%>%
  mutate(Periode= ifelse(date=='2020-04-06','Montag vor Ostern',Periode))%>%
  mutate(Periode = factor(Periode, levels = levels_period2)) %>% 
  ggplot(aes(x = woche, y = value, fill = Periode)) +
  geom_col() +
  #scale_fill_manual(values = zhpal$zhdiagonal) +
  scale_fill_manual(values = c('Prä-Lockdown'='#6CBD72',
                                'Übergangsphase (12.-16. März)'='#EDDD79',
                                'Lockdown'='#F3A5A7',
                                'Montag vor Ostern'='#E0D6E6',
                                'Ostermontag'='#3FA5A5',
                                'Erste Lockdown-Lockerung'= '#407B9F',
                                'Zweite Lockdown-Lockerung'= '#857091')) +
  labs(y = 'Mio. Fr.\n', 
       x = '\nKalenderwoche', 
       title = "\nVolumen Debitkarteneinsatz stationärer Einkauf CH an Montagen im Jahr 2020", 
       subtitle = "\nKein Online-Handel\n",
       caption = paste0('Daten: SIX BBS AG / Gesellschaftsmonitoring COVID-19 STAT'))+
  theme_stat() +
  theme(legend.position = "bottom",
        legend.title=element_blank()) +
  theme(#panel.grid = element_blank()
    axis.line = element_line(color="black", size = 1),
    axis.text = element_text(color="black", size = 25,face = "bold"),
    axis.title = element_text(color="black", size = 25,face = "bold"),
    axis.ticks.y = element_blank(),
    #panel.background = element_rect(fill='white'),
    #plot.background = element_rect(fill='white'),
    plot.title = element_text(color="black", size = 40,face = "bold"),
    plot.subtitle = element_text(color="black", size = 30),
    #legend.justification=c(0,1), 
    legend.position = 'bottom',
    #legend.background = element_rect(fill=alpha("white", 0)),
    legend.title=element_blank(),
    legend.text= element_text(color="black", size=25),
    #legend.key = element_rect(fill = NA),#
    legend.key.size = unit(3.5,"line"),
    plot.caption= element_text(color="black", size=20))


## save plot
#ggsave(filename = "plots/Economy_SIX_stat_einkauf_monday_plot.png")
  
ggsave('plots/Economy_SIX_stat_einkauf_monday_plot.png',width = 22.86, height =14.128, units = c("cm"),dpi=300,scale=3)



