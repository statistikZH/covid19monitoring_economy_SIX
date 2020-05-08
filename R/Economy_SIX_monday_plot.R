# Header ------------------------------------------------------------------

# R Script to prepare a plot for SIX data to highlight lockerungen im Lockdown

# Name: Lars Schoebitz
# Date: 2020-05-08

# libraries ---------------------------------------------------------------

library(tidyverse)

# load data ---------------------------------------------------------------

## raw data
dat_raw <- read_csv("Economy_SIX.csv")

## manipulated data
dat <- read_csv("dba.csv")

# explore data ------------------------------------------------------------

str(dat)
str(dat_raw)

names(dat)
names(dat_raw)

dat
dat_raw


