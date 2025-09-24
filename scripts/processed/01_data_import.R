library(here)

wvs7 <- read.dta13(here("data", "raw", "WVS7.dta"))
krg  <- read.csv(here("data", "raw", "dataset_bashur.csv"))
roj  <- read.csv(here("data", "raw", "dataset_rojava.csv"))

