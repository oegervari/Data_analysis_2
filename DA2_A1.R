# DA2 - A1

rm(list=ls())

# packages
library(tidyverse)
library(ggplot2)
library(fixest)

df <- read_csv("https://raw.githubusercontent.com/oegervari/Data_analysis_2/main/data/morg-2014-emp.csv")

#filtering for occupation and wages higher than 50$ / week
df <- df %>% filter(occ2012 == 800) %>% filter(earnwke > 50) %>% mutate(occupation = 'Accountants and auditors')

# Checking the data
P95 <- function(x){ quantile(x,.95,na.rm=T)}
datasummary( age + sex + earnwke + grade92 ~ Mean + SD + Min + Max + Median + P95 + N , data = df )




















