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

datasummary( as.factor(sex) * (age + earnwke + grade92) ~ Mean + SD + Min + Max + Median + P95 + N , data = df )


# Unconditional gender gap
reg1 <- feols( earnwke ~ sex , data = df )
reg1

ggplot(data = df, aes(x = sex, y = earnwke)) +
  geom_point()+ 
  geom_smooth(method="lm")+
  scale_x_continuous(limits=c(1, 2),     breaks= seq(1, 2, by=1))

## gender gap + education level
#checking unique education values

df %>% select(grade92) %>% unique() %>% arrange(grade92)

# Adding new column with education names

df <- df %>% 
  mutate(education_lvl = case_when(grade92 == 34 ~ '7th or 8th',
                                   grade92 == 37 ~ '11th',
                                   grade92 == 38 ~ '12th grade NO DIPLOMA',
                                   grade92 == 39 ~ 'High school graduate, diploma or GED',
                                   grade92 == 40 ~ 'Some college but no degree',
                                   grade92 == 41 ~ 'Associate degree -- occupational/vocational',
                                   grade92 == 42 ~ 'Associate degree -- academic program',
                                   grade92 == 43 ~ 'Bachelors degree (e.g. BA,AB,BS)',
                                   grade92 == 44 ~ 'Masters degree (e.g. MA,MS,MEng,Med,MSW,MBA)',
                                   grade92 == 45 ~ 'Professional school deg. (e.g. MD,DDS,DVM,LLB,JD)',
                                   grade92 == 46 ~ 'Doctorate degree (e.g. PhD, EdD)'))

reg2 <- feols( earnwke ~ sex + grade92 , data = df )
reg2









