#DA 2 - Assignment 2


rm(list=ls())

# packages
library(tidyverse)
library(modelsummary)
library(fixest)
library(ggpubr)
library(lspline)
library(mfx)

hotels_europe_price <- read_csv("https://osf.io/p6tyr/download")
hotels_europe_features <- read_csv("https://osf.io/utwjs/download")

data <- left_join(hotels_europe_price, hotels_europe_features, by = "hotel_id")
rm(hotels_europe_price,hotels_europe_features)

data <- data %>% filter(city_actual == 'Berlin')

data$highly_rated <- ifelse(data$rating >= 4, 1, 0)

# P95 <- function(x){ quantile(x,.95,na.rm=T)}
# datasummary( distance + stars + rating ~ Mean + SD + Min + Max + Median + P95 + N , data = data )

summary(data$highly_rated)  #  mean is 0.6, means that 60% of hotels are highly rated

table(data$highly_rated, data$city)

model_formula <- formula( highly_rated ~ distance + stars )

######### Linear probability model ##################

lpm <- feols(model_formula, data = data, vcov = 'hetero')
lpm

pred_lpm <- predict(lpm)

datasummary( pred_lpm ~ min + max + mean + median + sd , 
             data = pred_lpm )

#logit coefficients
logit <- feglm(model_formula, data = data, family = binomial( link = "logit" ))

# predicted logit probabilities
pred_logit <- predict(logit)

#Logit marginal differences
logit_marg <- logitmfx( model_formula, data=data, atmean=FALSE, robust = T )
print(logit_marg)


#probit coefficients
probit <- feglm(model_formula, data = data, family = binomial( link = "probit" ))

# predicted probit probabilities
pred_probit <- predict(probit)

#Probit marginal differences
probit_marg <- probitmfx( model_formula, data=data, atmean=FALSE, robust = T )
print(logit_marg) 

# Comparing predictions from the two models
datasummary(pred_logit + pred_probit~min+P25+Median+Mean+P75+Max,data=data[1:4200,])

#Creating model summary output
etable( lpm, logit, probit, digits=3 , fitstat = c('r2','pr2'))

# Including the marginals:
cm <- c('(Intercept)' = 'Constant')
pmodels <- list('LPM'=lpm, 'logit coeffs'=logit, 'logit marginals'=logit_marg, 'probit coeffs'=probit, 'probit marginals'=probit_marg)

msummary( pmodels ,
          fmt="%.3f",
          gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC|R2|PseudoR2',
          stars=c('*' = .05, '**' = .01),
          coef_rename = cm)

#goodness of fit - brier score
fitstat_register("brier", function(x){mean(x$residual^2)}, "Brier score")
etable( lpm, logit, probit , drop = "factor|lspline|income|exerc",fitstat = ~ brier  )

# Comparing predicted probabilities of logit and probit to LPM
g5<-ggplot(data = data[1:4200,]) +
  geom_point(aes(x=pred_lpm, y=pred_probit, color="Probit"), size=0.7,  shape=16) +
  geom_point(aes(x=pred_lpm, y=pred_logit,  color="Logit"), size=0.7,  shape=16) +
  geom_line(aes(x=pred_lpm, y=pred_lpm,    color="45 degree line"), size=0.7) +
  labs(x = "Predicted probability of high rating (LPM)", y="Predicted probability")+
  scale_y_continuous(expand = c(0.00,0.0), limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_x_continuous(expand = c(0.00,0.0), limits = c(0,1), breaks = seq(0,1,0.1)) 







