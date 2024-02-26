#######################
# Logistic Regression #
####################### 
## Load Packages and Set Seed
library(tidyverse)
library(readr)
set.seed(1)

## Set working directory
setwd("/Users/Sungjin/Dropbox (UH)/Marketing-Analytics-Lectures---2024-Spring/Chapter Examples")
## Read in Logistic Regression data
logit <- read_csv("Chapter 8/retail_logit.csv")
str(logit)
summary(logit)

## Transform and Create Data
logit = logit %>% mutate(number_of_orders2 = number_of_orders^2,
                         lnrevenue = log(revenue + 1))

## Run Logistic Regression using GLM
logit_result1 <- glm(formula = purchase ~ lnrevenue + number_of_orders + number_of_orders2 + recency_days + 
                       loyalty_card + married + income, data = logit, family = "binomial")

summary(logit_result1)

logit_result2 <- glm(formula = purchase ~ revenue + number_of_orders + number_of_orders2 + recency_days + 
                       loyalty_card + married + income, data = logit, family = "binomial")
summary(logit_result2)


## Predicted Probability

logit$predicted_prob = predict(object = logit_result1,
                               newdata = logit, 
                               type = "response")
str(logit)

## Predicted Probability of hypothetical situation
new.df = data.frame(lnrevenue=5, number_of_orders=5, number_of_orders2=25,
                    recency_days=10, loyalty_card = 1, married = 0, income=150)
predict(object = logit_result1, newdata = new.df, type = "response")

## Odds Ratio
exp(logit_result1$coefficients)


## Whom to target
logit_target = logit %>% filter(predicted_prob > 0.1563)
logit_target
