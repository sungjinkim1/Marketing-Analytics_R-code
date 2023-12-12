#############################
# Propensity Score Matching #
#############################

setwd("/Users/Sungjin/Dropbox (UH)/Teaching/Marketing Analytics/Chapter Examples")
## Install Packages (if needed)
install.packages("MatchIt")
install.packages("dplyr")
install.packages("AER")

## Load Packages and Set Seed
library(MatchIt)
library(dplyr)
library(AER)
library(readr)
set.seed(1)


## Read in the marketing mix data
psmatch <- read_csv("Chapter 16/retail_psmatch.csv")
glimpse(psmatch)
summary(psmatch)

## Look at pre-matched data
psmatch %>%
  group_by(email) %>%
  summarise(n_customers = n(), 
            mean_purchase_amt = mean(purchase_amt))

## Difference of covariates
psmatch_keep <- c('email', 'revenue', 'number_of_orders', 'number_of_orders2', 'recency_days')
psmatch_cov <- subset(psmatch, select = psmatch_keep)
psmatch_cov %>%
  group_by(email) %>%
  summarise_all(mean)

## Determine Optimal Caliper Size
ps_match <- glm(email ~ revenue + number_of_orders + number_of_orders2 + recency_days, 
	family = binomial(), data = psmatch)
ps_match_df <- data.frame(pr_score = predict(ps_match, type = "response"),
                     email = ps_match$model$email)
0.2*sd(ps_match_df$pr_score)

## Executing a matching algorithm
psmatch_nomiss <- psmatch %>%  
  select(customer, purchase_amt, one_of(psmatch_keep)) %>%
  na.omit()
psmatch_nomiss

mod_match <- matchit(email ~ revenue + number_of_orders + number_of_orders2 + recency_days,
      method = "nearest", ratio = 1, caliper = 0.023, replace = FALSE, data = psmatch_nomiss)
matched_data <- match.data(mod_match)
dim(matched_data)

## Looking at Post-matching means
#options(digits = 2)
matched_data_cov <- subset(matched_data, select = psmatch_keep)
matched_data_cov %>%
    group_by(email) %>%
    summarise_all(mean) 

## Tobit model with PS Match data
tobit_treat <- tobit(purchase_amt ~ email + revenue + number_of_orders + number_of_orders2 + recency_days, left = 0, data = matched_data)
summary(tobit_treat)

## Predict Expected Value
mu <- fitted(tobit_treat)
sigma <- tobit_treat$scale
p0 <- pnorm(mu/sigma)
matched_data$pred_purchase_amt  = pnorm(mu/sigma) * 
  (mu + sigma * dnorm(mu/sigma)/pnorm(mu/sigma) )

matched_data$pred_purchase_amt

## Compare the expected value for the email and no email groups (matched)
matched_data %>%
  group_by(email) %>%
  summarise(mean_pred_purchase_amt = mean(pred_purchase_amt))
22.71-19.20

## Run the Tobit model with the unmatched data
tobit_treat_nomatch <- tobit(purchase_amt ~ email + revenue + number_of_orders + number_of_orders2 + recency_days, left = 0, data = psmatch)
summary(tobit_treat_nomatch)

## Predict the expected value for the unmatched data
mu <- fitted(tobit_treat_nomatch)
sigma <- tobit_treat_nomatch$scale
psmatch$pred_purchase_amt  = pnorm(mu/sigma) * 
  (mu + sigma * dnorm(mu/sigma)/pnorm(mu/sigma) )

## Compare the expected value for the email and no email groups (no match)
psmatch %>%
  group_by(email) %>%
  summarise(mean_pred_purchase_amt = mean(pred_purchase_amt))
