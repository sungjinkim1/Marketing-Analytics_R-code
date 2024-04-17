#################
# Marketing Mix #
#################

setwd("/Users/Sungjin/Dropbox (UH)/Marketing-Analytics-Lectures---2024-Spring/Chapter Examples")
## Install Packages (if needed)
install.packages("tseries")

## Load Packages and Set Seed
library(tidyverse)
library(readr)
library(tseries)
set.seed(1)

## Read in the marketing mix data
mmix <- read.csv("Chapter 15/mmix_data.csv")

## Look at means of variables
summary(mmix)

## Create natural log, lag, and weekday variables
mmix <- mmix %>%
  mutate(
    ln_quantity = log(quantity),
    ln_price = log(price),
    ln_digital_ad = log(digital_ad),
    ln_digital_search = log(digital_search),
    ln_print = log(print + 1),  # Adding 1 to handle cases where print is 0
    ln_tv = log(tv),
    ln_quantity_lag = lag(ln_quantity, default = 0),  # Lagging ln_quantity with a default of 0
    weekdays = weekdays(as.Date(date))
  )
glimpse(mmix)
## Check for unit root
adf.test(mmix$ln_quantity)

## Check for multicollinearity
cor_table <- mmix %>% 
  select(ln_quantity, ln_quantity_lag, ln_price, ln_digital_ad,
         ln_digital_search, ln_print, ln_tv) %>%
  cor() %>%
  round(2)
cor_table

## Combine ln_digital_ad and ln_digital_search
mmix$ln_digital <- log(mmix$digital_ad + mmix$digital_search)
glimpse(mmix)
log(mmix$digital_ad + mmix$digital_search)
log(mmix$digital_ad) + log(mmix$digital_search)

## Run the regression
mmix_reg <- lm(ln_quantity ~ ln_quantity_lag + ln_price + ln_digital + ln_print + 
ln_tv + factor(weekdays), data = mmix)
summary(mmix_reg)

## Created predicted values for actual quantity
mmix$pred_quantity <- exp(predict(mmix_reg))
predict(mmix_reg)
mmix$pred_quantity

ggplot(mmix, aes(x = day)) +
  geom_line(aes(y = quantity, color = "Actual Quantity"), size = 1) +
  geom_line(aes(y = pred_quantity, color = "Predicted Quantity"), size = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Quantity Over Time",
       x = "Date",
       y = "Quantity") +
  scale_color_manual(values = c("Actual Quantity" = "blue", "Predicted Quantity" = "red")) +
  theme_minimal()

