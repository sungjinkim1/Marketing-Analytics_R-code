##################
# Bass Diffusion #
##################
setwd("/Users/Sungjin/Dropbox (UH)/Marketing-Analytics-Lectures---2024-Spring")

## Load Packages and Set Seed1
library(tidyverse)
set.seed(1)

## Read in the sales data
sales <- read_csv("Chapter Examples/Chapter 13/sales_data.csv")
glimpse(sales)


## Create cumulative sales and lag cumulative sales variables
sales <- sales %>%
  mutate(cumsales = cumsum(sales),
         cumsales2 = cumsales^2,
         cumsales_1 = lag(cumsales),
         cumsales2_1 = lag(cumsales2))
# lag() generates missing values for the first row => change them to zero
sales <- sales %>% mutate_all(~replace(., is.na(.), 0))

## Run the Bass Diffusion Model regression
bass <- lm(sales ~ cumsales_1 + cumsales2_1, data = sales)
summary(bass)

## Determine m, p, and q
a <- bass$coeff[1]  ## a: Intercept
b <- bass$coeff[2]  ## b: Coefficient on cumsales_1
c <- bass$coeff[3]  ## c: Coefficient on cumsales2_1

M1 <- (-b+sqrt(b^2-4*a*c))/(2*c)
M2 <- (-b-sqrt(b^2-4*a*c))/(2*c)
M <- max(M1,M2); names(M) <- "M"
p <- a/M; names(p) <- "p"
q <- b+p; names(q) <- "q"
print(M)
print(p)
print(q)

## Forecast the sales
periods <- 50
t <- seq(1, periods)
forecasts <- data.frame(t=1:periods) # define time period first
psales <- double(periods); pcumsales <- double(periods+1)
psales
pcumsales
# define null values for predicted sales and predicted cumulative sales
for (i in 1:periods){ #for each period from 1:50
  psales[i] <- p*M+(q-p)*pcumsales[i]-(q/M)*pcumsales[i]^2 # calculate this and store psales
  pcumsales[i+1] <- pcumsales[i]+psales[i] # calculate the cumulative sales
}
psales
pcumsales
forecasts$psales <- psales # store this into forecasts
forecasts$pcumsales <- pcumsales[-1]  ## Remove the first value which is 0
forecasts

#Comparison between actual sales and forecasts
glimpse(sales)
glimpse(forecasts)
sales_combine = sales %>% 
  select(t=period,
         sales = sales,
         cumsales = cumsales) %>%
  mutate(type = "Actual Sales")
sales_combine

forecasts_combine = forecasts %>% 
  select(t = t,
         sales = psales,
         cumsales = pcumsales) %>%
  mutate(type = "Predicted Sales")
forecasts_combine

combined_data <- bind_rows(sales_combine, forecasts_combine)
combined_data

#Visualize the sales
ggplot(combined_data, aes(x = t, y = sales, color = type)) +
  geom_line() +
  labs(title = "Actual Sales vs. Predicted Comparison", x = "Time", y = "Sales")+
  theme(legend.position = "bottom")

#Visualize the cumsales

ggplot(combined_data, aes(x = t, y = cumsales, color = type)) +
  geom_line() +
  labs(title = "Actual Sales vs. Predicted Comparison", x = "Time", y = "Sales")+
  theme(legend.position = "bottom")

