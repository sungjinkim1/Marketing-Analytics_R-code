##################################################
## Discriminant Analysis and Classification in R #
##################################################

#My version---------------------
## Install Packages (if needed)
#install.packages("MASS")
rm(list=ls())
## Load Packages and Set Seed
library(readr) # reading the dataset
library(MASS) #For LDA function
library(tidyverse) 

set.seed(1)
#You have to choose your own wd
setwd("G:/My Drive/Teaching/Marketing Analytics/Chapter Examples")
seg <- read_csv("segmentation_results.csv")
seg = column_to_rownames(.data = seg, var = "...1") #Need to change first column as the row name
prospect <- read_csv("Chapter 4/retail_classification.csv")

head(seg)
head(prospect)
## Run Discriminant Analysis
## Notice that we used seg data, not prospect data to understand the demographics of existing segments
## In the textbook p 91, you can see that 6 segments are not appropriate because seg 3 is only 0.03 and seg5 is only 0.02
fit <- lda(segment ~ married + own_home + household_size + income + age, data = seg,)

fit$counts #segment sizes
fit$prior #segment sizes
fit$means #group means
fit$scaling #Coefficient estimates
fit ## print the summary statistics of your discriminant analysis

#293/(293+579+56+884+188)

#plot(fit)
## Check which Discriminant Functions are Significant
ldaPred <- predict(fit, seg)
ldaPred$x
ld <- ldaPred$x
anova(lm(ld[,1] ~ seg$segment))
anova(lm(ld[,2] ~ seg$segment))
anova(lm(ld[,3] ~ seg$segment))
anova(lm(ld[,4] ~ seg$segment))

## Check Disciminant Model Fit
pred.seg <- predict(fit)
summary(pred.seg)
head(pred.seg$class)
head(pred.seg$posterior)
head(pred.seg$x)

pred.seg <- predict(fit)$class # this gives us the predicted segment assignments
tseg <- table(seg$segment, pred.seg) #making confusion table
tseg # print table
sum(diag(tseg))/nrow(seg) # print percent correct

## Run classification Using Discriminant Function
pred.prospect <- predict(fit, prospect)
head(pred.prospect$class)
head(pred.prospect$posterior)
head(pred.prospect$x)
pred.prospect <- predict(fit, prospect)$class
head(pred.prospect)
glimpse(pred.prospect)

prospect.table <- table(pred.prospect)
prospect.table # print table


glimpse(seg)

tbl_base = 
  seg %>%  group_by(segment) %>%
  dplyr::select(avg_order_size,
         avg_order_freq,
         crossbuy,
         multichannel,
         per_sale,tenure) %>% 
  summarise_all("mean")

tbl_base = bind_cols(tbl_base, seg_size = fit$prior)
tbl_base


tbl_demographic = 
  seg %>%  group_by(segment) %>%
  dplyr::select(married,
         own_home,
         household_size,
         income,
         age) %>% 
  summarise_all("mean")

tbl_demographic = bind_cols(tbl_demographic, seg_size = fit$prior)
tbl_demographic
## Add Predicted Segment to classification Data
prospect.seg <- cbind(prospect, pred.prospect)
write.csv(prospect.seg, file = "classification_pred.csv") ## Name file classification_pred.csv

