###################
# Factor Analysis #
###################

## Install Packages (if needed)
install.packages("GPArotation")
install.packages("gplots")
install.packages("RColorBrewer")

## Load Packages and Set Seed
library(tidyverse)
library(GPArotation)
library(gplots)
library(RColorBrewer)
library(readr)
set.seed(1)
setwd("/Users/Sungjin/Dropbox (UH)/Marketing-Analytics-Lectures---2024-Spring/Chapter Examples")

## Read in Factor Analysis data

retailer_survey <- read_csv("Chapter 11/retailer_survey.csv")
str(retailer_survey)

## Look at Descriptive Statistics

retailer.factor.mean = 
  retailer_survey %>% group_by(Retailer) %>% summarise(across(where(is.numeric), mean))
retailer.factor.mean

## Determine Number of Factors
retailer_factors <- retailer_survey %>% select(-Retailer) #need to exclude "Retailer" since it is categorical identifier of retailers
eigen(cor(retailer_factors))$values
retailer_factors
## Run Factor Analysis with 4 factors
retailer.fa = factanal(x = retailer_factors, factors = 4, scores = "Bartlett")
retailer.fa
### Optional###########
## Run Factor Analysis with 4 factors and oblique rotation 
retailer.fa_alternative <- factanal(x = retailer_factors, factors = 4, rotation="oblimin",scores = "Bartlett")
retailer.fa_alternative
?factanal
####################### 

## Print a Heatmap of Factor Loadings
heatmap.2(retailer.fa$loadings, col=brewer.pal(9, "Reds"), trace="none", key=FALSE, dend="none",
	Colv=FALSE, cexCol = 1, margins=c(12,8), main="Factor Loadings from Survey")

# heatmap.2(retailer.fa$loadings, col=brewer.pal(9, "Reds"),  key=FALSE, dend="none",
#           Colv=FALSE, cexCol = 1, margins=c(12,8), main="Factor Loadings from Survey")
# 
# 
# heatmap.2(retailer.fa$loadings, col=brewer.pal(9, "Reds"), dend="none",
#           Colv=FALSE, cexCol = 1, margins=c(12,8), main="Factor Loadings from Survey")
# 
# heatmap.2(retailer.fa$loadings, col=brewer.pal(9, "Reds"),
#           Colv=FALSE, cexCol = 1, margins=c(12,8), main="Factor Loadings from Survey")

## Aggregate Factor Scores by Retailer
retailer.scores <- data.frame(retailer.fa$scores)
retailer.scores
retailer.scores$retailer <- retailer_survey$Retailer
retailer.fa.mean = retailer.scores %>% group_by(retailer) %>% summarise(across(where(is.numeric), mean))
retailer.fa.mean
retailer.fa.mean = column_to_rownames(retailer.fa.mean, var = "retailer") 
retailer.fa.mean
retailer.fa.mean = 
retailer.fa.mean %>% rename(Innovative = Factor1,
                            Quality = Factor2,
                            Loyalty = Factor3,
                            Value = Factor4)

retailer.fa.mean

## Print a Heatmap of Retailer Scores; Feel free to play around the options

heatmap.2(as.matrix(retailer.fa.mean), col=brewer.pal(9, "Blues"), 
          trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1, margins=c(12,8),
          main="Factor Score by Retailer")

