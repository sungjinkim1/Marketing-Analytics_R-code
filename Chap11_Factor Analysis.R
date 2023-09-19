###################
# Factor Analysis #
###################

## Install Packages (if needed)
install.packages("nFactors")
install.packages("dplyr")
install.packages("GPArotation")
install.packages("gplots")
install.packages("RColorBrewer")

## Load Packages and Set Seed
library(nFactors)
library(dplyr)
library(GPArotation)
library(gplots)
library(RColorBrewer)
set.seed(1)

## Read in Factor Analysis data
retailer_survey <- read.csv(file.choose()) ## Choose retailer_survey.csv file

summary(retailer_survey)
## Look at Descriptive Statistics

retailer.factor.mean = 
retailer_survey %>% group_by(Retailer) %>% summarise(across(where(is.numeric), mean))


## Determine Number of Factors
retailer_factors <- retailer_survey %>% select(-Retailer)
eigen(cor(retailer_factors))$values

## Run Factor Analysis with 4 factors
factanal(retailer_factors, factors = 4)

## Run Factor Analysis with 4 factors and oblique rotation
retailer.fa <- factanal(retailer_factors, factors = 4, rotation="oblimin", scores="Bartlett")
retailer.fa

## Print a Heatmap of Factor Loadings
heatmap.2(retailer.fa$loadings, col=brewer.pal(9, "Reds"), trace="none", key=FALSE, dend="none",
	Colv=FALSE, cexCol = 1, margins=c(12,8), main="Factor Loadings from Survey")

## Aggregate Factor Scores by Retailer
retailer.scores <- data.frame(retailer.fa$scores)
retailer.scores$retailer <- retailer_survey$Retailer
retailer.fa.mean <- aggregate(. ~ retailer, data=retailer.scores, mean)
rownames(retailer.fa.mean) <- retailer.fa.mean[,1]
retailer.fa.mean <- select(retailer.fa.mean, -retailer)
names(retailer.fa.mean) <- c("Innovative", "High Quality", "Loyalty", "Good Value") 
retailer.fa.mean

## Print a Heatmap of Retailer Scores
heatmap.2(as.matrix(retailer.fa.mean), col=brewer.pal(9, "Blues"), trace="none", key=FALSE, dend="none",
	Colv=FALSE, cexCol = 1, margins=c(12,8), main="Factor Score by Retailer")


heatmap.2(as.matrix(retailer.fa.mean), col=brewer.pal(9, "Blues"), 
          trace="none",
          key=FALSE,
          dend="none",
          Colv=FALSE,
          cexCol = 1,
          margins=c(12,8),
          main="Factor Score by Retailer")


