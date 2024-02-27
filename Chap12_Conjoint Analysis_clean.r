#####################
# Conjoint Analysis #
#####################
rm(list = ls())
## Install Packages (if needed)
install.packages("conjoint")

## Load Packages and Set Seed
library(conjoint)
library(tidyverse)
set.seed(1)
setwd("/Users/Sungjin/Dropbox (UH)/Marketing-Analytics-Lectures---2024-Spring/Chapter Examples")
#setwd("G:/My Drive/Teaching/Marketing Analytics/Chapter Examples")
## Set up attributes and levels as a list
attrib.level <- list(brand = c("CR", "Apple", "Samsung", "FitBit"),
                     ship = c("$0", "$10", "$20"), 
                     restock = c("0%", "5%", "10%", "15%"),
                     retdays = c("7 days", "14 days", "21 days"), 
                     price = c("$150", "$200", "$250", "$300"))
attrib.level

## Create the fractional factorial design
experiment <- expand.grid(attrib.level)
design <- caFactorialDesign(data=experiment, type="fractional", cards=30, seed=1)
experiment
design
## Check for correlation in fractional factorial design
head(design)
head(caEncodedDesign(design))
print(cor(caEncodedDesign(design)))

## Read in the survey preference results
pref <- read_csv("Chapter 12/conjoint_preferences.csv") ## Choose the file named conjoint_preferences.csv
class(pref)
pref = t(pref)
pref

## Set up attributes and levels as a vector and Estimate the part-worths for each respondent
attrib.vector <- data.frame(unlist(attrib.level,use.names=FALSE))
colnames(attrib.vector) <- c("levels")
attrib.vector

## Run the conjoint analysis study
caPartUtilities(pref,design,attrib.vector) 
caUtilities(pref, design, attrib.vector)

## Calculating average attribute importance
avg.importance = caImportance(pref,design)
names(avg.importance) <- c("Brand", "Ship", "Restock", "Return", "Price")
avg.importance = data.frame(avg.importance)  %>% rownames_to_column(var = "Names")
avg.importance

avg.importance.fig <- ggplot(avg.importance, aes(x =Names, y = avg.importance)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Bar Chart of Numeric Values", x = "Names", y = "Values") +
  theme_minimal()
avg.importance.fig

## Getting Segmentation
library(fpc)
segments<-caSegmentation(pref,design,c = 3)
print(segments$seg)
plotcluster(segments$util,segments$sclu)
