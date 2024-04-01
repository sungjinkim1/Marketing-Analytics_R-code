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

#########################
#Optional: The manual way of doing it
#########################

og_pref = t(pref) # we need the non-transformed (original) data
## Set up attributes and levels as a vector and Estimate the part-worths for each respondent
attrib.vector <- data.frame(unlist(attrib.level,use.names=FALSE))
colnames(attrib.vector) <- c("levels")
part.worths <- NULL

for (i in 1:ncol(og_pref)){
  temp <- caPartUtilities(og_pref[,i], design, attrib.vector)
  ## Pick the baseline case
  ## Adjust coding as needed based on number of attributes and levels
  ## Base Case: Brand CR, Shipping $0, Restock 0%, Retdays 7 days, Price $150
  Base_Brand <- temp[,"CR"]; Base_Ship <- temp[,"$0"]; Base_Restock <- temp[,"0%"]
  Base_Retdays <- temp[,"7 days"]; Base_Price <- temp[,"$150"]
  ## Adjust Intercept
  temp[,"intercept"] <- temp[,"intercept"] - Base_Brand - Base_Ship - Base_Restock - 
    Base_Retdays - Base_Price
  ## Adjust Coefficients
  ## Brand
  L1 <- length(attrib.level$brand) + 1 ## Add 1 for the intercept
  for (j in 2:L1){temp[,j] <- temp[,j] - Base_Brand}
  ## Shipping
  L2 <- length(attrib.level$ship) + L1
  for (k in (L1+1):L2){temp[,k] <- temp[,k] - Base_Ship}
  ## Restock
  L3 <- length(attrib.level$restock) + L2
  for (l in (L2+1):L3){temp[,l] <- temp[,l] - Base_Restock}
  ## Retdays
  L4 <- length(attrib.level$retdays) + L3
  for (m in (L3+1):L4){temp[,m] <- temp[,m] - Base_Retdays}
  ## Price
  L5 <- length(attrib.level$price) + L4
  for (n in (L4+1):L5){temp[,n] <- temp[,n] - Base_Price}
  part.worths <- rbind(part.worths, temp)
}
rownames(part.worths) <- colnames(og_pref)
part.worths # notice that it explicitly used the base category.

## Calculating average attribute importance
avg.importance = caImportance(pref,design)
avg.importance
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
