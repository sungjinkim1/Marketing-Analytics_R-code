######################################
## Perceptual and Preference Mapping #
######################################

library(readr)
library(tidyverse)
set.seed(1)
setwd("your wd")
perception <- read_csv("Use the import Dataset tab to find the perceptions.csv")

#setwd("/Users/Sungjin/Dropbox (UH)/Marketing-Analytics-Lectures---2024-Spring/Chapter Examples")
#perception <- read_csv("Chapter 5/perceptions.csv")
glimpse(perception)
perception = column_to_rownames(.data = perception, var = "Brand") #Need to change first column as the row name
perception
pca_result <- prcomp(perception, scale = TRUE)

names(pca_result)
# means
pca_result$center
# standard deviations
pca_result$scale

pca_result$rotation
biplot(pca_result)
