######################################
## Perceptual and Preference Mapping #
######################################

## My version--------------
library(readr)
library(tidyverse)
set.seed(1)
setwd("G:/My Drive/Teaching/Marketing Analytics/Chapter Examples")
perception <- read_csv("Chapter 5/perceptions.csv")
glimpse(perception)
perception = column_to_rownames(.data = perception, var = "Brand") #Need to change first column as the row name

pca_result <- prcomp(perception, scale = TRUE, retx = TRUE)
#pca_result <- prcomp(perception, scale = TRUE)

names(pca_result)
# means
pca_result$center
# standard deviations
pca_result$scale

pca_result$rotation


#pca_result$rotation <- -pca_result$rotation
#pca_result$rotation

#pca_result$x <- - pca_result$x
biplot(pca_result,pc.biplot = T)
head(pca_result$x)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(pca_result,groups = pca_result$)
