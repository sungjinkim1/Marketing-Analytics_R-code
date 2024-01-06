#Importing data from Excel
library(readxl)
setwd("~/Dropbox (UH)/Marketing-Analytics_R-code")
smartwatch <- read_csv("smartwatch.csv")
View(smartwatch)

#
#Finding the optimal number of clusters through Eigenvalues
#Install package cluster and factoextra. And use the libraries
install.packages("factoextra")
library(factoextra)
fviz_nbclust(smartwatch, kmeans, method = "wss")

#After cluster=3, the additional loss of information is very less, which is the optimal number of clusters

#Finding the optimal number of clusters through K means clustering
k <- kmeans(smartwatch, centers = 3, nstart = 10)
str(k)
#Optimal number ofclusters is 4. Sizes of the cluster are 218, 321 and 461. 

#Getting segment means for K means clustering and saving the output to excel file
segment_means <- k$centers
segment_means <- round(segment_means, digits = 2)
cluster <- c(1: 3)
Final <- data.frame(cluster, segment_means)
write.csv(Final, file = "C:/Users/jisukim2/Downloads/smartwatch Output.csv")

#CLUSTER DENDOGRAM
#Calculating Eucaldian distance
distance <- dist(smartwatch, method = 'euclidean' )

#Cluster Dendogram
plot(hc.a, cex = 0.6) # plot tree
rect.hclust(hc.a, k = 3, border = 2:5)

# Compute hierarchical clustering and cut into 4 clusters
res <- hcut(smartwatch, k = 3, stand = TRUE)

# Visualize
fviz_dend(res, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))
