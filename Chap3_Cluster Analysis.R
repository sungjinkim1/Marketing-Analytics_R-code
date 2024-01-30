rm(list=ls())

install.packages("cluster")    # clustering algorithms
install.packages("factoextra") # clustering algorithms & visualization
install.packages("gridExtra") #tool for showing multiple pictures

library(readr) # reading the dataset
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra) #tool for showing multiple pictures

setwd("your folder") #choose your folder
#setwd("/Users/Sungjin/Dropbox (UH)/Chapter Examples/Chapter 3") #my folder is this

seg <- read_csv("retail_segmentation.csv") 
glimpse(seg)

seg = column_to_rownames(.data = seg, var = "Cust_No")

summary(seg)
class(seg)
seg_base_var = 
  seg %>% select(avg_order_size,
                 avg_order_freq,
                 crossbuy,
                 multichannel,
                 per_sale,tenure) %>% 
  scale() %>% 
  as_tibble()

summary(seg_base_var)


k2 <- kmeans(seg_base_var, centers = 2, nstart = 25)
k2

fviz_cluster(k2, data = seg_base_var)

# seg_base_var %>%
#   as_tibble() %>%
#   mutate(cluster = k2$cluster,
#          state = row.names(seg_base_var)) %>%
#   ggplot(aes(avg_order_size, avg_order_freq, color = factor(cluster), label = state)) +
#   geom_text()

k3 <- kmeans(seg_base_var, centers = 3, nstart = 25)
k4 <- kmeans(seg_base_var, centers = 4, nstart = 25)
k5 <- kmeans(seg_base_var, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = seg_base_var) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = seg_base_var) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = seg_base_var) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = seg_base_var) + ggtitle("k = 5")
ggsave(plot = p4,filename = "kmeans.png")
grid.arrange(p1, p2, p3, p4, nrow = 2)

set.seed(1)

fviz_nbclust(seg_base_var, kmeans, method = "wss")
fviz_nbclust(seg_base_var, kmeans, method = "silhouette")

# Compute k-means clustering with k = 5

final <- kmeans(seg_base_var, 5, nstart = 25)
final
segment = final$cluster
segment
segmentation_result <- bind_cols(seg, segment=segment)
glimpse(segmentation_result)

final$size
final$centers
fviz_cluster(final, data = seg_base_var)

write.csv(x = segmentation_result,file = "segmentation_results.csv",row.names = T)

class(seg_base_var)

tbl = 
seg %>%
  select(avg_order_size,
         avg_order_freq,
         crossbuy,
         multichannel,
         per_sale,tenure) %>% 
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

tbl
bind_cols(tbl,seg_size = final$size)

# Compute with agnes from cluster package
hc <- agnes(seg_base_var, method = "complete")

pltree(hc, cex = 0.6, hang = -1, main = "Dendrogram of Consumers") 

#find the number of clusters
fviz_nbclust(seg_base_var, FUN = hcut, method = "wss")
fviz_nbclust(seg_base_var, FUN = hcut, method = "silhouette")

sub_grp <- cutree(hc, k = 4)
fviz_cluster(list(data = seg_base_var, cluster = sub_grp))

