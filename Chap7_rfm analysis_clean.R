###################
# RFM Analysis 	#
###################

install.packages("rfm")
library(tidyverse)
library(rfm)

## Read in RFM data
setwd("/Users/Sungjin/Dropbox (UH)/Marketing-Analytics-Lectures---2024-Spring/Chapter Examples")
rfm <- read_csv("Chapter 7/retail_rfm.csv")

## How many levels for each
groups <- 5 ## This will use quintiles to sort and give 125 total groups

## Run RFM Analysis with Independent Sort
rfm$recency_score_indep <- ntile(rfm$recency_days*-1, groups)
rfm$frequency_score_indep <- ntile(rfm$number_of_orders, groups)
rfm$monetary_score_indep <- ntile(rfm$revenue, groups)
rfm$rfm_score_indep <- paste(rfm$recency_score_indep*100 + rfm$frequency_score_indep * 10 + rfm$monetary_score_indep)

class(rfm$recency_score_indep)

## Making RFM table
tab = 
rfm %>% group_by(rfm_score_indep) %>% 
  summarise(segment_size = n(),
            response_rate = mean(purchase)*100)

rfm %>% group_by(rfm_score_indep) %>% 
  summarise(segment_size = n(),
            response_rate = mean(purchase)*100) %>%
  arrange(desc(response_rate))

## Visualizing the results
ggplot(data = tab, aes(x = rfm_score_indep, y = response_rate)) +
  geom_bar(stat="identity")+
  ggtitle("Response Rates across RFM Groups") +
  xlab("(Independent) RFM Scores") + ylab("Response Rates") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"))



glimpse(rfm)
rfm %>% select(customer_id, recency_days, number_of_orders,revenue, rfm_score_indep) %>% head()

##Using rfm package
package_results = 
rfm %>% rfm_table_customer(customer_id = customer_id ,
                           n_transactions = number_of_orders,
                           recency_days = recency_days,
                           total_revenue = revenue)

package_results
rfm_heatmap(package_results)
rfm_histograms(package_results)

## Run RFM Analysis with Sequential Sort
rfm$recency_score_seq <- ntile(rfm$recency_days*-1, groups)

r_groups <- NULL; rf_groups <- NULL; temp <- NULL ## Initialize empty matrices
for (r in 1:groups) {
  r_groups[[r]] <- filter(rfm, rfm$recency_score_seq == r)
  r_groups[[r]]$frequency_score_seq <- ntile(r_groups[[r]]$number_of_orders, groups)
  for (m in 1:groups) {
    rf_groups[[m]] <- filter(r_groups[[r]], r_groups[[r]]$frequency_score_seq == m)
    rf_groups[[m]]$monetary_score_seq <- ntile(rf_groups[[m]]$revenue, groups)
    temp <- bind_rows(temp, rf_groups[[m]])
  }	
}

rfm_result <- temp[order(temp$customer_id),]
rfm_result$rfm_score_seq <- paste(rfm_result$recency_score_seq*100 + rfm_result$frequency_score_seq * 10 + rfm_result$monetary_score_seq)
glimpse(rfm_result)

tab_seq = 
rfm_result %>% group_by(rfm_score_seq) %>% 
  summarise(segment_size = n(),
            response_rate = mean(purchase)*100)
tab_seq

## Sorting out whom to target
tab_target = 
tab %>% filter(response_rate > 15.63) 
tab_target %>% summarise(target_customer_n = sum(segment_size),
                         target_group_n = n_distinct(rfm_score_indep))
sum(tab$segment_size)
