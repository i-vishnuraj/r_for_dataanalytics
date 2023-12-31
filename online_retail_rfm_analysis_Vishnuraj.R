### read the data ### Submitted by Vishnuraj
setwd('D:/F/Learnings/IIMKAABI/R Material/R Dataset')
library(readxl)
library(dplyr)
library(rfm)
library(lubridate)

data <- read.csv("Online Retail.csv", header=T)
str(data)

summary(data)

#Check for NA values in data
sum(is.na(data))

#Filter NA from CustomerID
filtered_data <- data %>% filter(!is.na(CustomerID)) %>% mutate(TotalSale = Quantity * UnitPrice)

#Create new column to capture the type of transaction if its Actual or Return
filtered_data$TypeOfTrans <- factor (ifelse(filtered_data$TotalSale <= 0, "Return", "Actual"))

#Keep only the Actual Sales records for rfm analysis
filtered_data <- filtered_data %>% filter(TypeOfTrans == "Actual")

#Convert Invoice Date to date format
filtered_data$order_date <-parse_date_time(filtered_data$InvoiceDate, orders= c("%d-%m-%Y", "%m-%d-%y"))
summary(filtered_data)
str(filtered_data)

#Select required columns from data
filtered_data <- filtered_data[,c(1,7,9,11)]
filtered_data$order_date <- as.Date(filtered_data$order_date)
#Since the data is of Order type we use rfm_table_order
analysis_date <- as.Date("2011-12-11")

recency_data <- filtered_data %>% arrange(CustomerID, order_date) %>%
  group_by(CustomerID) %>% summarize(lastTransdate = max(order_date))

recency_data$recency <- as.numeric(analysis_date - recency_data$lastTransdate,  units="days")

frequency_data <- filtered_data %>%  group_by(CustomerID) %>% 
  summarize(frequency = n())

monetary_data <- filtered_data %>%  group_by(CustomerID) %>% 
  summarize(monetary = sum(TotalSale))

# Getting recency score quantile
percent_rec_20 = quantile(recency_data$recency, probs = .2)
percent_rec_40 = quantile(recency_data$recency, probs = .4)
percent_rec_60 = quantile(recency_data$recency, probs = .6)
percent_rec_80 = quantile(recency_data$recency, probs = .8)

recency_data$rscore <- cut(recency_data$recency, 
                     breaks=c(0,percent_rec_20,percent_rec_40, percent_rec_60,percent_rec_80, Inf), 
                     labels=c(5,4,3,2,1),
                     include.lowest = TRUE)


# Getting frequency score quantile
percent_freq_20 = quantile(frequency_data$frequency, probs = .2)
percent_freq_40 = quantile(frequency_data$frequency, probs = .4)
percent_freq_60 = quantile(frequency_data$frequency, probs = .6)
percent_freq_80 = quantile(frequency_data$frequency, probs = .8)

frequency_data$fscore <- cut(frequency_data$frequency, 
                          breaks=c(0,percent_freq_20,percent_freq_40, percent_freq_60,percent_freq_80, Inf), 
                          labels=c(1,2,3,4,5),
                          include.lowest = TRUE)

# Getting monetary score quantile
percent_mont_20 = quantile(monetary_data$monetary, probs = .2)
percent_mont_40 = quantile(monetary_data$monetary, probs = .4)
percent_mont_60 = quantile(monetary_data$monetary, probs = .6)
percent_mont_80 = quantile(monetary_data$monetary, probs = .8)

monetary_data$mscore <- cut(monetary_data$monetary, 
                             breaks=c(0,percent_mont_20,percent_mont_40, percent_mont_60,percent_mont_80, Inf), 
                             labels=c(1,2,3,4,5),
                             include.lowest = TRUE)

recency_data  <- as.data.frame(recency_data)
frequency_data  <- as.data.frame(frequency_data)
monetary_data  <- as.data.frame(monetary_data)

customer_rfm_data <- recency_data %>%
  left_join(frequency_data, by='CustomerID') %>%
  left_join(monetary_data, by='CustomerID')

customer_rfm_data$rscore <- as.numeric(as.character(customer_rfm_data$rscore))
customer_rfm_data$fscore <- as.numeric(as.character(customer_rfm_data$fscore))
customer_rfm_data$mscore <- as.numeric(as.character(customer_rfm_data$mscore))

customer_rfm_data$rfmscore <- paste0(customer_rfm_data$rscore, 
                                    customer_rfm_data$fscore, 
                                    customer_rfm_data$mscore)

typeof(customer_rfm_data)

write.csv(customer_rfm_data, "online_retail_rfm.csv")

?rfm_segment



customer_rfm_data$segment <- factor (ifelse(between(customer_rfm_data$rscore, 4, 5) & between(customer_rfm_data$fscore, 4, 5) & between(customer_rfm_data$mscore, 4, 5), "High Value", 
                                    ifelse(between(customer_rfm_data$rscore, 3, 5) & between(customer_rfm_data$fscore, 4, 5) & between(customer_rfm_data$mscore, 3, 5), "Loyal", 
                                    ifelse(between(customer_rfm_data$rscore, 3, 5) & between(customer_rfm_data$fscore, 1, 5) & between(customer_rfm_data$mscore, 3, 5), "Likely Loyal", 
                                    ifelse(between(customer_rfm_data$rscore, 3, 5) & between(customer_rfm_data$fscore, 2, 5) & between(customer_rfm_data$mscore, 1, 3), "Promising", 
                                    ifelse(between(customer_rfm_data$rscore, 4, 5) & between(customer_rfm_data$fscore, 1, 3) & between(customer_rfm_data$mscore, 1, 3), "New", 
                                    ifelse(between(customer_rfm_data$rscore, 1, 3) & between(customer_rfm_data$fscore, 1, 3) & between(customer_rfm_data$mscore, 4, 5), "Can't Lose", 
                                    ifelse(between(customer_rfm_data$rscore, 1, 2) & between(customer_rfm_data$fscore, 2, 4) & between(customer_rfm_data$mscore, 1, 3), "Less Frequent Now", 
                                    ifelse(between(customer_rfm_data$rscore, 3, 5) & between(customer_rfm_data$fscore, 1, 2) & between(customer_rfm_data$mscore, 1, 3), "Low Value", 
                                    ifelse(between(customer_rfm_data$rscore, 1, 2) & between(customer_rfm_data$fscore, 1, 2) & between(customer_rfm_data$mscore, 1, 3), "Churned", 
                                    ifelse(between(customer_rfm_data$rscore, 1, 2) & between(customer_rfm_data$fscore, 3, 5) & between(customer_rfm_data$mscore, 2, 5), "Require Assistance", 
                                                  "Others")))))))))))



segment_summary <- customer_rfm_data %>% count(segment)%>% rename (frequency = n)%>%
  mutate(percentage = (frequency)/sum(frequency) *100) %>% arrange (desc(percentage))

write.csv(customer_rfm_data, "online_retail_rfm_division.csv")

segment_group <- customer_rfm_data %>% group_by(segment)%>% summarise(Avg_R = mean(recency),
                                                           Avg_F = mean(frequency),
                                                           Avg_M = mean(monetary))



