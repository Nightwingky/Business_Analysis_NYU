url<-"https://raw.githubusercontent.com/Nightwingky/BusinessAnalytics/master/BAData/Cheesemakers_v2.csv"
#url<-"/Users/kunyangque/Documents/NYUTandon/course/Business Analysis/Homework3/Cheesemakers_v2.csv"
data.cheesemakers<-read.csv(url, header = TRUE, stringsAsFactors = TRUE)
head(data.cheesemakers)

summary(data.cheesemakers$Gross.profit)

library(ggplot2)
count<-1: length(data.cheesemakers$Gross.profit)
histogram.data<-data.frame(num = count, gross.profit = data.cheesemakers$Gross.profit)
ggplot(data = histogram.data, mapping = aes(x=count, y=gross.profit)) + geom_bar(stat="identity")
boxplot(data.cheesemakers$Gross.profit,horizontal=TRUE,notch=TRUE)

library(dplyr)
data.group<- group_by(data.cheesemakers, data.cheesemakers$Customer.ID)
help(group_by)
data.group
data.GroupByID<- summarise(data.group,count = n())
data.GroupByID
data.GroupByID<- data.GroupByID[order(data.GroupByID$count,decreasing=T),]
data.GroupByID

summarise(group_by(data.cheesemakers, Customer.ID), mean(Sales.target))


recurring_clients<-subset(data.GroupByID, count>1)
mean(recurring_clients$count)

library(sqldf)
df<-data.cheesemakers
head(df)
result<-sqldf("select customer_id, count(customer_id) as count, avg(sale_amount) from df group by customer_id having count(customer_id)>1")
result

state_record<-sqldf("select [State], count(distinct([Customer.ID])) as customer_num, 
    sum([Number.of.Records]) as records, sum([Sale.amount]) as total_sales from df group by [State];")
state_record
mean_num<-mean(state_record$customer_num)
mean_records<-mean(state_record$records)
mean_sales<-mean(state_record$total_sales)
state_record$customer_num<-(state_record$customer_num - mean_num) / (max(state_record$customer_num) - min(state_record$customer_num))
state_record
state_record$records<-(state_record$records - mean_records) / (max(state_record$records) - min(state_record$records))
state_record$total_sales<-(state_record$total_sales - mean_sales) / (max(state_record$total_sales) - min(state_record$total_sales))
state_record

cor(state_record$customer_num, state_record$records)
cor(state_record$customer_num, state_record$total_sales)

#######################################################################

url<-"/Users/kunyangque/Documents/NYUTandon/course/Business Analysis/Homework3/store_sales.xlsx"
library(readxl)
data.store<-read_excel(url)
data.store
