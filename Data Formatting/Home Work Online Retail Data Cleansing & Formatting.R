# Title       : Data Cleansing & Formatting.R
# Author      : Syifa Humaira
# Date        : August 22, 2019
# Description : This is an online retail script and formatting process with RFM method
# Objective   : To format data (after cleaning data)
# Data source : https://archive.ics.uci.edu/ml/datasets/online+retail 
#               This is a transnational data set which contains all the transactions occurring between 01/12/2010 and 09/12/2011 for a UK-based and registered non-store online retail.The company mainly sells unique all-occasion gifts. Many customers of the company are wholesalers.

****************************************************************************************************************************************************************************************************************************************************************************************
# Load Library

library(readxl)
library(lubridate)
library(DataExplorer)
library(dplyr)


#Import data excel

Online_Retail <- read_excel("E:/Data MBA IYKRA/Online-Retail.xlsx")


#Check missing data

plot_missing(Online_Retail)


#Summary of the data

summary(Online_Retail)


#Preview of the first 6 rows and last 6 rows of data

head(Online_Retail)
tail(Online_Retail)


#Check Varible data

str(Online_Retail)


#Visualize Data Structure

plot_str(Online_Retail)


#Drop missing the value in CustomerID

Online_Retail_drop <- Online_Retail [!is.na(Online_Retail$CustomerID),]


#View Data after Drop the missing the value

View(Online_Retail_drop)


#Summary of Data after Drop missing the value

summary(Online_Retail_drop)


#Check missing Data after Drop missing the value

plot_missing(Online_Retail_drop)


#Visualize bar graph of the Country

plot_bar(Online_Retail_drop$Country)


#Change the name of Country using gsub

Online_Retail_drop$Country <- gsub('United Kingdom', 'Inggris', Online_Retail_drop$Country)


#Checkin the Country after change the name with visualize bar graph

plot_bar(Online_Retail_drop)


#Now we're going to make a new table :
# CustomerID | Recency | Frequency | Monetary
# Recency   : Days count from 2011-12-31 to the last transaction made (in days)
# Frequency : Transaction count in the last 6 months
# Monetary  : Money spent for transaction by distinct CustomerID (in dollars)


frequency <- Online_Retail_drop %>% group_by(CustomerID) %>% summarise(frequency = n_distinct(InvoiceNo))


monetary <- Online_Retail_drop %>% group_by(CustomerID) %>% summarise(monetary=sum(UnitPrice*Quantity))


recency <- Online_Retail_drop %>% group_by(CustomerID) %>% arrange(desc(InvoiceDate)) %>%   filter(row_number()==1) %>% mutate(recency = as.numeric(as.duration(interval(InvoiceDate,ymd("2011-12-31"))))/86400) %>% select(CustomerID, recency)


# Make the new table by joining the 3 new variable

df_rfm <- recency %>% left_join(frequency,by="CustomerID") %>% left_join(monetary,by="CustomerID")


#Summaries data RFM

summary(df_rfm)

# Make a csv file from the table

write.csv(df_rfm,"Online_Retail_RFM.csv")


