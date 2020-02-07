
install.packages("arules")
install.packages("arulesViz")
install.packages("readxl")
install.packages("RColorBrewer")
install.packages("tidyverse")
install.packages("knitr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("plyr")
install.packages("dplyr")

library(RColorBrewer)
library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)

setwd("C:/Users/lenovo/Desktop/Praxis/MA")
Retail_Data <- read_excel('Online Retail Corrected.xlsx')
str(Retail_Data)
View(Retail_Data)

Retail_Data$Date<- as.Date(Retail_Data$InvoiceDate)

?ddply
Transaction_Data <- ddply(Retail_Data,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))
View(Transaction_Data)
Transaction_Data$InvoiceNo <- NULL
#set column Date of dataframe transactionData
Transaction_Data$Date <- NULL
#Rename column to items
colnames(Transaction_Data) <- c("items")
#Show Dataframe transactionData
#iunique(Transaction_Data$items)

write.csv(Transaction_Data,"Transactions.csv", quote = FALSE, row.names = FALSE)
Transactions <- read.transactions('Transactions.csv', format = 'basket', sep=',')

?apriori()
?list()
summary(Transactions)
mba.rules <- apriori(Transactions, parameter = list(supp=0.01, conf=0.8,maxlen=10))

inspect(mba.rules[1:10])
new.mba.rules <- apriori(Transactions, parameter = list(supp=0.01, conf=0.8,maxlen=3))

inspect(new.mba.rules[1:10])
