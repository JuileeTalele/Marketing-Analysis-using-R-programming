
#========================================================================================================
# CODE
#========================================================================================================
# Importing required library

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

# Reading file

setwd("C:/Users/lenovo/Desktop/Praxis/MA")
X <- read_excel("Online Retail Corrected.xlsx")
View(X)

# Formating date col

X$Date <- as.Date(X$InvoiceDate)

# Combining Description columns based on common invoice no and date
# Creating data T having the item purchased,invoice no,date

T <- ddply(X,c("InvoiceNo","Date"),
           function(df1)paste(df1$Description,collapse = ","))

View(T)

# deleting invoice no and date column

T$InvoiceNo <- NULL
T$Date <- NULL

# Renaming the column

colnames(T) <- c("items")

# Writing csv to verify

write.csv(T,"T.csv",quote = FALSE,row.names = FALSE)

# Reading csv in transaction (basket) format

T <- read.transactions("T.csv",format = "basket",sep=",")

summary(T)

#===========================
# Examples
#===========================


#=======================================================================================================


mba.rule1 <- apriori(T,parameter = list(supp=0.01,conf=0.8,maxlen=10),
                    appearance = list(lhs='COFFEE',rhs = "SUGAR"))
inspect(mba.rule1)

# Output : 0.800

#lhs         rhs     support    confidence lift     count
#[1] {COFFEE} => {SUGAR} 0.01501231 0.8008753  53.34792 366



mba.rule2 <- apriori(T,parameter = list(supp=0.01,conf=0.8,maxlen=10),
                    appearance = list(lhs='BACK DOOR',rhs='KEY FOB'))
inspect(mba.rule2)

# Output : 0.010

#lhs            rhs       support    confidence lift     count
#[1] {BACK DOOR} => {KEY FOB} 0.01017227 1          57.90974 248


#========================================================================================================

mba.rule3 <- apriori(T,parameter = list(supp=0.001,conf=0.8,maxlen=10),
                    appearance = list(lhs='METAL',default='rhs'))
inspect(mba.rule3)

mba.rule4 <- apriori(T,parameter = list(supp=0.001,conf=0.8,maxlen=10),
                     appearance = list(rhs='METAL',default='lhs'))
inspect(mba.rule4)

# Output : {WOBBLY CHICKEN,DECORATION,WOBBLY RABBIT}

#lhs                            rhs     support     confidence lift     count
#[1] {WOBBLY CHICKEN}            => {METAL} 0.001271534 1          399.6721 31   
#[2] {WOBBLY RABBIT}             => {METAL} 0.001804758 1          399.6721 44   
#[3] {DECORATION}                => {METAL} 0.002502051 1          399.6721 61   
#[4] {DECORATION,WOBBLY CHICKEN} => {METAL} 0.001271534 1          399.6721 31   
#[5] {DECORATION,WOBBLY RABBIT}  => {METAL} 0.001804758 1          399.6721 44 


#========================================================================================================

mba.rule5 <- apriori(T,parameter = list(supp=0.004,conf=0.8,maxlen=10),
                     appearance = list(lhs='COFFEE',default='rhs'))
inspect(mba.rule5)

mba.rule6 <- apriori(T,parameter = list(supp=0.004,conf=0.8,maxlen=10),
                     appearance = list(rhs='COFFEE',default='lhs'))
inspect(mba.rule6)

# Output : {SUGAR,RED SPOTTY BISCUIT TIN,SET 3 RETROSPOT TEA}

#lhs                                                   rhs      support     confidence lift     count
#[1] {SUGAR}                                            => {COFFEE} 0.015012305 1          53.34792 366  
#[2] {SET 3 RETROSPOT TEA}                              => {COFFEE} 0.015012305 1          53.34792 366  
#[3] {RED SPOTTY BISCUIT TIN,SUGAR}                     => {COFFEE} 0.005701395 1          53.34792 139  
#[4] {RED SPOTTY BISCUIT TIN,SET 3 RETROSPOT TEA}       => {COFFEE} 0.005701395 1          53.34792 139  
#[5] {SET 3 RETROSPOT TEA,SUGAR}                        => {COFFEE} 0.015012305 1          53.34792 366  
#[6] {RED SPOTTY BISCUIT TIN,SET 3 RETROSPOT TEA,SUGAR} => {COFFEE} 0.005701395 1          53.34792 139


#========================================================================================================

mba.rule7 <- apriori(T,parameter = list(supp=0.001,conf=0.8,maxlen=5))

mba.rule7_by_lift <- sort(mba.rule7,by="lift")  # sort by lift

inspect(mba.rule7_by_lift[1:5])

# Output :

#lhs                          rhs                       support     confidence lift     count
#[1] {BILLBOARD FONTS DESIGN}  => {WRAP}                    0.001230517 1.0000     761.8750 30   
#[2] {WRAP}                    => {BILLBOARD FONTS DESIGN}  0.001230517 0.9375     761.8750 30   
#[3] {CHRISTMAS GARLAND STARS} => {TREES}                   0.001353568 1.0000     738.7879 33   
#[4] {TREES}                   => {CHRISTMAS GARLAND STARS} 0.001353568 1.0000     738.7879 33   
#[5] {FUNK MONKEY}             => {ART LIGHTS}              0.001927810 1.0000     518.7234 47  
