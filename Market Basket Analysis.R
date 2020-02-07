#MARKET BASKET ANALYSIS
#================================================================================
#DATA
#================================================================================
library(arules)
data("Groceries")
data <- Groceries
data

#Typ of data
class(data)

#Check the summary of the data
summary(Groceries)


#No. of nonempty cells
prod(dim(data))*0.026091


#INSPECT
#================================================================================
#inspecting transactions
inspect(Groceries[1:3])
inspect(Groceries[1000])


?itemFrequencyPlot
itemFrequencyPlot(Groceries, support=0.1) #Minimal support
itemFrequencyPlot(Groceries, topN=10)     #Top products by frequency



#FINDING RULES:
#================================================================================
#Finding Rules with minimum support 0.007 and minimum confidence 0.25
rules1 <- apriori(data=Groceries, parameter=list(supp=0.01,
                                                 conf = 0.15, minlen=2))

#summary of the association rules found out
summary(rules1)
inspect(rules1[1:5])

#Sorting the rules in decreasing order of lift
rules <- sort(rules1, decreasing=TRUE, by="lift")

#Inspect the first 5 rules
inspect(rules[1:5])


#==================
#RULE 2
#==================

#Finding all the rules such that the LHS learn to {Whole Milk} in RHS with a 
#support and confidence threshold is 0.01 and 0.5

rules<-apriori(data=Groceries, parameter=list(supp=0.01,conf = 0.5), 
               appearance = list(default="lhs",rhs="whole milk"))
summary(rules)
inspect(rules)

#Sort the rules by decreasing order ofconfidence
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:10])


