#RFM
# Find the recency of customer : visting frequency of customer lately/recently 

getwd()
setwd("C:\\Users\\lenovo\\Desktop\\Praxis\\MA")
# Load text file into local variable called 'rfm_data'
rfm_data = read.delim(file = 'rfm_data.txt', header = FALSE, sep = '\t', dec = '.')

# Add headers and interpret the last column as a date, extract year of purchase
colnames(rfm_data) = c('customer_id', 'amount', 'purchase_date')
rfm_data$purchase_date = as.Date(rfm_data$purchase_date, "%Y-%m-%d")
rfm_data$days_since       = as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = rfm_data$purchase_date,
                                            units = "days"))

# Display the rfm_data after transformation
head(rfm_data)
summary(rfm_data)

# Compute key marketing indicators using SQL language
library(sqldf)

# Compute recency, frequency, and average purchase amount
rfm_customers = sqldf("SELECT customer_id,
                          MIN(days_since) AS 'recency',
                          COUNT(*) AS 'frequency',
                          AVG(amount) AS 'amount'
                   FROM rfm_data GROUP BY customer_id")

# Explore the rfm_data
head(rfm_customers)
summary(rfm_customers)
hist(rfm_customers$recency)
hist(rfm_customers$frequency)
hist(rfm_customers$amount)
hist(rfm_customers$amount, breaks = 100)


# --- PREPARING AND TRANSFORMING rfm_data ----------------------


# Copy customer rfm_data into new rfm_data frame
new_rfm_data = rfm_customers

# Remove customer id as a variable, store it as row names
head(new_rfm_data)
row.names(new_rfm_data) = new_rfm_data$customer_id
new_rfm_data$customer_id = NULL
head(new_rfm_data)

# Take the log-transform of the amount, and plot
new_rfm_data$amount = log(new_rfm_data$amount)
new_rfm_data$frequency = log(new_rfm_data$frequency)

hist(new_rfm_data$amount)
hist(new_rfm_data$frequency)

# Standardize variables
new_rfm_data = scale(new_rfm_data)
head(new_rfm_data)


# --- RUNNING A HIERARCHICAL SEGMENTATION ------------------


# Compute distance metrics on standardized rfm_data
# This will likely generate an error on most machines
# d = dist(new_rfm_data)

# Take a 10% sample
sample = seq(1, 18417, by = 10)
head(sample)
rfm_customers_sample = rfm_customers[sample, ]
new_rfm_data_sample  = new_rfm_data[sample, ]

# Compute distance metrics on standardized rfm_data
d = dist(new_rfm_data_sample)

# Perform hierarchical clustering on distance metrics
c = hclust(d, method="ward.D2")

# Plot de dendogram
plot(c)

# Cut at 9 segments
members = cutree(c, k = 5)

# Show 30 first rfm_customers, frequency table
members[1:30]
table(members)
View(members)
install.packages("dplyr")
library(dplyr)
# Show profile of each segment
aggregate(rfm_customers_sample[, 2:4], by = list(members), mean)

# Observe the output : Group 6 has a pretty recency, frequency also their 
# amount spend by this group is more

cluster1 <- cbind(rfm_customers_sample, clusterid=members)
View(cluster1)
clust1<- filter(cluster1, clusterid==1)
clust1 <- filter(cluster1, customer_id==260|customer_id==5920)
cluster1 %>% select (customer_id,clusterid) %>% filter(clusterid == 1)

# CODE FOR K MEAN------------------------------------------------------------

set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- new_rfm_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss[]
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


kmm = kmeans(new_rfm_data_sample,9,nstart = 50,iter.max = 15)
cluster1 <- cbind(rfm_customers_sample, kmm$cluster)

kmm$totss
kmm$betweenss
