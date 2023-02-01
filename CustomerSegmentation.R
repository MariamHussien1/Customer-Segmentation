#Installing and reading packages
install.packages("ggplot2")
install.packages("corrplot")
install.packages("cluster")
install.packages("ClusterR")
install.packages("factoextra")
install.packages("NbClust")
install.packages("dbscan")
install.packages("fpc")
install.packages("tidyverse")
install.packages("fdm2id")
install.packages("flexclust")
library(ggplot2)
library(corrplot)
library(cluster)
library(ClusterR)
library(factoextra)
library(NbClust)
library(dbscan)
library(fpc)
library(tidyverse)
library(fdm2id)
library(flexclust)

########################################################################################################

#reading csv file
CustomerSegmentation <- read_csv("CustomerSegmentation.csv")

#Renaming columns
names(CustomerSegmentation)[names(CustomerSegmentation) == "Annual Income (k$)"] <- "Annual.Income"
names(CustomerSegmentation)[names(CustomerSegmentation) == "Spending Score (1-100)"] <- "Spending.Score"

CustomerSegmentation$Annual.Income<- CustomerSegmentation$Annual.Income*1000

#making sure data doesn't contain Nulls
sum(is.na(CustomerSegmentation$Age))
sum(is.na(CustomerSegmentation$Annual.Income))
sum(is.na(CustomerSegmentation$Gender))
sum(is.na(CustomerSegmentation$Spending.Score))
#Removing customer ID column
CustomerSegmentation <- CustomerSegmentation[,-c(1)]

#Encoding categorical data
CustomerSegmentationNew <- CustomerSegmentation
CustomerSegmentationNew$Gender <- factor(CustomerSegmentationNew$Gender, levels = c('Male','Female'), labels = c(1,2))
CustomerSegmentationNew$Gender <- strtoi(CustomerSegmentationNew$Gender)
print(is.numeric(CustomerSegmentationNew$Age))

#print first 6 rows of dataset
head(CustomerSegmentation)
head(CustomerSegmentationNew)

########################################################################################################

#Some statistics
print("Summary of Spending Score")
summary(CustomerSegmentation$Spending.Score)
cat("Variance : ",var(CustomerSegmentation$Spending.Score))
print("Summary of Age")
summary(CustomerSegmentation$Age)
cat("Variance : ",var(CustomerSegmentation$Age))
print("Summary of Annual Income")
summary(CustomerSegmentation$Annual.Income)
cat("Variance : ",var(CustomerSegmentation$Annual.Income))
print("Summary of Annual Gender")
summary(factor(CustomerSegmentation$Gender))

########################################################################################################

#Data Visualization

#First Boxplot to visualize outliers in Spending Score
ggplot(CustomerSegmentation) +
  aes(x = "", y = Annual.Income) +
  geom_boxplot(fill = "#D2F3DC")

#Second Barplot of Gender
ggplot(CustomerSegmentation,aes(Gender,fill=Gender))+geom_bar(alpha=0.5)+ggtitle("Box plot of Gender")+theme(plot.title = element_text(hjust = 0.5))

#Third some Histograms
#Histogram to Show Count of Age
hist(CustomerSegmentation$Age,
     main="Histogram to Show Count of Age",
     col="#F9EAE5",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)

#Histogram for Spending Score
hist(CustomerSegmentation$Spending.Score,
     main="Histogram for Spending Score",
     col="#E6F7F7",
     xlab="Spending Score",
     ylab="Frequency",
     labels=TRUE)

#Histogram for Annual Income
hist(CustomerSegmentation$Annual.Income,
     col="#DBEBFF",
     main="Histogram for Annual Income",
     xlab="Annual Income",
     ylab="Frequency",
     labels=TRUE)

#Visualization Gender and Spending Score
ggplot(CustomerSegmentation,aes(x= Spending.Score,fill=Gender))+geom_histogram(binwidth = 3,alpha=0.5)+ggtitle("Histogram for Spending score and Gender")+theme(plot.title = element_text(hjust = 0.5))

#Finally Correlation
m <- cor(CustomerSegmentationNew)
corrplot(m, method="color",title = "Correlation Matrix",mar=c(0,0,1,0))

########################################################################################################

#Splitting Dataset into train and test
sample <- sample(c(TRUE, FALSE), nrow(CustomerSegmentationNew), replace=TRUE, prob=c(0.85,0.15))
Custtrain <- CustomerSegmentationNew[sample, ]
Custtest <- CustomerSegmentationNew[!sample, ]

#Number of Train and Test data
print("Count of Train dataset")
count(Custtrain)
print("Count of Test dataset")
count(Custtest)

#Scaling Data
Custtrain <- scale(Custtrain)
Custtest <- scale(Custtest)
CustomerSegmentationNew <- scale(CustomerSegmentationNew)

########################################################################################################

#Getting number of clusters
fviz_nbclust(Custtrain, kmeans, method = "wss")+geom_vline(xintercept = 5, linetype = 2)

#K-means
kmeans1<-kmeans(Custtrain, 5)
fviz_cluster(kmeans1 ,data = Custtrain,geom="point",main = "K-means")
table(kmeans1$cluster)
predict(kmeans1, Custtest)

#k-means without Gender
kmeans2<-kmeans(Custtrain[,2:4], 5)
fviz_cluster(kmeans2, data = Custtrain[,2:4],geom="point",main = "K-means without Gender")
table(kmeans2$cluster)
predict(kmeans2, Custtest[,2:4])

########################################################################################################

#Getting number of clusters
fviz_nbclust(CustomerSegmentationNew, pam, method = "wss")+geom_vline(xintercept = 4, linetype = 2)

#K-Medoids
kmedoid1 <- pam(CustomerSegmentationNew, 4)
fviz_cluster(kmedoid1 ,data = CustomerSegmentationNew,geom="point", main = "K-Medoids")
table(kmedoid1$cluster)

#K-Medoids without Gender
kmedoid2 <- pam(CustomerSegmentationNew[,2:4], 4)
fviz_cluster(kmedoid2 ,data = CustomerSegmentationNew, geom="point", main = "K-Medoids without Gender")
table(kmedoid2$cluster)

########################################################################################################

#Agglomerative Clustering
agglomerative1  = agnes(x=CustomerSegmentationNew, diss = FALSE, stand = TRUE, method = "complete")
agglomerativeplot1 = as.dendrogram(agglomerative1)
plot(agglomerativeplot1)

#Agglomerative Clustering without Gender
agglomerative  = agnes(x=CustomerSegmentationNew[,2:4], diss = FALSE, stand = TRUE, method = "complete")
agglomerativeplot = as.dendrogram(agglomerative)
plot(agglomerativeplot)

########################################################################################################

#kNNdistplot(CustomerSegmentationNew[,2:4], k=3) + abline(h = 0.55, lty = 2)
#d <- dbscan::dbscan(CustomerSegmentationNew[,2:4], eps = 0.55, MinPts = 6)
#fviz_cluster(d, CustomerSegmentationNew[,2:4],geom = "point",main = "DBScan plot")

