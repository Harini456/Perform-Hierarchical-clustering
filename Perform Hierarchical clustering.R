#NAME: Harini G
#ROLLNO:2048034
#LAB10

#install.packages("tidyverse")
#install.packages("factoextra")
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

head(mtcars)

class(mtcars) #class of the dataset 

typeof(mtcars) #datatype of the dataset

nrow(mtcars) #number of rows/observations in the dataset
ncol(mtcars) #number of columns/features in the dataset
dim(mtcars) #Shape/dimension of the dataset

is.na(mtcars) #checking missing value in the dataset
sum(is.na(mtcars)) #total number of missing values in the dataset

str(mtcars) #Structure of the dataset
summary(mtcars)
sapply(mtcars, class)#displaying the datatype of each column

df <- na.omit(mtcars)#removing missing values if they are present

df <- scale(df)

#It works in a bottom-up manner. 
#each object is initially considered as a single-element cluster (leaf). 
#At each step of the algorithm, the two clusters that are the most similar are combined into a new bigger cluster (nodes). 
#This procedure is iterated until all points are member of just one single big cluster (root). 
#The result is a tree which is plotted using dendrogram.

d <- dist(df, method = "euclidean")# Dissimilarity matrix
d

hc1 <- hclust(d, method = "complete" )# Hierarchical clustering using Complete Linkage
hc1

plot(hc1, cex = 0.6, hang = -1)# Plot the obtained dendrogram


# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac)
#Wards method identifies the strongest clustering structure of the four methods

hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

hc3 <- agnes(df, method = "average")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

hc3 <- agnes(df, method = "single")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

hc3 <- agnes(df, method = "complete")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

#Average Silhouette Method
fviz_nbclust(df, FUN = hcut, method = "silhouette")
#the optimal number of clusters required is 10, by using silhouette methos

#Gap Statistic Method
gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
#the optimal number of clusters required is 10, by using Gap Statistics method
