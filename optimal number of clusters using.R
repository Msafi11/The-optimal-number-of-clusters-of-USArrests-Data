library(corrplot)
library(cluster)
library(factoextra)

data("USArrests") 

any(is.na(USArrests))

USArrests <- scale(USArrests)


set.seed(123)
crime <- sample(1:15,1)


wss <- sapply(1:crime, 
              function(k){kmeans(USArrests, k, nstart=20,iter.max = 15                     )$tot.withinss})
plot(1:crime, wss,
     type="b", pch = 19, frame = TRUE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fviz_nbclust(USArrests, kmeans, nstart = 20,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "TASK#3 - 20170237 - Mohamed Safi Ahmed")