Data_healthy=as.data.frame(healthy_lifestyle_city_2021_Copia)
Data_healthy
head(Data_healthy)
Ranking=Data_healthy$Rank
Ranking

ncol(Data_healthy)
library(tidyverse)
Data_healthy %>% 
  DataExplorer::create_report()

Data_healthy_new=Data_healthy[,-2]
Data_healthy_new=Data_healthy_new[,-7]
Data_healthy_new=Data_healthy_new[-41,]

head(Data_healthy_new)

rownames(Data_healthy_new)<-Data_healthy_new$City


Data_healthy_com=Data_healthy_new[complete.cases(Data_healthy_new),]
Data_healthy_com
View(Data_healthy_com)
head(Data_healthy_com)
nrow(Data_healthy_com)
#41
ncol(Data_healthy_com)
#10
Data_healthy_com[,c(2:10)]

Data_healthy_com$`Sunshine hours(City)`=as.numeric(Data_healthy_com$`Sunshine hours(City)`)
is.numeric(Data_healthy_com$`Sunshine hours(City)`)
Data_healthy_com$`Cost of a bottle of water(City)`=as.numeric(Data_healthy_com$`Cost of a bottle of water(City)`)
Data_healthy_com$`Obesity levels(Country)`=as.numeric(Data_healthy_com$`Obesity levels(Country)`)
Data_healthy_com$`Life expectancy(years) (Country)`=as.numeric(Data_healthy_com$`Life expectancy(years) (Country)`)
Data_healthy_com$`Pollution(Index score) (City)`=as.numeric(Data_healthy_com$`Pollution(Index score) (City)`)
Data_healthy_com$`Happiness levels(Country)`=as.numeric(Data_healthy_com$`Happiness levels(Country)`)
Data_healthy_com$`Outdoor activities(City)`=as.numeric(Data_healthy_com$`Outdoor activities(City)`)
Data_healthy_com$`Number of take out places(City)`=as.numeric(Data_healthy_com$`Number of take out places(City)`)

Data_healthy_com$`Cost of a monthly gym membership(City)`=as.numeric(Data_healthy_com$`Cost of a monthly gym membership(City)`)

Data_healthy_com %>% 
  DataExplorer::create_report()

set.seed(123)
#####Hierarchical clustering
##first thing I gave to scale
Data_healthy_fram=as.data.frame(Data_healthy_com)
#ncol(Data_healthy_fram)
#head(Data_healthy_fram)
#str(Data_healthy_fram)
Data_healthy_scale=scale(as.matrix(Data_healthy_fram[,c(-1)]))
#di=dist(Data_healthy_scale)
#health_cl_comp=hclust(di,method="complete")
#plot(health_cl_comp, labels=Data_healthy_fram$City)

#health_cl_ave=hclust(di,method="average")
#plot(health_cl_ave,labels=Data_healthy_fram$City)

#health_cl_sin=hclust(di,method="single")
#plot(health_cl_sin,labels=Data_healthy_fram$City)

par(mfrow=c(1,1))

###PCA
library(car)
head(Data_healthy_fram)
rho=scale(Data_healthy_fram[,-1])
rho

health.pca=prcomp((Data_healthy_fram[,-1]),scale=TRUE)
summary(health.pca)
biplot(health.pca)

pr.var=health.pca$sdev^2
pve=pr.var/sum(pr.var)

##scree plot
par(mfrow=c(1,2))
plot(pve,xlab="Principal component",ylab="Proportion of variance expalined",ylim=c(0,1),type="b")
plot(cumsum(pve),xlab="Principal component",ylab="Cumulative variance expalined",ylim=c(0,1),type="b")

#other plot
heal.prin=princomp(scale(Data_healthy_fram[,-1]),scores = TRUE,cor=T)
heal.prin$scores
heal.prin$loadings
par(mfrow=c(1,2))
# loadinngs
plot(heal.prin$loadings, main="Loadings plot",
     xlab="comp1",ylab="comp2")
text(heal.prin$loadings,rownames(Data_healthy_fram))
abline(v=0,h=0,col="red")


summary(heal.prin)
# number of components:
screeplot(princomp(scale(Data_healthy_fram[,-1]), cor=T))
# plots:
plot(princomp(Data_healthy_fram[,-1], cor=T)$scores)
text(princomp(Data_healthy_fram[,-1], cor=T)$scores, rownames(Data_healthy_fram))
abline(h=0, v=0)


par(mfrow=c(1,1))
###############################################
###Kmeans cluster##############################
###############################################


####
head(Data_healthy_fram)
##Life expectancy 
#pollution index

##we want to compare cities by their life expectancy e pollution index

set.seed(123)
grouplife <- kmeans(Data_healthy_scale[,c(4,5)], centers=5, nstart=10)
grouplife

plot(Data_healthy_scale[,4],Data_healthy_scale[,5], type="n",xlim=c(-5,2), xlab="Life expectancy", ylab="Pollution index")
text(x=Data_healthy_scale[,4], y=Data_healthy_scale[,5], 
     labels=Data_healthy_fram$City,col=grouplife$cluster+1)

##Selecting the number of cluster:
wss=0
for (i in 1:6){
  km.ou=kmeans(Data_healthy_scale[,c(4,5)],centers=i,nstart=20)
  wss[i]=km.ou$tot.withinss
}
plot(1:6,wss,type="b",xlab="Num of clusters",ylab="within groups sum of squares")

##The elbow shows that the number of groups should be two.
grouplife_km2 <- kmeans(Data_healthy_scale[,c(4,5)], centers=2, nstart=20)
grouplife_km2

plot(Data_healthy_scale[,4],Data_healthy_scale[,5], type="n",xlim=c(-5,2), xlab="Life expectancy", ylab="Pollution index")
text(x=Data_healthy_scale[,4], y=Data_healthy_scale[,5], 
     labels=Data_healthy_fram$City,col=grouplife_km2$cluster+1)
##with just two groups it will be like this 
##we can identify that in one group there are the cities that have a higher pollution index
##and a lower life expectancy 
##on the other hand we have cities that have a lower pollution index and a higher life expectancy

d.life <- dist(Data_healthy_scale[,c(4,5)], method = "euclidean") # Euclidean distance matrix.
H.life <- hclust(d.life, method="ward.D2")
plot(H.life,labels=Data_healthy_fram$City) # display dendogram
rect.hclust(H.life, k=2, border="red") ##in order to identify the two groups

##Clustering on all variables
Data_healthy_scale=scale(Data_healthy_fram[,-1])
km.h=kmeans(Data_healthy_scale,centers=5)
print(km.h)
str(km.h)
clusplot(Data_healthy_scale, km.h$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)

##Dendogrm with all the variables
Data_healthy_scale=scale(Data_healthy_fram[,-1])
km.h=kmeans(Data_healthy_scale,centers=5)
print(km.h)
str(km.h)

##Selectiong the number of cluster
wss=0
for (i in 1:30){
  km.out=kmeans(Data_healthy_scale,centers=i,nstart=20)
  ##save the total within sum of squares
  wss[i]=km.out$tot.withinss
}
plot(1:30,wss,type="b",xlab="Num of cluster",ylab="within group sum of squares")

##we can see that we don't actually have a clear wlbow.
##we can try with k=6 and 4
library(cluster)
clusplot(Data_healthy_scale, km.h$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
km.cl=hclust(dist(Data_healthy_scale))
plot(km.cl,labels=Data_healthy_fram$City)
groups.life <- cutree(km.cl, k=5) # cut tree into 5 clusters
rect.hclust(km.cl, k=5, border="red")

###plot with four groups
set.seed(123)
km.h=kmeans(Data_healthy_scale,centers=4)
print(km.h)
str(km.h)
clusplot(Data_healthy_scale, km.h$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)

##plot with two groups:
set.seed(123)
km.h=kmeans(Data_healthy_scale,centers=2)
print(km.h)
str(km.h)
clusplot(Data_healthy_scale, km.h$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)

##plot with three groups:
set.seed(123)
km.h=kmeans(Data_healthy_scale,centers=3)
print(km.h)
str(km.h)
clusplot(Data_healthy_scale, km.h$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)
plot(km.cl,labels=Data_healthy_fram$City)
groups.life <- cutree(km.cl, k=3) # cut tree into 3 clusters
rect.hclust(km.cl, k=3, border="red")
##Dendogrm with all the variables
km.cl=hclust(dist(Data_healthy_scale))
plot(km.cl,labels=Data_healthy_fram$City)
groups.life.4 <- cutree(km.cl, k=4) # cut tree into 5 clusters
rect.hclust(km.cl, k=4, border="red")
plot(km.cl,labels=Data_healthy_fram$City)
groups.life <- cutree(km.cl, k=4) # cut tree into 5 clusters
rect.hclust(km.cl, k=4, border="red")
