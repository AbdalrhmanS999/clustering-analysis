View(iris)
z <- t[,-c(5,5)]
z
m <- apply(z,2,mean)
s <- apply(z,2,sd)
z <- scale(z,m,s
           )
eculid <- dist(z)
print(eculid,digits = 3)
hc <- hclust(eculid,method = "average")
plot(hc)
plot(hc,labels = t$Species,hang = -1)
t <- select(iris[1:30,],c(1:5))
irnew <- iris
irnew$Species <- NULL
nn <- kmeans(irnew,3)
plot(irnew[c("Sepal.Length","Sepal.Width")],col=nn$cluster)
points(nn$centers,col=1:3,pch=20,cex=2)
points(nn$cluster,col=1:3,pch=20,cex=2)
plot(irnew[c("Sepal.Length","Sepal.Width")],col=nn$centers)
plot(irnew[c("Sepal.Length","Sepal.Width")],col=nn$cluster)
points(nn$cluster,col=1:3,pch=11,cex=2)
points(nn$centers,col=1:3,pch=11,cex=2)
normalize <- function(x) {
  return( (x - min(x)) / (max(x) - min(x))) }
irissv <- as.data.frame(lapply(iris[,c(1,2,3,4)],normalize))
iristr <- irissv[1:129,]
iristest <- irissv[130:150,]
irisdevlp <-irissv[1:129,5]
irisdevlptest <- irissv[130:150,5]
require(class)
nbv <- knn(train = iristr,test = iristest,cl=irisdevlp,k=13)
nsbv <- knn(train = iristr,test = iristest,cl=irisdevlp,k=13)
View(iris)
View(iris3)
irnew$Species
table(irisdevlptest, nsbv)
abline(nn$cluster)
hist(Sepal.Length,col = "green",legend=rownames(Sepal.Length))
hist(Sepal.Length,col = "green",legend=rownames(Sepal.Length))
hist(Sepal.Length,col = "green",legend=rownames(Sepal.Length))
hist(Sepal.Length,col = "green",legend=rownames(Sepal.Length))
hist(iris$Sepal.Length,col = "green",legend=rownames(iris$Sepal.Length))
iris$Species <- as.factor(iris$Species)
plot(cars)
