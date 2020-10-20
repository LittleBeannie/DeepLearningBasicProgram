#######################################
#            Random Forest            #
#######################################
library(randomForest)
## ntree: Number of trees to grow. 
## mtry: Number of variables randomly sampled as candidates at each split.
## replace: Should sampling of cases be done with or without replacement?
## nodesize: Minimum size of terminal nodes.
## maxnodes: Maximum number of terminal nodes trees in the forest can have.
## classwt: Priors of the classes. Need not add up to one. Ignored for regression.
## cutoff: (Classification only) A vector of length equal to number of classes. The 'winning' class for an observation is the one with the maximum ratio of proportion of votes to cutoff. Default is 1/k where k is the number of classes (i.e., majority vote wins).
## sampsize: Size(s) of sample to draw.
## importance: Should importance of predictors be assessed?
## proximity: if proximity=TRUE when randomForest is called, a matrix of proximity measures among the input (based on the frequency that pairs of data points are in the same terminal nodes).
#######################################
#            Example 1                #
#######################################
fit <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fit) # view results
importance(fit) # importance of each predictor

#######################################
#            Example 2                #
#######################################
## Classification:
data(iris)
set.seed(71)
iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        proximity=TRUE)
print(iris.rf)
## Look at variable importance:
round(importance(iris.rf), 2)
## Do MDS on 1 - proximity:
iris.mds <- cmdscale(1 - iris.rf$proximity, eig=TRUE)
op <- par(pty="s")
pairs(cbind(iris[,1:4], iris.mds$points), cex=0.6, gap=0,
      col=c("red", "green", "blue")[as.numeric(iris$Species)],
      main="Iris Data: Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(iris.mds$GOF)

#######################################
#            Example 3                #
#######################################
## stratified sampling: draw 20, 30, and 20 of the species to grow each tree.
iris.rf2 <- randomForest(iris[1:4], iris$Species, 
                          sampsize=c(20, 30, 20))

#######################################
#            Example 4                #
#######################################
## Regression:
ozone.rf <- randomForest(Ozone ~ ., data=airquality, mtry=3,
                         importance=TRUE, na.action=na.omit)
print(ozone.rf)
## Show "importance" of variables: higher value mean more important:
round(importance(ozone.rf), 2)







