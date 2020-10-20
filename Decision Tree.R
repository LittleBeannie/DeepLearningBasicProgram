#######################################
#         Classification Tree         #
#######################################
library(rpart)

# grow tree
## method: one of "anova", "poisson", "class" or "exp". 
##         - If y is a survival object, then method = "exp" is assumed
##         - if y has 2 columns then method = "poisson" is assumed
##         - if y is a factor then method = "class" is assumed
##         - otherwise method = "anova" is assumed. 
## control: Various parameters that control aspects of the rpart fit. rpart.control(...)
##         - minsplit: the minimum number of observations that must exist in a node in order for a split to be attempted.
##         - minbucket: the minimum number of observations in any terminal <leaf> node
##         - cp: complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted.
##         - maxdepth: Set the maximum depth of any node of the final tree, with the root node counted as depth 0. 
##         - maxcompete: the number of competitor splits retained in the output.
##         - maxsurrogate: the number of surrogate splits retained in the output. 
##         - xval: number of cross-validations.
## surrogatestyle: controls the selection of a best surrogate. 
##         - If set to 0 (default) the program uses the total number of correct classification for a potential surrogate variable
##         - if set to 1 it uses the percent correct, calculated over the non-missing values of the surrogate. 
## usesurrogate: how to use surrogates in the splitting process. 
##         - 0 means display only; an observation with a missing value for the primary split rule is not sent further down the tree. 
##         - 1 means use surrogates, in order, to split subjects missing the primary variable; if all surrogates are missing the observation is not split. 
##         - For value 2 ,if all surrogates are missing, then send the observation in the majority direction. A value of 0 corresponds to the action of tree, and 2 to the recommendations of Breiman et.al (1984).
fit <- rpart(Kyphosis ~ Age + Number + Start, 
             method = "class", data = kyphosis)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE,
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for Kyphosis")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

#######################################
#           Regression Tree          #
#######################################
library(rpart)

# grow tree
fit <- rpart(Mileage~Price + Country + Reliability + Type,
             method="anova", data=cu.summary)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fit) # visualize cross-validation results  

# plot tree
plot(fit, uniform=TRUE,
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree
pfit<- prune(fit, cp=0.01160389) # from cptable   

# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Regression Tree for Mileage")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
