################################
##         XGBoost            ##
################################
# reference: https://cran.r-project.org/web/packages/xgboost/vignettes/xgboostPresentation.html
install.packages("xgboost")
require(xgboost)
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
# Each variable is a list containing two things, label and data
# label is the outcome of our dataset meaning it is the binary classification we will try to predict.
# Let's discover the dimensionality of our datasets.
dim(train$data)
dim(test$data)
# As seen below, the data are stored in a dgCMatrix which is a sparse matrix and label vector is a numeric vector ({0,1}
class(train$data)[1]
class(train$label)
# build XGBoost model
# objective: specify the learning task and the corresponding learning objective:
#            - "reg:squarederror" Regression with squared loss (Default)
#            - "reg:logistic" logistic regression.
#            - "binary:logistic" logistic regression for binary classification. Output probability.
#            -  "binary:logitraw" logistic regression for binary classification, output score before logistic transformation.
#            - "num_class" set the number of classes. To use only with multiclass objectives.
#            - "multi:softmax set xgboost to do multiclass classification using the softmax objective. 
#            - "multi:softprob" same as softmax, but prediction outputs a vector of ndata * nclass elements, which can be further reshaped to ndata, nclass matrix. The result contains predicted probabilities of each data point belonging to each class.
#            - "rank:pairwise" set xgboost to do ranking task by minimizing the pairwise loss
# max_depth: maximum depth of a tree. Default: 6;
# eta: control the learning rate: scale the contribution of each tree by a factor of 0 < eta < 1 when it is added to the current approximation. Used to prevent overfitting by making the boosting process more conservative
# gamma: minimum loss reduction required to make a further partition on a leaf node of the tree. the larger, the more conservative the algorithm will be.
# nthread: the number of cpu threads we are going to use;
# nrounds: max number of boosting iterations.
# verbose:	If 0, xgboost will stay silent. 
#           If 1, it will print information about performance. 
#           If 2, some additional information will be printed out. 
# min_child_weight: minimum sum of instance weight (hessian) needed in a child. If the tree partition step results in a leaf node with the sum of instance weight less than min_child_weight, then the building process will give up further partitioning. In linear regression mode, this simply corresponds to minimum number of instances needed to be in each node. The larger, the more conservative the algorithm will be. Default: 1
# subsample: subsample ratio of the training instance. Setting it to 0.5 means that xgboost randomly collected half of the data instances to grow trees and this will prevent overfitting.
# lambda: L2 regularization term on weights. Default: 0
# lambda_bias: L2 regularization term on bias. Default: 0
# alpha: L1 regularization term on weights. (there is no L1 reg on bias because it is not important). Default: 0
# method 1: use sparse matrix
bstSparse <- xgboost(data = train$data, label = train$label, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
# method 2: use dense matrix 
#Alternatively, you can put your dataset in a dense matrix, i.e. a basic R matrix.)
bstDense <- xgboost(data = as.matrix(train$data), label = train$label, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
# method 3: use xgb.DMatrix
dtrain <- xgb.DMatrix(data = train$data, label = train$label)
bstDMatrix <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

# verbose = 0, no message
bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 0)
# verbose = 1, print evaluation metric
bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 1)
# verbose = 2, also print information about tree
bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 2)

# predict the model
pred <- predict(bst, test$data)
print(head(pred))

# prediction error
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))

