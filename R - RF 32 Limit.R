
# USE model.matrix to manage columns with > 32 predictors for modeling with randomForest, etc

Dummy variables are useful for several models being fit in this section. 
The randomForest function has a limitation that all factor predictors must not have more than 32 levels. 
The customer type predictor has 39 levels, so a predictor set of dummy variables is created for this and other 
models using the model.matrix function:

> ## The first column is the intercept, which is eliminated:
> trainingInd   <- data.frame(model.matrix(CARAVAN ~ ., data = training))[,-1]
