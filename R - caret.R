
#PCA with caret
dat <- iris
knnFit2 <- train(Species~., dat, method = "knn", preProcess=c("pca"), trControl = trainControl(method = "cv"))
