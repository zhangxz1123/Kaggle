save(train, file="train.RData")
load("train.RData")

#extract predictors
train_data = train[,2:(length(train)-1)]
test_data = test[,2:length(test)]

train_data_nozero=removeColsAllZeros(train_data)

#run pca
model_pca = prcomp(train_data_nozero,center = T, scale.=T)
#first 75 pc's
feature = model_pca$x[,1:75]

#combine predictors and response
train_75 = as.data.frame(cbind(feature,train[,371]))
#run logistic regression
glm.out = glm(target~., family=binomial(logit), data=train_75)
summary(glm.out)

# project the new data onto PCA space
m = as.matrix(train_data)
zeros = which(colSums(abs(m),na.rm = TRUE) == 0)
test_data2 = test_data[,-zeros]
proj = scale(test_data2, model_pca$center, model_pca$scale) %*% model_pca$rotation
proj = as.data.frame(proj)
pred = predict(glm.out, proj[,1:75], type="response") 
prediction = round(pred)
save(prediction,file = "prediction.RData")
write.csv(prediction,file = "pred1.csv")



