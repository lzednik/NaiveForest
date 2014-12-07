##My analysis##
source("pre_Caret.R")

library(caret)
build_C= cbind(build,churn=yValues$churn)
build_C$churn = as.factor( build_C$churn )
###Ensembling ################
set.seed(123456)
inTrain = createDataPartition(y=build_C$churn,p=0.6,list=F)
train_C = build_C[inTrain,]
test_C   = build_C[-inTrain,]



fitControl = trainControl(method="cv",number=10,repeats=1)
## modADA = train(churn~.,data=sampleTrain_C,method="ada",trControl=fitControl)  ## too long 
 modGBM = train(churn~.,data=train_C,method="gbm",trControl = fitControl)
 modSVM = train(churn~.,data=train_C,method="svmRadial",trControl = fitControl)
 modGLM = glm(churn~.,data=train_C,family= binomial,trControl=fitControl) 

predADA = predict(modADA,newdata=test_C)
predGBM= predict(modGBM,newdata=test_C)
predSVM = predict(modSVM,newdata=test_C)
predGLM = predict(modGLM,newdata=test_C)
