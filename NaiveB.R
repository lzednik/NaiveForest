
train<-read.csv("train.csv",header=TRUE)
test<-read.csv("test.csv",header=TRUE)

names(train) <- c('x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9', 'x10', 'x11', 'x12','y')

write.csv(res, file = "MyData.csv")

library('e1071')
#this does not work
model<-glm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12,data=train,family=binomial(link="logit"))

#
nB_model <- naiveBayes(y ∼ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12, data=train)



train$x11 <- as.factor(train$x11)
train$x12 <- as.factor(train$x12)
nB_model <- naiveBayes(y∼., data=train[1:13])


res<-predict(nB_model,test)
