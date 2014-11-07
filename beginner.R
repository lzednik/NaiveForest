## Small data set - for testing
churn <- read.csv('./data/sm_churn_labels.csv', header=TRUE, sep=',')
kdd    =  read.table("./data/orange_small_train.data",sep="\t",
	header=TRUE ,na.strings=c("","NA")) 
churn = (churn+1) / 2                                 ## -1->0  1->1

selectedVar = c("Var1","Var2",                  ## numeric - NA too much
	"Var94",  "Var189",                       ## numeric - NA so so 
	"Var6","Var109" ,                          ## numeric - NA a little   
           "Var222","Var220",                          ## categorial NA too much
           "Var200","Var226",                          ## cateforial NA so so
             "Var208"  , "Var192"                     ##  categorial  NA a little   ) 
          )
kdd = kdd[,selectedVar]
cateVar = which(sapply(kdd,class) %in% c("factor","character"))
numVar = which(sapply(kdd,class) %in% c("numeric","integer"))
kdd = cbind(kdd,churn = churn$V1)

## check if na means sth. or nothing
naTable = is.na(kdd[,-ncol(kdd)])
naTable = cbind(naTable,churn=churn$V1)
naTable = as.data.frame(naTable)
na.logitRegr = glm(churn~.,data=naTable,family=binomial)    ## giving nothing
na.regr = lm(churn~.,data=naTable)
aovNAregr = anova(na.regr)
naCountColumn = rownames(aovNAregr)[aovNAregr[5]<0.05] 

# aov(churn~a+b+c+d+e+f+A+B+C+D+E+F, data=kdd)



