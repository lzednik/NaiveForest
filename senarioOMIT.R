#           setwd("./R/KDD/2009")
### Too much NA so I skip ####
appet <- read.csv('./data/sm_appet_labels.csv', header=TRUE, sep=',')
churn <- read.csv('./data/sm_churn_labels.csv', header=TRUE, sep=',')
upsell <- read.csv('./data/sm_upsell_labels.csv', header=TRUE, sep=',')
build <- read.table('./data/orange_small_train.data', header=TRUE, sep='\t')
### There's no test data set 

####  All is duplicated   >>>>  build$isInBuild : yes yes yes yes ... yes
colLevels = sapply(build, function(k) length(unique(k)))
isUseless = (colLevels ==1)
build = build[,!isUseless]

#### NA is too many ,say 95% of the row got no values.
naCount = sapply(build,function(k) sum(is.na(k)))
summary(naCount) ;  table(naCount<45000) ;
tooMuchNA = naCount > 45000 ## 79 var  /230  var
build = build[,!tooMuchNA]

## NA supplement 
##perfect = which( naCount ==0)

##corr = cor(build[,numVar]) lots of missing value

## Because Target can be Neither churn , appet  not upsell. It's three seperated question
yValues = data.frame(churn,appet,upsell)
names(yValues) = c("churn","appet","upsell")
#build_C = data.frame(build, churn)
#build_A = data.frame(build, appet)
#build_U = data.frame(build, upsell)
rm(appet,churn,upsell,isUseless,
	colLevels,naCount,tooMuchNA)



## Another senario to narrow down the range to 8800s rows
##target = cbind(churn,appet,upsell)
##targetConcise = which(rowSums(target)==-1)
##build  = build[targetConcise,]