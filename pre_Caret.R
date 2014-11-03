#
source("senarioOMIT.R")

vars = names(build)
numVar = vars[sapply(build,class) %in% c("integer","numeric")]
head(build[,numVar])
cateVar = vars[sapply(build,class) %in% c("factor","character")]
head(build[,cateVar])

# Numeric  Variable operation###########################
## Delete high correlation     >>>Need  update (build = build[,c(newVar,cateVar)])  to implement
library(caret)
corr =cor(build[,numVar],use="complete") ##Works 
highcorr = findCorrelation(corr,0.75)
numVar = numVar[-highcorr]
## Standardlize
for(v in numVar){
   build[,v] =  (build[,v] - mean(build[,v],na.rm=TRUE))/sd(build[,v],na.rm=TRUE)
}
##sum(sapply(build[,numVar],function(k) sum(is.na(k))))
##[1] 242623
## Mode supplement
for(v in numVar){
 howMuchComplete = nrow(build) - sum(is.na(build[,v]))
 modeCount = max(table(build[,v]))
 if(modeCount >= 0.6*howMuchComplete){        ## If mode occupies the feature, go mode
        mode = (table(build[,v])==max(table(build[,v]))) ## split to two lines
        mode = as.numeric(names(mode))                  ## Now we get mode
        if(length(mode)==1)
               build[which(is.na(build[,v])),v] = mode  
  }
}                     ##[1] 242623

## Median supplement
for(v in numVar) {
     build[which(is.na(build[,v])),v] =median(build[,v],na.rm=TRUE)
}

# PCA  in the way as following codes doesn't work ~
#Xnum = preProcess(build[,numVar],method=c("scale","center","pca"))
#build[,numVar] = predict(Xnum,build[,numVar])
#Categorical Variable operation#########################
## BOO-YAH~~~ THERE'S NO MISSING VALUES HERE
### if it's too small , say under 50 

for(v in cateVar){
	### Grouping the trifles
          cateTooSmall = names(which(table(build[,v])<= 200 ))
          shouldBeGroup = sapply(build[,v],function(k) k %in% cateTooSmall) ## add which cause error (sapply can't symplify it! !)
          if(sum(shouldBeGroup*1) >0){
                 levels(build[,v]) = c(levels(build[,v]),"others")
                 build[shouldBeGroup,v]=  "others" 
          }
              ### Recreate the levels
              build[,v]=as.factor(as.character(build[,v]))
              ### Remove 1-levels  features    >>>   Need  update (build = build[,c(newVar,cateVar)])  to implement
              if(length(unique(build[,v]))==1) {
                  cateVar = setdiff(cateVar,v)
              }
}



# Output ########################################## 
build = build[,c(numVar,cateVar)]
