## SenarioINFER
appet <- read.csv('./data/sm_appet_labels.csv', header=TRUE, sep=',')
churn <- read.csv('./data/sm_churn_labels.csv', header=TRUE, sep=',')
upsell <- read.csv('./data/sm_upsell_labels.csv', header=TRUE, sep=',')
yValues = data.frame(churn,appet,upsell)
build = read.table('./data/orange_small_train.data', header=TRUE, sep='\t',
	na.strings=c('NA',''))
### There's no test data set 
##Useless column
build <- build[!duplicated(lapply(df.train,c))]                       ##Narrow down to 213 column
naFull = which( (sapply(build,function(k) sum(is.na(k)))) ==50000)
build <- build[,-naFull]
## Indicator ##
var.name = names(build)

cateVar.name = names(build[,cateVar])
numVar.name = names(build[,numVar])
naTooMuch = which(sapply(build,function(k) sum(is.na(k))>0.95*nrow(build)))

### NA Imputation #########################################################################
#######################################################################################
## is NA stands for any thing??
# anova to chech is NA helps?
naTable = is.na(build[,-ncol(build)])
naTable = data.frame(naTable,churn=churn$V1)
aovForNA = anova(lm(churn~.,data=naTable))
naPrecious.name = rownames(aovForNA)[aovForNA[5]<=0.05]
naPrecious.name = na.omit(naPrecious.name)
# Operation in uni: delete the columns NAisTooMuch
gonnaDelete= !(names(naTooMuch) %in% naPrecious.name)    ## Delete (  !naPracious &&  naTooMuch  )
gonnaDelete.name = names(naTooMuch)[ gonnaDelete]
var.name = setdiff(var.name,gonnaDelete.name) ## All columns' names reduce (NAtooMuch && !NAprecious)
build = build[,var.name] ### 50000x81 without target

#Operation in the rest # recall : naPrecious.name
cateVar = which(sapply(build,class) %in% c("factor","character"))
numVar = which(sapply(build,class) %in% c("numeric","integer"))                                                                   ##########            manyNAs         aFewNAs
#Opration : cate                                                                                                                                                        # NA precious          imputeSth         imputeSth
preciousAmongCate = naPrecious.name[naPrecious.name %in%  names(build)[cateVar] ]                        # NA useless                Deleted            Remained?
for(v in preciousAmongCate){
	levels(build[,v]) = c(levels(build[,v]),"undetected")
	 build[is.na(build[,v]),v] = "undetected"
}
#652624
#Operation : num                                                                                                                                                      ##########            manyNAs         aFewNAs
# Interaction with naPrecious.name                                                                                                                       # NA precious          imputeSth           imputeSth
preciousAmongNum = naPrecious.name[naPrecious.name %in% names(build)[numVar]]                          # NA useless                Deleted           Remained?
summary(build[,preciousAmongNum])
for(v in preciousAmongNum){
	build[,v] = cut(build[,v],breaks=unique(quantile(build[,v],na.rm=TRUE)))
	levels(build[,v]) = c(levels(build[,v]),"undetected")
	build[is.na(build[,v]),v] = "undetected"
}
cateVar = which(sapply(build,class) %in% c("factor","character"))
numVar = which(sapply(build,class) %in% c("numeric","integer"))   
var.name = c(numVar,cateVar)
#367968

#Violence chechHistogramByEach

### Dimension Compression #####################################################################
##########################################################################################
## Correlation test for numeric features          >> SADLY : na too nuch , finish the Imputation then do it
cor.matrix = matrix(0,ncol(build[,numVar]),ncol(build[,numVar]))
for(v1 in numVar){
	x=1
	for(v2 in numVar){
		y=1
		if(v1!=v2){
			cort = cor.test(build[,v1],build[,v2],alternative ="two.sided",
				method ="kendall")  	
		       if(cort$p.value < 0.95){
                     	cor.matrix[x,y] = 1
                     	cor.matrix[y,x] = 1
		       }    	 
		}
	      y=y+1
	}
     x=x+1
}
## PCA
