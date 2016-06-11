



######################################################
### 1. Load the data
######################################################

library(data.table)
library(lubridate)
trainDF<-fread("data/train.csv",data.table=FALSE)
testDF<-fread("data/test.csv",data.table=FALSE)
#setting data.table=FALSE so that trainDF and testDF are data frames instead of data tables



######################################################
### 2. Data Preprocessing 
######################################################


trainDF$Dates<-fast_strptime(trainDF$Dates, format="%Y-%m-%d %H:%M:%S", tz="UTC") 
trainDF$Day<-day(trainDF$Dates) 
trainDF$Month<-month(trainDF$Dates) 
trainDF$Year<-year(trainDF$Dates) 
trainDF$Hour<-hour(trainDF$Dates) 
trainDF$Minute<-minute(trainDF$Dates) 
trainDF$Second<-second(trainDF$Dates) 

#got idea from https://brittlab.uwaterloo.ca/2015/11/01/KaggleSFcrime/
trainDF$Night<-ifelse(trainDF$Hour > 22 | trainDF$Hour < 6,1,0)
trainDF$Intersection<-grepl("/", trainDF$Address) 
trainDF$Intersection<-plyr::mapvalues(trainDF$Intersection,from=c("TRUE","FALSE"),to=c(1,0)) 

# 제거할 변수
names(trainDF)[c(1,3,6,7,10,14,15)]
#can't try PCA since we have categorial variables
#remove these columns because the 3rd column Descript and 
#6th column Resolution are not in the test set
#the 7th column Address cannot be quantified. Maybe convert it to Zipcode?
#Can also use X and Y instead of the 7th column
trainDF_subset<-trainDF[,names(trainDF)[-c(1,3,6,7,10,14,15)]]


testDF$Dates<-fast_strptime(testDF$Dates, format="%Y-%m-%d %H:%M:%S", tz="UTC") 
testDF$Day<-day(testDF$Dates) 
testDF$Month<-month(testDF$Dates) 
testDF$Year<-year(testDF$Dates) 
testDF$Hour<-hour(testDF$Dates)
testDF$Minute<-minute(testDF$Dates) 
testDF$Second<-second(testDF$Dates)

testDF$Night<-ifelse(testDF$Hour > 22 | testDF$Hour < 6,1,0)
testDF$Intersection<-grepl("/", testDF$Address) 
testDF$Intersection<-plyr::mapvalues(testDF$Intersection,from=c("TRUE","FALSE"),to=c(1,0)) 

testDF_subset<-testDF[,names(testDF)[-c(1,2,5,8)]]




## 10000개만 샘플링
library(dplyr)
#testDF_subset2 <- testDF_subset %>% sample_n(10000)
#trainDF_subset2 <- trainDF_subset %>% sample_n(10000)










#####
#convert to sparse matrix
set.seed(6) # 6,296,354, 425...
index <- sample(1:nrow(trainDF), 30000)
trainDF_subsetCV<-trainDF_subset[index,]
categoryMatrix<-data.frame(with(trainDF_subsetCV,model.matrix(~Category+0))) 
names(categoryMatrix)<-sort(unique(trainDF$Category)) 
trainDF_subsetCV<-cbind(categoryMatrix,trainDF_subsetCV)
library(glmnet)
#got idea from https://brittlab.uwaterloo.ca/2015/11/01/KaggleSFcrime/
sparse.mat.tr<-sparse.model.matrix(~as.factor(PdDistrict)+X+Y+DayOfWeek+Intersection+Night,data=trainDF_subsetCV) 
object.size(sparse.mat.tr)
object.size(m) #m is the model matrix for the features, used in xgboost
#m uses about 2x memory as sparse.mat.tr
object.size(categoryMatrix)
#same as model.matrix(~Category, data=trainDF_subset)







######################################################
### 3. XGBoost
######################################################

library(caret)
library(Metrics)
library(gbm)
library(xgboost)
library(doMC)


getDoParWorkers() 
# -> 몇개 코어인지?
registerDoMC(4) #  is used to register the multicore parallel backend with the foreach package.
# -> 코어 4개 등록

# 병렬처리에 대한 설명: http://www.stat.wisc.edu/~gvludwig/327-5/parallel.html#/





#trainDF_subsetCV <- trainDF_subset %>% sample_n(10000)
str(trainDF_subsetCV)
#need PdDistrct and DayOfWeek to be converted to Factor
#trainDF_subsetCV$DayOfWeek <- as.factor(trainDF_subsetCV$DayOfWeek)
#trainDF_subsetCV$PdDistrict <- as.factor(trainDF_subsetCV$PdDistrict)


#model.matrix then converts the factors into dummy variables, that is
#Monday = (1,0,0,...), Tuesday = (0,1,0,0,..)
m <- model.matrix( 
  ~ PdDistrict + DayOfWeek + X + Y +Night+Intersection, data = trainDF_subsetCV
)

trainDF_subsetCV$Category<-factor(trainDF_subsetCV$Category)
num.class=length(levels(trainDF_subsetCV$Category))
levels(trainDF_subsetCV$Category)=1:num.class
ynum = as.matrix(as.integer(trainDF_subsetCV$Category)-1)
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss", "nthread" = 4,
              "num_class" = num.class, "max_depth" = 16,    # maximum depth of tree 
              "eta" = 0.3)    # step size shrinkage 
bst.cv = xgb.cv(param=param, data = m, label = ynum, 
                nfold = 3, nrounds = 5) #nrounds = max number of iterations
#Of the nfold subsamples, a single subsample is retained as the validation data for testing 
#the model, and the remaining nfold - 1 subsamples are used as training data.
#The cross-validation process is then repeated nrounds times, with each of the nfold subsamples
#used exactly once as the validation data


bst <- xgboost(param=param, data=sparse.mat.tr, label=ynum, nrounds=5, verbose=TRUE)
#nrounds=50, eta=.1
#eta=.01 produces training error too large, due to underfitting perhaps
#results in logloss of 2.43 on Kaggle dashboard page




######################################################
### 4. Model Understanding
######################################################

#http://www.rpubs.com/SteveM49/Benesty_on_XGBoost2

model <- xgb.dump(bst, with.stats = T)
model[1:10]

# Get the feature real names
names <- dimnames(sparse.mat.tr)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)

# Nice graph
#install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)
xgb.plot.importance(importance_matrix[1:10,])


install.packages("DiagrammeR")
library(DiagrammeR)
xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 2)

