
# leanpub의 The Art of Data Science(https://leanpub.com/artofdatascience) 중 chapter4 EDA의 방법을 이용하여 EDA.
# data : https://www.kaggle.com/c/sf-crime



#################################################
## Step 1. formulate your question
#################################################

## Question : 캘리포니아 주의 음주운전은 토요일에 가장 많이 발생할 것이다.




#################################################
## Step 2. Read in your data 
#################################################
#install.packages("readr")
library(readr)


df_train <- read_csv("data/train.csv") # col_types = "ccincc..." 으로 불러올 값 설정 가능.
df_test <- read_csv("data/test.csv")
sampleSubmission <- read_csv("data/sampleSubmission.csv")

#names(df_train) <- make.names(names(df_train)) # 열이름에 띄어쓰기 있는 경우에 사용.



#################################################
## Step 3. Check the packaging 
#################################################

#### 1. row check
nrow(df_train)
nrow(df_test)
nrow(sampleSubmission)

#### 2. column check
ncol(df_train)
ncol(df_test)
ncol(sampleSubmission)

#### 3. structure check
str(df_train)
str(df_test)

#################################################
## Step 4. Look at the top and the bottom of your data  
#################################################
head(df_train)
head(df_test)

tail(df_train)
tail(df_test)


#################################################
## Step 5. Check your "n"s
#################################################

#### 1. 범죄 카테고리는 몇개일까? sampleSubmission과 비교.

## 1) train 데이터의 Category
unique(df_train$Category)
# => 39개

## 그 분포는..?
barplot(table(df_train$Category))



## 2) sampleSubmission
colnames(sampleSubmission)
# => Id 제외하고 39개 

## 결론 : 이상없음!



#### 2. 시간적 결측치(?)가 있을까?

## 1) 주별 범죄 분포 
barplot(table(df_train$DayOfWeek))

## 2) 월별 범죄 분포
library(lubridate)
barplot(table(month(ymd_hms(df_train$Dates))))

## 3) 연도별 범죄 분포 
barplot(table(year(ymd_hms(df_train$Dates))))


## 3) 시간별 범죄 분포 
barplot(table(hour(ymd_hms(df_train$Dates))))


## 결론 : 연도 빼고는 고르게 분포되어 있음. 2015년은 빼고하는게 좋을듯..?





#################################################
## Step 6. Validate with at least one external data source  
#################################################

## 





#################################################
## Step 7. Make a plot           
## Step 8. Try the easy solution first
#################################################

library(dplyr)
library(ggplot2)


theme.ti <- element_text(family="NanumSquare", face="bold", size=22) 
theme.ax <- element_text(family="NanumSquare", face="bold", size=20, angle=00, hjust=0.54, vjust=0.5) #그래프 축 
theme.leti<-element_text(family="NanumSquare", face="bold", size=20) 
theme.lete<-element_text(family="NanumSquare", size=20) 




df <- df_train %>% group_by(Category, DayOfWeek) %>% summarise(count=n())
plot_temp <- ggplot(df, aes(x=DayOfWeek, y=count, colour=Category, group=Category)) + geom_line()+ theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete)

plot_temp

### 여기서 문제점은 무엇일까????



















## 1) exponential 함.
df <- df_train %>% group_by(Category, DayOfWeek) %>% summarise(count=log10(n()))

## 2) 요일순서대로 정리!
df$DayOfWeek <- factor(df$DayOfWeek, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
plot_temp <- ggplot(df, aes(x=DayOfWeek, y=count, colour=Category, group=Category)) + geom_line()+ theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete)


## 3) 카테고리가 너무 많음..ㅜ
library(plotly)
ggplotly(plot_temp)



### 결론 : 음주운전은 토요일에 가장 많이 발생한다.



#################################################
## Step 9. Follow up            
#################################################


### EDA 결론 : 
### 시,공간별로 가능한 패턴에서 의미를 찾아서 예측에 해보자


#http://rpubs.com/cloud_wei/2107








































