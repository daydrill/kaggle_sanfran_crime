


# leanpub의 The Art of Data Science(https://leanpub.com/artofdatascience) 중 chapter4 EDA의 방법을 이용하여 EDA.
# data : https://www.kaggle.com/c/sf-crime


#################################################
## Step 1. formulate your question
#################################################

## Question : 부동산 가격이 낮으면 범죄발생율도 높을까?

# data vis type : heat map




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
barplot(table(df_train$Category))


## 2) sampleSubmission
colnames(sampleSubmission)
# => Id 제외하고 39개 

## 결론 : 이상없음!



#### 2. 요일 갯수는? 
unique(df_train$DayOfWeek)
# => 7개
barplot(table(df_train$DayOfWeek))
## 결론 : 이상없음!






#################################################
## Step 6. Validate with at least one external data source  
#################################################


## 샌프란 부동산 데이터 구해서 매쉬업하기!



#################################################
## Step 7. Make a plot           
#################################################
#install.packages("devtools") 
library(devtools)
#install_github("ramnathv/htmlwidgets")
#install_github("bokeh/rbokeh")
library(rbokeh)
library(htmlwidgets)
#install.packages("ggmap")
library(ggmap)
library(dplyr)

library(maps)





##gmap(title = "NYC taxi pickups January 2013",
##     lat = 40.74, lng = -73.95, zoom = 11,
##     map_type = "roadmap", width = 1000, height = 800) %>%
##  ly_hexbin(nyctaxihex, alpha = 0.5,
##            palette = "Spectral10", trans = log, inv = exp)


# sfMap <- qmap("San Francisco", zoom = 12, color = "bw")
# => qmap은 rbokeh에 안됨..
  
  
  

df_temp <- df_train %>% sample_n(4000)  

gmap(title="San Francisco", lng=mean(df_temp$X) , lat=mean(df_temp$Y),  zoom=11, map_type="roadmap",width = 1000, height = 800) %>%
  ly_hexbin(data=df_temp, X, Y, xbins=10, shape=1, style="colorscale", alpha=0.8)




#################################################
## Step 8. Try the easy solution first    
#################################################





#################################################
## Step 9. Follow up            
#################################################





