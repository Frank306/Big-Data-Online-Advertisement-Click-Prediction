if(!require("tree")) { install.packages("tree"); require("tree") }
if(!require("FNN")) { install.packages("FNN"); require("FNN") }
if(!require("randomForest")) { install.packages("randomForest"); require("randomForest") }
if(!require("xgboost")) { install.packages("xgboost"); require("xgboost") }
if(!require("mltools")) { install.packages("mltools"); require("mltools") }
if(!require("varhandle")) { install.packages("varhandle"); require("varhandle") }
library(data.table)

# Read in training data and test data
TrainData <- fread("Train-000.csv") # five million
TestData <- fread("ProjectTestData.csv")
colnames(TrainData)<-c("id","click","hour","C1","banner_pos","site_id","site_domain","site_category","app_id","app_domain","app_category","device_id","device_ip","device_model","device_type","device_conn_type","C14","C15","C16","C17","C18","C19","C20","C21")
# Insert an empty column in the test data, which will be used for prediction
TestData <- data.table(click = "", TestData)
setcolorder(TestData , c(2,1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24))
# rbind Train and Test to perform feature transformations all together
data <- rbind(TrainData,TestData)

# 1. For "hour", we converted it into time bins and weekdays.
# Convert dates into days of week starting October 21, 2014, which was a Tuesday 
data[, Tuesday:= 0]; data[substr(data$hour, 5, 6) == 21 | substr(data$hour, 5, 6) == 28, Tuesday:= 1]
data[, Wednesday:= 0]; data[substr(data$hour, 5, 6) == 22 | substr(data$hour, 5, 6) == 29, Wednesday:= 1]
data[, Thursday:= 0]; data[substr(data$hour, 5, 6) == 23, Thursday:= 1]
data[, Friday:= 0]; data[substr(data$hour, 5, 6) == 24, Friday:= 1]
data[, Saturday:= 0]; data[substr(data$hour, 5, 6) == 25, Saturday:= 1]
data[, Sunday:= 0]; data[substr(data$hour, 5, 6) == 26, Sunday:= 1]
data[, Monday:= 0]; data[substr(data$hour, 5, 6) == 27, Monday:= 1]

# 2. Assigning times of day as morning(5-11am), day(11am-5pm), evening (5-8pm), night (8pm-12am)
data[, Morning:= 0]; data[as.numeric(substr(data$hour, 7, 8))>=5 & as.numeric(substr(data$hour, 7, 8)) < 11, Morning:= 1] # 5 to 11
data[, Day:= 0]; data[as.numeric(substr(data$hour, 7, 8))>=11 & as.numeric(substr(data$hour, 7, 8)) < 17, Day:= 1] # 11 to 17
data[, Evening:= 0]; data[as.numeric(substr(data$hour, 7, 8))>= 17 & as.numeric(substr(data$hour, 7, 8)) < 20, Evening:= 1] # 17 to 20
data[, Hour:= as.numeric(substr(data$hour, 7, 8))]
data[, Weekday:= as.numeric(substr(data$hour, 5, 6))]
data[, Night:= ifelse(data$Hour>=20&data$Hour<24,1,ifelse(data$Hour>=0&data$Hour<5,1,0))]


# 3. add interactive features
data <- as.data.frame(data)
data['app_site'] <- with(data, paste0(site_id, app_id))
data['site_device'] <- with(data, paste0(site_domain, device_model))
data['week_app'] <- with(data, paste0(Weekday, app_id))
data['week_site'] <- with(data, paste0(Weekday, site_id))
data['device_site_id'] <- with(data, paste0(site_id, device_id))
data['device_app_id'] <- with(data, paste0(app_id, device_id))
data['device_site_ip'] <- with(data, paste0(site_id, device_ip))

# 4. add counts
data<-as.data.table(data)
data[,count_app_site:=.N,by=c('Weekday','app_site')]
data[,count_device_site:=.N,by=c('Weekday','device_site_id')]
data[,count_app_device:=.N,by=c('Weekday','device_app_id')]
data[,count_device_ip:=.N,by=c('Weekday','device_ip')]
data[,count_web_app_events:=.N,by=c('device_ip','app_site')]
data[,count_visits_web:=.N,by=c('device_ip','site_id')]
data[,count_visits_app:=.N,by=c('device_ip','app_id')]
data[,count_web_traffic:=.N,by=c('site_id')]
data[,count_app_traffic:=.N,by=c('app_id')]
data[,web_traffic_hour:=.N, by=c('site_id','hour')]
data[,app_traffic_hour:=.N, by=c('app_id','hour')]

data[,count_app_site:=log(count_app_site)]
data[,count_device_site:=log(count_device_site)]
data[,count_app_device:=log(count_app_device)]
data[,count_device_ip:=log(count_device_ip)]
data[,count_web_app_events:=log(count_web_app_events)]
data[,count_visits_web:=log(count_visits_web)]
data[,count_visits_app:=log(count_visits_app)]
data[,count_web_traffic:=log(count_web_traffic)]
data[,count_app_traffic:=log(count_app_traffic)]
data[,web_traffic_hour:=log(web_traffic_hour)]
data[,app_traffic_hour:=log(app_traffic_hour)]

# 5. Label-encoding categorical variables
for(i in c(1,3:55)){
  if(class(data[[i]])=="character"){
    data[[i]]=as.numeric(as.factor(data[[i]]))
  }
}
# Split data back into training and test
Traindata <- data[1:5000000,]
Testdata <- data[5000001:18015341,-'click']

# Write the transformed datasets into a file. We will build model in Python.
write.csv(Traindata,"Traindata.csv")
write.csv(Testdata,"Testdata.csv")
