
########################################## Loading Packages and Dataset ########################################

# Loading packages required for the analysis
if(!require("tree")) { install.packages("tree"); require("tree") }
if(!require("FNN")) { install.packages("FNN"); require("FNN") }
if(!require("randomForest")) { install.packages("randomForest"); require("randomForest") }
if(!require("xgboost")) { install.packages("xgboost"); require("xgboost") }
if(!require("mltools")) { install.packages("mltools"); require("mltools") }
if(!require("varhandle")) { install.packages("varhandle"); require("varhandle") }
library(data.table)
LogLoss <- function(y_pred, y_true) {
  eps <- 1e-15
  y_pred <- pmax(pmin(y_pred, 1 - eps), eps)
  LogLoss <- -mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
  return(LogLoss)
}

# The dataset was divided into files with 100K observations using Cygwin
# Reading Train000, which will be used for further analysis
Train000 <- fread("Train-000.csv")
colnames(Train000)<-c("id","click","hour","C1","banner_pos","site_id","site_domain","site_category","app_id","app_domain","app_category","device_id","device_ip","device_model","device_type","device_conn_type","C14","C15","C16","C17","C18","C19","C20","C21")

########################################## Feature Transformation #############################################

# 1. Changing categorical variables with less than 21 categories into binary encoded variables
# Specifying variables and changing them to factors 
cat_vars <- c("C1","banner_pos","site_category","app_category","device_type","device_conn_type","C15","C16","C18")
categorical_1 <- Train000[, lapply(.SD, as.factor),.SDcols= cat_vars]
# One hot encoding factor variables
categorical_1 <- one_hot(categorical_1, dropCols = TRUE, naCols = FALSE)

# 2. Filtering categorical variables with more than 21 categories using top frequencies
# Changing into factor variables
cat_vars <- c("site_id","site_domain","app_id","app_domain","device_model","device_id",
              "device_ip","C14","C17","C19","C20","C21")
# Saving variables as factors
categorical_2 <- Train000[, lapply(.SD, as.factor),.SDcols= cat_vars]
categorical_2 <- as.data.frame(categorical_2)

# Obtaining 20 most frequent categories for these variables 
for(colname in cat_vars){
  top_20 <- as.character(as.data.frame(sort(table(categorical_2[,colname]),decreasing=TRUE)[1:20])$Var1)
  for(name in top_20){
    new_name <- paste(colname,name,sep="_")
    categorical_2[categorical_2[,colname]==name, new_name]<- 1
    categorical_2[!categorical_2[,colname]==name, new_name]<- 0
  }
}
categorical_2 <- categorical_2[,13:252]

# 3.For "hour", we converted it into time bins and weekdays.
# Convert dates into days of week starting October 21, 2014, which was a Tuesday 
Train000[, Tuesday:= 0]; Train000[substr(Train000$hour, 5, 6) == 21 | substr(Train000$hour, 5, 6) == 28, Tuesday:= 1]
Train000[, Wednesday:= 0]; Train000[substr(Train000$hour, 5, 6) == 22 | substr(Train000$hour, 5, 6) == 29, Wednesday:= 1]
Train000[, Thursday:= 0]; Train000[substr(Train000$hour, 5, 6) == 23, Thursday:= 1]
Train000[, Friday:= 0]; Train000[substr(Train000$hour, 5, 6) == 24, Friday:= 1]
Train000[, Saturday:= 0]; Train000[substr(Train000$hour, 5, 6) == 25, Saturday:= 1]
Train000[, Sunday:= 0]; Train000[substr(Train000$hour, 5, 6) == 26, Sunday:= 1]
Train000[, Monday:= 0]; Train000[substr(Train000$hour, 5, 6) == 27, Monday:= 1]

# Assigning times of day as morning(5-11am), day(11am-5pm), evening (5-8pm), night (8pm-12am)
Train000[, Morning:= 0]; Train000[as.numeric(substr(Train000$hour, 7, 8))>=5 & as.numeric(substr(Train000$hour, 7, 8)) < 11, Morning:= 1] # 5 to 11
Train000[, Day:= 0]; Train000[as.numeric(substr(Train000$hour, 7, 8))>=11 & as.numeric(substr(Train000$hour, 7, 8)) < 17, Day:= 1] # 11 to 17
Train000[, Evening:= 0]; Train000[as.numeric(substr(Train000$hour, 7, 8))>= 17 & as.numeric(substr(Train000$hour, 7, 8)) < 20, Evening:= 1] # 17 to 20
Train000[, Hour:= as.numeric(substr(Train000$hour, 7, 8))]
Train000[, Weekday:= as.numeric(substr(Train000$hour, 5, 6))]
Train000[, Night:= ifelse(Train000$Hour>=20&Train000$Hour<24,1,ifelse(Train000$Hour>=0&Train000$Hour<5,1,0))]

# 4. Adding interaction between features in the dataset
Train000 <- as.data.frame(Train000)
Train000['app_site'] <- with(Train000, paste0(site_id, app_id))
Train000['site_device'] <- with(Train000, paste0(site_domain, device_model))
Train000['week_app'] <- with(Train000, paste0(Weekday, app_id))
Train000['week_site'] <- with(Train000, paste0(Weekday, site_id))
# Changing into factor variables
cat_vars <- c('app_site','site_device','week_app','week_site')
# Saving variables as factors
Train000 <- as.data.table(Train000)
categorical_3 <- Train000[, lapply(.SD, as.factor),.SDcols= cat_vars]
categorical_3 <- as.data.frame(categorical_3)
# Obtaining top 20 most frequent features
for(colname in cat_vars){
  top_20 <- as.character(as.data.frame(sort(table(categorical_3[,colname]),decreasing=TRUE)[1:20])$Var1)
  for(name in top_20){
    new_name <- paste(colname,name,sep="_")
    categorical_3[categorical_3[,colname]==name, new_name]<- 1
    categorical_3[!categorical_3[,colname]==name, new_name]<- 0
  }
}
categorical_3 <- categorical_3[,5:84]

# Combining target variable with the transformed and newly created variables above
Train000 <- cbind(Train000[,c(2,25:36,39)],categorical_1)
Train000 <- cbind(Train000, categorical_2)
Train000 <- cbind(Train000, categorical_3)

########################################## Train and Test Split ###############################################

# Splitting 70% of data as train and 30% as test RPerm <- sample(nrow(Train000))
Train000 <- Train000[RPerm,]
TrainData <- Train000[1:70000,]
ValData <- Train000[(70000+1):100000,]

# Writing training and validation files into csv to be able to be importe in Python
write.csv(TrainData,"Trainftrl.csv")
write.csv(Train000,"Train000Features2.csv")

########################################## Log Loss Function #################################################
LogLoss <- function(y_pred, y_true) {
  eps <- 1e-15
  y_pred <- pmax(pmin(y_pred, 1 - eps), eps)
  LogLoss <- -mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
  return(LogLoss)
}
