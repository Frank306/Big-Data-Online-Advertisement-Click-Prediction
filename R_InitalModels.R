
########################################## Loading Packages and Dataset ########################################

# Loading packages required for the analysis
if(!require("tree")) { install.packages("tree"); require("tree") }
if(!require("FNN")) { install.packages("FNN"); require("FNN") }
if(!require("randomForest")) { install.packages("randomForest"); require("randomForest") }
if(!require("xgboost")) { install.packages("xgboost"); require("xgboost") }
if(!require("mltools")) { install.packages("mltools"); require("mltools") }
if(!require("data.table")) { install.packages("data.table"); require("data.table") }

# The dataset was divided into files with 100K observations using Cygwin
# Reading three data files with 100k observations each, Train000 will be used for further analysis
Train000 <- fread("Train-000.csv")
Train001 <- fread("Train-001.csv")
Train002 <- fread("Train-002.csv")
colnames(Train000)<-c("id","click","hour","C1","banner_pos","site_id","site_domain","site_category","app_id","app_domain","app_category","device_id","device_ip","device_model","device_type","device_conn_type","C14","C15","C16","C17","C18","C19","C20","C21")
colnames(Train001)<-c("id","click","hour","C1","banner_pos","site_id","site_domain","site_category","app_id","app_domain","app_category","device_id","device_ip","device_model","device_type","device_conn_type","C14","C15","C16","C17","C18","C19","C20","C21")
colnames(Train002)<-c("id","click","hour","C1","banner_pos","site_id","site_domain","site_category","app_id","app_domain","app_category","device_id","device_ip","device_model","device_type","device_conn_type","C14","C15","C16","C17","C18","C19","C20","C21")

########################################## Feature Transformation #############################################

# 1. Encoding categorical variables with less than 21 categories into (n-1) binary variables
# Specifying variables and changing them to factors 
cat_vars <- c("C1","banner_pos","site_category","app_category","device_type","device_conn_type","C15","C16","C18")
categorical_1 <- Train000[, lapply(.SD, as.factor),.SDcols= cat_vars]
# One hot encoding factor variables
categorical_1 <- one_hot(categorical_1, dropCols = TRUE, naCols = FALSE)

# 2. Filtering categorical variables with more than 21 categories using correlation
# Specifying variables and changing them to factors 
cat_vars <- c("site_id","site_domain","app_id","app_domain","device_model","C14","C17","C19","C20","C21")
categorical_2 <- Train000[, lapply(.SD, as.factor),.SDcols= cat_vars]
# One hot encoding variables 
categorical_2 <- one_hot(categorical_2, dropCols = TRUE, naCols = FALSE)

# Adding the target into the dataframe so we can calculate correlation
data_model <- cbind(Train000[, c("click")], categorical_2)
cor <- as.data.table(cor(data_model[,-1], data_model$click))
cor <- cbind(colnames(data_model[,-1]),cor)
colnames(cor)<- c("category","correlation")
summary(cor)
cor <- cor[order(cor$correlation),]
head(cor)
# Retaining variables with absolute correlation larger than 0.05
large_cor <- cor[cor$correlation>0.05|cor$correlation<(-0.05),]
name_list <- c(large_cor$category)
categorical_2 <- as.data.frame(categorical_2)
categorical_2 <- categorical_2[,name_list]

# 3. Since the column device_id and device_ip have more than 10000 categories, we just encode the most frequent ones.
# Examining most frequent categories for device ip and id
head(summary(as.factor(Train000$device_id)),5)
head(summary(as.factor(Train000$device_ip)),5)

# Creating a variable for each top five device_ip and the top one device_id
Train000[, device_ip_6b9769f2:= 0]; Train000[device_ip == "6b9769f2", device_ip_6b9769f2:=1]
Train000[, device_ip_431b3174:= 0]; Train000[device_ip == "431b3174", device_ip_431b3174:=1]
Train000[, device_ip_930ec31d:= 0]; Train000[device_ip == "930ec31d", device_ip_930ec31d:=1]
Train000[, device_ip_af9205f9:= 0]; Train000[device_ip == "af9205f9", device_ip_af9205f9:=1]
Train000[, device_ip_2f323f36:= 0]; Train000[device_ip == "2f323f36", device_ip_2f323f36:=1]
Train000[, device_id_a99f214a:= 0]; Train000[device_id == "a99f214a", device_id_a99f214a:=1]

# 4. "hour" variable was converted into time bins and weekdays.
# Converting dates into days of week starting October 21, 2014, which was a Tuesday 
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
Train000[, Night:= ifelse(Train000$Hour>=20&Train000$Hour<24,1,ifelse(Train000$Hour>=0&Train000$Hour<5,1,0))]
Train000[as.numeric(substr(Train000$hour, 7, 8))>=0 & as.numeric(substr(data_model$hour, 7, 8)) < 5, Night:= 1]

# Combining target variable with the binary encoded and feature engineered variables from above
Train <- cbind(Train000[,c(2,25:42)],categorical_1)
Train <- cbind(Train, categorical_2)
colnames(Train)[121]<-'C20_negative_1'

########################################## Train and Test Split ###############################################

# Splitting 70% of data as train and 30% as test 
RPerm <- sample(nrow(Train))
Train <- Train[RPerm,]
TrainData <- Train[1:70000,] # 700K observations in train
ValData <- Train[(70000+1):100000,] # 300K observations in test

########################################## Log Loss Function #################################################
LogLoss <- function(y_pred, y_true) {
  eps <- 1e-15
  y_pred <- pmax(pmin(y_pred, 1 - eps), eps)
  LogLoss <- -mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
  return(LogLoss)
}

########################################## Logistic Regression #################################################

# Creating big and small formulas to run stepwise logistic
SmallFm <- click ~ 1
Vars <- names(TrainData)
BigFm <- paste(Vars[1],"~",paste(Vars[2:137],collapse=" + "),sep=" ")
BigFm <- formula(BigFm)

# Building binomial glm model using training data
OutSmall <- glm(SmallFm,family=binomial(link = "logit"),data=TrainData)
sc <- list(lower=SmallFm,upper=BigFm)
out <- step(OutSmall,scope=sc,direction="both")
summary(out)

# Predicting the probabilities on validation data, plotting ROC and calculating log loss
pred.val <- predict(out,newdata=ValData,type="response")
source("ROCPlot.r")
ROCPlot(pred.val,ValData$click)
print(paste("Logistic Regression Log Loss:",LogLoss(pred.val,as.numeric(ValData$click)))) # 0.43

########################################## Random Forest ######################################################

# Building Random Forest model with 500 trees
out3 <- randomForest(BigFm,data=TrainData,ntree=500)

# Predicting the probabilities on validation data, plotting ROC and calculating log loss
PHat <- predict(out3,newdata=ValData,type="prob")[,2]
ROCPlot(PHat,ValData$click)
print(paste("RF Log Loss:",LogLoss(PHat,as.numeric(ValData$click))))

########################################## K-NN Model ############################################################

# Making new data frames for standardized variables by copying unstandardized data frames
StndKnnTrainDF <- as.data.frame(TrainData[,-'click'])
StndKnnValDF <- as.data.frame(ValData[,-'click'])

# Overwriting each variable with the standardized variable using the mean and sd from the
# training data to standardize the validation and test data.
for(i in 1:length(StndKnnTrainDF)) {
  TrainMean <- mean(TrainData[[i+1]])
  TrainSD <- sd(TrainData[[i+1]])
  StndKnnTrainDF[,names(TrainData)[i+1]] <- (TrainData[[i+1]]-TrainMean)/TrainSD
  StndKnnValDF[,names(TrainData)[i+1]] <- (ValData[[i+1]]-TrainMean)/TrainSD
}

# Changing k from 5 to 100 with an interval of 10 to calculate log loss and AUC for each k 
Log.Loss <- rep(NA,10)
AUC.knn <- rep(NA,10)
ind <- 1
k_list <- c(5,10,20,30,40,50,60,70,80,100)
for(i in c(5,10,20,30,40,50,60,70,80,100)) {
  out <- knn(train=StndKnnTrainDF, test=StndKnnValDF,cl=TrainData$click,k=i,prob=TRUE)
  PHat_knn <- as.numeric(attr(out,"prob"))
  Log.Loss[ind] <- LogLoss(PHat_knn,as.numeric(ValData$click))
  AUC.knn[ind] <- ROCPlot(PHat_knn,ValData$click,Plot=F)$AUC
  ind <- ind + 1
}

# Printing k with the lowest log loss and the log loss value
print(paste("Log Loss of KNN:", min(Log.Loss)))
print(paste("best k:",k_list[which.min(Log.Loss)]))

#Change to category and same levels
for i in range(1,22):
  categories = np.union1d(train000.iloc[:,i], train001.iloc[:,i])
train000.iloc[:,i] = pd.Categorical(train000.iloc[:,i], categories=categories)
train001.iloc[:,i] = pd.Categorical(train001.iloc[:,i], categories=categories)
