# Big Data Online Advertisement Click Prediction
This project involves predicting clicks for on-line advertisements. The training data consists of
data for 9 days from October 21, 2014 to October 29, 2014. The training data is in the file
“ProjectTrainingData.csv.” There are in excess of **30 million records** in this file. The variables in
this file are as follows:<br/>
<br/>
	-id = the identifier of the ad (this may, or may not be unique to each row). <br/>
      	-click = 1 means the ad was clicked on. click = 0 means the ad was not clicked on.<br/>
      	-hour = the date and hour when the ad was displayed. Format is YYMMDDHH.<br/>
      	-C1 = an anonymized categorical variable.<br/>
      	-banner_pos = the position in the banner.<br/>
      	-site_id = an identifier for the web site.<br/>
      	-site_domain = an identifier for the site domain<br/>
	-site_category = a code for the site’s category.<br/>
      	-app_id = an identifier for the application showing the ad.<br/>
      	-app_domain = an identifier for the app’s domain.<br/>
      	-app_category = a code for the category of the app.<br/>
      	-device_id = an identifier for the device used.<br/>
      	-device_ip = a code for the ip of the device.<br/>
      	-device_model = the model of the device.<br/>
      	-device_type = the type of the device.<br/>
      	-device_conn_type = the type of the device’s connection<br/>
      	-C14 – C21 = anonymized categorical variables<br/>
		<br/>
Thus, there are 24 columns in the dataset. The variable “click” is the Y-variable in the dataset.
<br/>
**R_InitialModels.R:** <br/>
This file shows initial feature transformation and initial models (logistic regression, knn and random forest) being built. The models were built using 139 predictor variables and their performance based on log loss is evaluated. The feature transformations in this file includes encoding categorical variables with less than 21 categories into binary variables. For features with over 21 categories, only categories that have higher absolute correlation than 0.05 with predictor variable is retained. For device id and ip, most frequent categories were retained. Finally, hour variable was converted into time bins and weekdays, after which the data was split into training and test, and modeling performed. 
<br/>
**FeatureTransformation.R:** <br/><br/>
This file shows the final feature transformations in R after which the training and validation data were written to csv and imported in Python for further modeling. Changes were made to the previous file in how variables with over 21 categories are encoded. Instead of using correlation, 20 most frequent categories were obtained and encoded into binary variables. Similarly, interaction features were also created.  <br/>
<br/>
**Logitic_Boosting.ipynb:**<br/>
In this file, feature selection for logistic regression was performed and its performance was compared to that of XGBoost and LightGBM. Additionally, feature selection and parameter tuning was also performed for the boosting models. 
<br/>
**LabelEncoding:** <br/>
This file is an addition to feature transformation done in the FeatureTransformation file to include count variables and categorical ones that advanced models handle well. In this file, some count variables have been created. For instance, the frequency of using a particular site on a particular day is counted in a variable called count_app_site. Similarly, some variables have been label encoded instead of one hot encoding. This is done because advanced boosting models work well with categorical variables. <br/>
**AdvancedModels.ipynb:** <br/>
This file shows comparison of performance between the advanced models: XGBoost, LightGBM and Cat Boosting, in addition to feature selection and hyperparameter tuning for each of the models. 

