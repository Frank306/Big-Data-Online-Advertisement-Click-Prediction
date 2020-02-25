# Big Data Online Advertisement Click Prediction
This project involves predicting clicks for on-line advertisements. The training data consists of
data for 9 days from October 21, 2014 to October 29, 2014. The training data is in the file
“ProjectTrainingData.csv.” There are in excess of 30 million records in this file. The variables in
this file are as follows:<br/>
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
