# Getting and Cleaning Data Course Project #
The purpose of this project is to demonstrate an ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. The requirement is to submit:
1. a tidy data set as described below
1. a link to a Github repository with your script for performing the analysis
1. a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 
1. a README.md in the repo with the scripts. 

## Background ##
One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

- http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

- https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## Details of the Data Set ##
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix ‘t’ to denote time) were captured at a constant rate of 50 Hz. and the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ).

The body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).

A Fast Fourier Transformation was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. 

### Basis of Data Set information ###
Experiments were carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### Abbreviations used in the Data Set field names ###
The field names of the data set are used throughout the script, so it is important the reader has an understanding of their meanings:-

- t	= a leading t means it is a time based measurement.
- f	= a leading f means is is a frequency based measurement.
- Body = related to body movement.
- Gravity = acceleration of gravity.
- Acc = accelerometer measurement.
- Gyro = gyroscopic measurements.
- Jerk = sudden movement acceleration.
- Mag = magnitude of movement.
- mean = the mean values calculated for each subject for each activity.
- SD - the standard deviation vales calculated for each subject for each activity.


## The script "run_analysis.R" ##
One R script called run_analysis.R has been created, that does the following:-
1. Merges the training and the test sets to create one data set.
1. Extracts only the measurements on the mean and standard deviation for each measurement.
1. Uses descriptive activity names to name the activities in the data set
1. Appropriately labels the data set with descriptive variable names.
1. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### Load the R libraries required for the function ###
	library(data.table)
	library(dplyr)
	library(tidyr)

### Set the working directory ###
	setwd("C:/Users/Ian/R/Rprogramming/assignment10/getdata-projectfiles-UCI HAR Dataset")

### Files in folder ‘UCI HAR Dataset’ utilised by the script ###
1. SUBJECT FILES ( test/subject_test.txt and train/subject_train.txt )
1. ACTIVITY FILES ( test/X_test.txt and train/X_train.txt )
1. DATA FILES ( test/y_test.txt and train/y_train.txt )
1. features.txt - Names of column variables in the dataTable
1. activity_labels.txt - Links the class labels with their activity name.

### 1.	Merge the training and the test sets to create one data set ###
	
#### Read the Subject, Activity and Data into respective Data Frames  ####
	df_SubjectTrain <- tbl_df(read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE))
	df_SubjectTest  <- tbl_df(read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE))
	df_ActivityTrain <- tbl_df(read.table("UCI HAR Dataset/train/Y_train.txt", header = FALSE))
	df_ActivityTest  <- tbl_df(read.table("UCI HAR Dataset/test/Y_test.txt", header = FALSE))
	df_DataTrain <- tbl_df(read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE))
	df_DataTest  <- tbl_df(read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE))

#### Separately merge the Train and Test dataframes of Subject, Activity and Data  ####
	df_AllSubject <- rbind(df_SubjectTrain, df_SubjectTest)
	df_AllActivity <- rbind(df_ActivityTrain, df_ActivityTest)
	df_AllData <- rbind(df_DataTrain , df_DataTest)

#### Read the Activity Features and Activity Labels into respective Data Frames ####
	df_DataFeatures <- tbl_df(read.table("UCI HAR Dataset/features.txt"))
	df_ActivityLabels <- tbl_df(read.table("UCI HAR Dataset/activity_labels.txt"))

#### Set variable and column names for respective Data Frames ####
	setnames(df_AllActivity, "V1", "activityNum")
	setnames(df_AllSubject, "V1", "subject")
	setnames(df_DataFeatures, names(df_DataFeatures), c('featureNum', 'featureName'))
	colnames(df_AllData ) <- df_DataFeatures$featureName
 	setnames(df_ActivityLabels , names(df_ActivityLabels), c('activityNum','activityName'))

#### Merge the columns of df_AllSubject and df_AllActivity, then merge result with df_AllData ####
	df_AllSubjectActivity <- cbind(df_AllSubject, df_AllActivity)
	df_AllData <- cbind(df_AllSubjectActivity, df_AllData)
	print('Merge of Training and Test sets completed')
	
### 2. Extract the measurements on the mean and standard deviation for each measurement ###
	df_SelectedFeatures <- grep("mean\\(\\)|std\\(\\)",df_DataFeatures$featureName,value=TRUE)
	df_SelectedFeatures <- union(c("subject","activityNum"), df_SelectedFeatures)
	df_AllData <- subset(df_AllData,select=df_SelectedFeatures)
	print('Extracts of mean and standard deviation completed')

### 3. Applies descriptive names to the activities in the data se ###t
	df_AllData <- merge(df_ActivityLabels, df_AllData , by="activityNum", all.x=TRUE)
	df_AllData$activityName <- as.character(df_AllData$activityName)

#### Create a data frame with 'mean', sorted by subject and activity ####
	df_Agrregated <- aggregate(. ~ subject - activityName, data = df_AllData, mean) 
	df_AllData <- tbl_df(arrange(df_Agrregated,subject,activityName))
	print('Application of descriptive names to activities completed')
	
#### 4.	Appropriately labels the data set with descriptive variable names. ####
The following table and code shows the translation of the variables names

- angle converts to Angle (Angle / Pitch of device)
- Acc converts to Accelerometer (Acceleration measurement)
- BodyBody converts to Body (Body movement)
- gravity converts to Gravity (Acceleration of gravity)
- Gyro converts to Gyroscope (Gyroscopic measurement)
- Mag converts to Magnitude (Magnitude of movement)
- ^t converts to Time (Time measurement)
- ^f converts to Frequency (Frequency measurement) 

#
    write.table(names(df_AllData), "old_names.txt", row.name=FALSE)	
    names(df_AllData) <-gsub("Acc", "Accelerometer", names(df_AllData))
    names(df_AllData) <-gsub("Gyro", "Gyroscope", names(df_AllData))
    names(df_AllData) <-gsub("BodyBody", "Body", names(df_AllData))
    names(df_AllData) <-gsub("Mag", "Magnitude", names(df_AllData))
    names(df_AllData)	<-gsub("^t", "Time", names(df_AllData))
    names(df_AllData)	<-gsub("^f", "Frequency", names(df_AllData))
    names(df_AllData)	<-gsub("tBody", "TimeBody", names(df_AllData))
    names(df_AllData)	<-gsub("-mean()", "Mean", names(df_AllData), ignore.case = TRUE)
    names(df_AllData)	<-gsub("-std()", "STD", names(df_AllData), ignore.case = TRUE)
    names(df_AllData)	<-gsub("-freq()", "Frequency", names(df_AllData), ignore.case = TRUE)
    names(df_AllData)	<-gsub("angle", "Angle", names(df_AllData))
    names(df_AllData)	<-gsub("gravity", "Gravity", names(df_AllData))
    write.table(names(df_AllData), "new_names.txt", row.name=FALSE)
    print('Labeling the data set with descriptive variable names completed')

### 5. From the data set in step 4, creates a second, independent 'Tidy' data set with the average of each variable for each activity and each subject. ###
	df_AllData$subject <- as.factor(df_AllData$subject)
	df_TidyData <- data.table(df_AllData)
	df_TidyData <- aggregate(. ~subject + activityName, df_TidyData, mean)
	df_TidyData <- df_TidyData[order(df_TidyData$subject,df_TidyData$activityName),]
	write.table(df_TidyData, file = "Tidy.txt", row.names = FALSE)
	print('Creation of tidy data set completed')


