# 'run_analysis.R' does the following:-
#
# 1.	Merges the training and the test sets to create one data set.
# 2.	Extracts only the measurements on the mean and standard deviation for each measurement.
# 3.	Uses descriptive activity names to name the activities in the data set
# 4.	Appropriately labels the data set with descriptive variable names.
# 5.	From the data set in step 4, creates a second, independent tidy data set with the 
#	average of each variable for each activity and each subject.

	## Load the R libraries required for the function
	library(data.table)
	library(dplyr)
	library(tidyr)

	## Set the working directory 
	setwd("C:/Users/Ian/R/Rprogramming/assignment10/getdata-projectfiles-UCI HAR Dataset")

# 1.	Merge the training and the test sets to create one data set.
	
	## Read the Subject, Activity and Data into respective Data Frames 
	df_SubjectTrain <- tbl_df(read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE))
	df_SubjectTest  <- tbl_df(read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE))
	df_ActivityTrain <- tbl_df(read.table("UCI HAR Dataset/train/Y_train.txt", header = FALSE))
	df_ActivityTest  <- tbl_df(read.table("UCI HAR Dataset/test/Y_test.txt", header = FALSE))
	df_DataTrain <- tbl_df(read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE))
	df_DataTest  <- tbl_df(read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE))

	## Separately merge the Train and Test dataframes of Subject, Activity and Data 
	df_AllSubject <- rbind(df_SubjectTrain, df_SubjectTest)
	df_AllActivity <- rbind(df_ActivityTrain, df_ActivityTest)
	df_AllData <- rbind(df_DataTrain , df_DataTest)

	## Read the Activity Features and Activity Labels into respective Data Frames
	df_DataFeatures <- tbl_df(read.table("UCI HAR Dataset/features.txt"))
	df_ActivityLabels <- tbl_df(read.table("UCI HAR Dataset/activity_labels.txt"))

	## Set variable and column names for respective Data Frames
	setnames(df_AllActivity, "V1", "activityNum")
	setnames(df_AllSubject, "V1", "subject")
	setnames(df_DataFeatures, names(df_DataFeatures), c('featureNum', 'featureName'))
	colnames(df_AllData ) <- df_DataFeatures$featureName
 	setnames(df_ActivityLabels , names(df_ActivityLabels), c('activityNum','activityName'))

	## Merge the columns of df_AllSubject and df_AllActivity, then merge result with df_AllData
	df_AllSubjectActivity <- cbind(df_AllSubject, df_AllActivity)
	df_AllData <- cbind(df_AllSubjectActivity, df_AllData)
	print('Merge of Training and Test sets completed')
	
# 2.	Extract the measurements on the mean and standard deviation for each measurement
	df_SelectedFeatures <- grep("mean\\(\\)|std\\(\\)",df_DataFeatures$featureName,value=TRUE)
	df_SelectedFeatures <- union(c("subject","activityNum"), df_SelectedFeatures)
	df_AllData <- subset(df_AllData,select=df_SelectedFeatures)
	print('Extracts of mean and standard deviation completed')

# 3.	Applies descriptive names to the activities in the data set
	df_AllData <- merge(df_ActivityLabels, df_AllData , by="activityNum", all.x=TRUE)
	df_AllData$activityName <- as.character(df_AllData$activityName)

	## Create a data frame with 'mean', sorted by subject and activity
	df_Agrregated <- aggregate(. ~ subject - activityName, data = df_AllData, mean) 
	df_AllData <- tbl_df(arrange(df_Agrregated,subject,activityName))
	print('Application of descriptive names to activities completed')
	
# 4.	Appropriately labels the data set with descriptive variable names.
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

# 5.	From the data set in step 4, creates a second, independent 'Tidy' data set with the 
#	average of each variable for each activity and each subject.
	df_AllData$subject <- as.factor(df_AllData$subject)
	df_TidyData <- data.table(df_AllData)
	df_TidyData <- aggregate(. ~subject + activityName, df_TidyData, mean)
	df_TidyData <- df_TidyData[order(df_TidyData$subject,df_TidyData$activityName),]
	write.table(df_TidyData, file = "Tidy.txt", row.names = FALSE)
	print('Creation of tidy data set completed')

	