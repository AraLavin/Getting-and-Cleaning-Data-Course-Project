# This R script called run_analysis.R does the following.
#0.Preprocessing 
#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement.
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names.
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#NOTE: THIS SCRIPT REQUIRES THE PREVIOUS INSTALLATION OF THE PACKAGES dplyr AND data.table

###### 0: PRE PROCESSING - Getting the Data 
  
  #Downloads the Dataset
url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "dataset.zip")
  #Decompress the Dataset
unzip("dataset.zip", files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, exdir = ".",
      unzip = "internal", setTimes = FALSE)

  #Loads supporting data 
feature_names = read.table("UCI HAR Dataset/features.txt")
activity_labels = read.table("UCI HAR Dataset/activity_labels.txt")
  #Loads train set
train_set = read.table("UCI HAR Dataset/train/X_train.txt")
train_labels = read.table("UCI HAR Dataset/train/y_train.txt")
train_subject = read.table("UCI HAR Dataset/train/subject_train.txt")
  #Loads test set
test_set = read.table("UCI HAR Dataset/test/X_test.txt")
test_labels = read.table("UCI HAR Dataset/test/y_test.txt")
test_subject = read.table("UCI HAR Dataset/test/subject_test.txt")

######STEP 1: Merges the training and the test sets to create one data set.

  #Makes one data.table from the test and training data
subjects <- rbind(train_subject, test_subject)
activities <- rbind(train_labels, test_labels) 
features <- rbind(train_set, test_set)
 
 #Names the columns of features with the names in features_names column 2
colnames(features) <- t(feature_names[2])
 #Name the only column of activities as "Activity
colnames(activities) <- "Activity"
  #Name the only column of subjects as "subject"
colnames(subjects) <- "subject"
  #Creates the whole data set (merge in one data set)
whole_dataset <- cbind(features,activities,subjects)

######End STEP 1

######STEP 2: Extracts only the measurements on the mean and standard deviation for each measurement.
  
  #Identify the columns from the whole_dataset wich have mean or std in the name of the column and adds activity and subject
cols_mean_std<-c(grep("([sS][tT][dD])|([mM][eE][aA][nN])", names(whole_dataset)),562,563)
  #Creates the data set with the identified columns 
extracted_data <- whole_dataset[,cols_mean_std]
  
######End STEP 2

######STEP 3: #Uses descriptive activity names to name the activities in the data set
  
  #Replaces the activity code with the activity name
extracted_data$Activity = as.character(extracted_data$Activity)
for (i in 1:6){
  extracted_data$Activity[extracted_data$Activity == i] = as.character(activity_labels[i,2])
}


######End STEP 3

######STEP 4:Appropriately labels the data set with descriptive variable names.
  
  #Replaces the names of variables in order to make them descriptive
  #Replaces Acc with Accelerometer
names(extracted_data) = gsub("Acc", "Accelerometer", names(extracted_data))
  #Replaces Gyro with Gyroscope
names(extracted_data) = gsub("Gyro", "Gyroscope", names(extracted_data))
  #Replaces BodyBody with Body
names(extracted_data) = gsub("BodyBody", "Body", names(extracted_data))
  #Replaces Mag with Magnitude
names(extracted_data) = gsub("Mag", "Magnitude", names(extracted_data))
  #Replaces 't' with Time
names(extracted_data) = gsub("^t", "Time", names(extracted_data))
  #Replaces 'f'  with Frequency
names(extracted_data) = gsub("^f", "Frequency", names(extracted_data))
  #Replaces tBody with TimeBody
names(extracted_data) = gsub("tBody", "TimeBody", names(extracted_data))
  #Replaces -mean() with Mean
names(extracted_data) = gsub("-mean()", "Mean", names(extracted_data), ignore.case = TRUE)
  #Replaces -std() with STD
names(extracted_data) = gsub("-std()", "STD", names(extracted_data), ignore.case = TRUE)
  #Replaces -freq() with Frequency
names(extracted_data) = gsub("-freq()", "Frequency", names(extracted_data), ignore.case = TRUE)
  #Replaces angle with Angle
names(extracted_data) = gsub("angle", "Angle", names(extracted_data))
  #Replaces gravity with Gravity
names(extracted_data) = gsub("gravity", "Gravity", names(extracted_data))

######End Step 4

######STEP 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

  #Create tidy_data with mean for each activity and subject
tidy_data = aggregate(. ~subject + Activity, extracted_data, mean)

  #Write text file with tidy_data 
write.table(tidy_data, file = "Tidy.txt", row.names = FALSE)

######End Step5

  #Look at the created file
data <- read.table("Tidy.txt", header = TRUE)
View(data)

