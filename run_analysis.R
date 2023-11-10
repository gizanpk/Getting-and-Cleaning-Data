# Getting and Cleaning Data Project. specialization data science. John Hopkins Coursera
# Author: Giovanny Sandoval P.

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Loading required packages
library(dplyr)

#assign R working directory
setwd("C:/Users/hgupyff/Documents/Coursera/E. DATA SCIENCE/3. Getting and Cleaning Data/4 Semana/7. Project")


#Si el directorio existe, sino existe lo crea
if (!file.exists("data")) {
        dir.create("data")
}

#Download the dataset
dataset_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(dataset_url, "data/dataset.zip")
unzip("data/dataset.zip", exdir = "data/dataset")

# Checking if folder exists
list.files("data/dataset")
list.files("data/dataset/UCI HAR Dataset")

#Assigning all data frames
#data frames: variables
features <- read.table("data/dataset/UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("data/dataset/UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

#dara frames: test 
subject_test <- read.table("data/dataset/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("data/dataset/UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("data/dataset/UCI HAR Dataset/test/y_test.txt", col.names = "code")

#dara frames: train
subject_train <- read.table("data/dataset/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("data/dataset/UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("data/dataset/UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Step 1: Merges the training and the test sets to create one data set.

#Merge databases by rows (X)
X <- rbind(x_train, x_test)

#Merge databases by rows (Y)
Y <- rbind(y_train, y_test)

#Merge databases by rows (subject)
Subject <- rbind(subject_train, subject_test)

#Merge between Subject, Y, X by column
Merged_Data <- cbind(Subject, Y, X)

#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

#Step 3: Uses descriptive activity names to name the activities in the data set.
TidyData$code <- activities[TidyData$code, 2]

#Step 4: Appropriately labels the data set with descriptive variable names.
names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))

#Step 5: From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.

FinalData <- TidyData %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)

#Checking variable names
str(FinalData)

#Take a look at final data
FinalData