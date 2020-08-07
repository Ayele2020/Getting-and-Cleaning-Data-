
# libraries used for this Project 

library(data.table)
library(dplyr)

#reading features and activity data from the training dataset 

featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

#reading training  data

subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

#reading test data from the training dataset

subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

#---------------------------------------------------------------------------------------------
# Step 1 - Merge the Training Data and the Test Data to create one data set

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

colnames(features) <- t(featureNames[2])

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

#----------------------------------------------------------------------------------------------=
# Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
#list and look at the dimension of complete Data
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
#look at the dimension of requiredColumns
extractedData <- completeData[,requiredColumns]
dim(extractedData)

#------------------------------------------------------------------------------------------------
# Step 3 - Uses descriptive activity names to name the activities in the data set

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)

#------------------------------------------------------------------------------------------------
# Step 4 - Appropriately labels the data set with descriptive variable names
# get the names of the variables using names(extractedData)
# Some Acronyms replaced 

names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))

#extracted Data after they are editing names of the variables

names(extractedData)

#-----------------------------------------------------------------------------------------------
# Part 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject


extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

#create tidyData as a data set and order the enties in tidyData and write it into data file Tidy.txt that contains the processed data.

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

#END








# Getting-and-Cleaning-Data-
