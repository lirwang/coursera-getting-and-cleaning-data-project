getwd()
setwd("C:/Users/Whalie/Dropbox/learn data science/Data cleaning/w4/")

#download and unzip file
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("UCIHARdata.zip")){
      download.file(fileUrl, destfile="./UCIHARdata.zip")
}

if(!file.exists("UCI HAR dataset")){
      unzip("UCIHARdata.zip")
}


#####################################################
#Step 0: read the txt files into data tables
#####################################################

#####################################################
#Step 0A: read the training data
#####################################################
trainingsubject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
trainingvalue <- read.table("./UCI HAR Dataset/train/X_train.txt")
traininglabels <- read.table("./UCI HAR Dataset/train/y_train.txt")

#####################################################
#Step 0B: read the test data
#####################################################
testsubject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
testvalue <- read.table("./UCI HAR Dataset/test/X_test.txt")
testlabels <- read.table("./UCI HAR Dataset/test/y_test.txt")


#####################################################
#Step 0C: read the features
#####################################################
features <- read.table("./UCI HAR Dataset/features.txt", as.is = TRUE)
#note: not all features have unique names


#####################################################
#Step 0d: read the activity labels
#####################################################
activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
colnames(activities) <- c("activityID", "activitylabel")


#####################################################
#Step 1: merge the training and test datasets 
#####################################################

#####################################################
#Step 1A: bind the training subjects, data and labels
#####################################################
trainingdata <- cbind(trainingsubject, trainingvalue, traininglabels)

#####################################################
#Step 1B: bind the test subjects, data, and labels
#####################################################
testdata <- cbind(testsubject, testvalue, testlabels)

#####################################################
#Step 1C: merge the two datasets
#####################################################
humanactivity <- rbind(trainingdata, testdata)
colnames(humanactivity) <- c("subject", features[,2], "activity")




#####################################################
#Step 2: Extract only those measurements on the 
#means and standard deviations for each measurement
#####################################################
#create an object that contains the number of columns to be kept
columnstokeep <- grep("subject|activity|mean|std", colnames(humanactivity))
#just checking to see what are the numbers of columns to be kept
columnstokeep
#keep only those columns (create a new file so if any mistake happens it's easier to fix)
humanactivity2 <- humanactivity[, columnstokeep]



#####################################################
#Step 3: Use descriptive activity names to name the 
#activities in the data set
#####################################################
humanactivity2$activity <-factor(humanactivity$activity,
                                levels = activities[,1], labels = activities[,2])




#####################################################
#Step 4: Appropriately labels the data set with 
#descriptive variable names
#####################################################
# get column names
humanactivity2column <- colnames(humanactivity2)

# remove special characters
humanactivity2column <- gsub("[\\(\\)-]", "", humanactivity2column)

# expand abbreviations and clean up names
humanactivity2column <- gsub("^f", "frequencyDomain", humanactivity2column)
humanactivity2column <- gsub("^t", "timeDomain", humanactivity2column)
humanactivity2column <- gsub("Acc", "Accelerometer", humanactivity2column)
humanactivity2column <- gsub("Gyro", "Gyroscope", humanactivity2column)
humanactivity2column <- gsub("Mag", "Magnitude", humanactivity2column)
humanactivity2column <- gsub("Freq", "Frequency", humanactivity2column)
humanactivity2column <- gsub("mean", "Mean", humanactivity2column)
humanactivity2column <- gsub("std", "StandardDeviation", humanactivity2column)

# correct typo
humanactivity2column <- gsub("BodyBody", "Body", humanactivity2column)

# use new labels as column names
colnames(humanactivity2) <- humanactivity2column



########################################################
#From the data set in step 4, creates a second, 
#independent tidy data set with the average of each 
#variable for each activity and each subject.
########################################################

#group by activity and subject variable
#summarize with mean
library(dplyr)
humanactivity2mean <- humanactivity2 %>%
      group_by(subject, activity) %>%
      summarize_each(funs(mean))

#write the result into file "tidy.txt"
write.table(humanactivity2mean, "tidy.txt", row.names = FALSE, quote = FALSE)
