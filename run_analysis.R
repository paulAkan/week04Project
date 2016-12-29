library(dplyr)
library(tidyr)
library(downloader)
library(reshape2)

#############################################
#### DOWNLOAD AND UNZIP FILES
############################################

#url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#download(url, dest="./Dataset.zip", mode="wb")

#unzip(zipfile = "./Dataset.zip", exdir = "./")
#
############################################
#### FILES LOCATED IN UCI HAR DATASET
###########################################

dirData <- "UCI HAR Dataset"
feature <- paste(dirData, "/features.txt", sep = "")
activity_labels <- paste(dirData, "/activity_labels.txt", sep = "")
x_train <- paste(dirData, "/train/X_train.txt", sep = "")
y_train <- paste(dirData, "/train/y_train.txt", sep = "")
subject_train <- paste(dirData, "/train/subject_train.txt", sep = "")
x_test  <- paste(dirData, "/test/X_test.txt", sep = "")
y_test  <- paste(dirData, "/test/y_test.txt", sep = "")
subject_test <- paste(dirData, "/test/subject_test.txt", sep = "")

############################################
#### LOAD DATASET INTO MEMORY
###########################################

features <- tbl_df(read.table(feature))
activityLabels <- tbl_df(read.table(activity_labels))

featureUsed <- grep(".*mean.*|.*std.*", features$V2, value = FALSE)
#RETURNS THE INDICES
#
featureUsed.names <- grep(".*mean.*|.*std.*", features$V2, value = TRUE)
#value= TRUE returns the actual names
#
#REPLACE CHARACTERS
featureUsed.names = gsub('-mean', 'Mean', featureUsed.names)
featureUsed.names = gsub('-std', 'Std', featureUsed.names)
featureUsed.names <- gsub('[-()]', '', featureUsed.names)


#select x_train based on indices in featureUsed
x_train <- tbl_df(read.table(x_train)[featureUsed])
y_train <- tbl_df(read.table(y_train))
subject_train <- tbl_df(read.table(subject_train))
trainData <- cbind(subject_train, y_train, x_train)

x_test <- tbl_df(read.table(x_test)[featureUsed])
y_test <- tbl_df(read.table(y_test))
subject_test <- tbl_df(read.table(subject_test))
testData <- cbind(subject_test, y_test, x_test)


############################################
#### MERGE TRAINING AND TESTS DATASET
###########################################

combinedData <- rbind(trainData, testData)

### USE DESCRIPTIVE LABELS
featureUsed.names<-gsub("Acc", "Accelerometer", featureUsed.names)
featureUsed.names <- gsub('GyroJerk',"AngularAcceleration",featureUsed.names)
featureUsed.names <- gsub('Gyro',"AngularSpeed",featureUsed.names)
featureUsed.names <- gsub('Mag',"Magnitude",featureUsed.names)
featureUsed.names <- gsub('^t',"TimeDomain.",featureUsed.names)
featureUsed.names <- gsub('^f',"FrequencyDomain.",featureUsed.names)
featureUsed.names <- gsub('\\.mean',".Mean",featureUsed.names)
featureUsed.names <- gsub('\\.std',".StandardDeviation",featureUsed.names)
featureUsed.names <- gsub('Freq\\.',"Frequency.",featureUsed.names)
featureUsed.names <- gsub('Freq$',"Frequency",featureUsed.names)

#assign column names to combinedData
names(combinedData) <- c("subject", "activity", featureUsed.names)

#subjects and activities uses numbers. Assign as factors
#assign levels and corresponding labels
combinedData$activity <- factor(combinedData$activity, levels = activityLabels$V1, labels = activityLabels$V2)
combinedData$subject <- as.factor(combinedData$subject)

#tidy data
#combinedDataGathered<-combinedData%>%gather(variable, value, TimeDomain.BodyAccelerometerMeanX:FrequencyDomain.BodyBodyAngularAccelerationMagnitudeMeanFrequency)
combinedData.melted <- melt(combinedData, id = c("subject", "activity"))
combinedData.mean <- dcast(combinedData.melted, subject + activity ~ variable, mean)

#write to file
write.table(combinedData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)
