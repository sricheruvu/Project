## Getting and Cleaning Data Course Project 
## Human Activity Recognition Using Smartphones
## Sriram Cheruvu

## The purpose of this project is to demonstrate the ability to collect, 
## work with, and clean a data set. The goal is to prepare tidy data that 
## can be used for later analysis.  The following is needed: 
## 1) a tidy data set,  
## 2) a link to a Github repository with the script for performing the analysis, 
## and 3) a code book that describes the variables, the data, and any 
## transformations or work that was performed to clean up the data called 
## CodeBook.md. 

## Source of data for this project: 
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## Step 0 - Clean the R workspace. Setup and load the packages/libraries, working directory, and path
## we would be using loop functions, data table, reshape2 and potentially 
## dplyr/tidyr libraries

# Clean up workspace
rm(list=ls())

packages <- c("data.table","reshape2","dplyr","tidyr")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

## set the working directory path
path <- getwd()
path

## Step 1: Get the data from the data source. Download the file and put it in the Data folder
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "Dataset.zip"

if (!file.exists(path)) {dir.create(path)}
download.file(url, file.path(path, zipFile))

## now unzip the file (quite large) using the 7-Zip or 7z programs
executable <- file.path("C:", "Program Files (x86)", "7-Zip", "7z.exe")
parameters <- "x"
cmd <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(path, zipFile), "\""))
system(cmd)

## The archive put the files in a folder named UCI HAR Dataset. Set this folder 
## as the input path. List the files here to double check if all of them are present.

pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive=TRUE)

## Ask #1. Merge the training and the test sets to create one data set.

## To do this, first read the files into corresponding data tables
## NOTE: there are 3 categories of files: a) Subject files - these start with subject*
## b) Train files - these end with *_train.txt and c) Test data

## Read features.txt
dt_features     = read.table('./UCI HAR Dataset/features.txt',header=FALSE)
## Read activity_labels.txt
dt_activityType = read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE)
## Read subject_train.txt
dt_subjectTrain = read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE)
## Read x_train.txt
dt_xTrain       = read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE)
## Read y_train.txt
dt_yTrain       = read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE)

## Assign column names to the data imported above
colnames(dt_activityType)  = c('activityId','activityType')
colnames(dt_subjectTrain)  = "subjectId"
colnames(dt_xTrain)        = dt_features[,2] 
colnames(dt_yTrain)        = "activityId"

## Create the final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(dt_yTrain,dt_subjectTrain,dt_xTrain);

## Next read in the test data
## subject_test.txt
subjectTest = read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE)
## x_test.txt
xTest       = read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE)
## y_test.txt
yTest       = read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE)

## Assign column names to the test data imported above
colnames(subjectTest) = "subjectId"
colnames(xTest)       = dt_features[,2] 
colnames(yTest)       = "activityId"

## Create the final test set by merging the xTest, yTest and subjectTest data
testData = cbind(yTest,subjectTest,xTest)

## Combine training and test data to create a final data set
finalData = rbind(trainingData,testData)

## Create a vector for the column names from the finalData, which will be used
## to select the desired mean() & stddev() columns
colNames  = colnames(finalData)

## ASK #2. Extract only the measurements on the mean and standard deviation for each measurement. 

## Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

## Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE]

## ASK #3. Use descriptive activity names to name the activities in the data set

## Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,dt_activityType,by='activityId',all.x=TRUE)

## Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData) 

## ASK #4. Appropriately label the data set with descriptive activity names. 

## Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

## Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames

## ASK #5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

## Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType']

## Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)

## Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,dt_activityType,by='activityId',all.x=TRUE)

## Export the tidyData set 
write.table(tidyData, './DatasetHumanActivityRecognitionUsingSmartphones.txt',row.names=TRUE,sep='\t')

#knit("makeCodebook.Rmd", output="codebook.md", encoding="ISO8859-1", quiet=TRUE)
#markdownToHTML("codebook.md", "codebook.html")
