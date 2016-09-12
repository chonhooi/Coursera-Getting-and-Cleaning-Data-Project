## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Import necessary library
library(data.table)
library(plyr)

## Declare the necessary data
testActivityData <- read.table("./UCI HAR Dataset/test/Y_test.txt", header = FALSE)
trainActivityData <- read.table("./UCI HAR Dataset/train/Y_train.txt", header = FALSE)

testSubjectData <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
trainSubjectData <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)

testFeaturesData <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
trainFeaturesData <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)

featuresNamesData <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)[,2]


## Merges the training and the test sets to create one data set.
subjectData <- rbind(testSubjectData, trainSubjectData)
activityData <- rbind(testActivityData, trainActivityData)
featuresData <- rbind(testFeaturesData, trainFeaturesData)

# rename variables
names(subjectData) <- "subject"
names(activityData) <- "activity"
names(featuresData) <- featuresNamesData

# merge into one data set
data <- cbind(featuresData, subjectData, activityData)


## Extracts only the measurements on the mean and standard deviation for each measurement.
subFeaturesNamesData <- featuresNamesData[grep("mean\\(\\)|std\\(\\)", featuresNamesData)]
data <- subset(data, select = c(as.character(subFeaturesNamesData), "subject", "activity"))


## Uses descriptive activity names to name the activities in the data set
activityLabels <- read.table("./Course3/Week4/UCI HAR Dataset/activity_labels.txt")
data$activity <- sapply(data$activity, function (activityId) {
    activityLabels$V2[activityLabels$V1 == activityId]
})


## Appropriately labels the data set with descriptive variable names.
names(data)<-gsub("^t", "time", names(data))
names(data)<-gsub("^f", "frequency", names(data))
names(data)<-gsub("Acc", "Accelerometer", names(data))
names(data)<-gsub("Gyro", "Gyroscope", names(data))
names(data)<-gsub("Mag", "Magnitude", names(data))
names(data)<-gsub("BodyBody", "Body", names(data))


## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
newData <- aggregate(. ~ subject +activity, data, mean)
newData <- newData[order(newData$subject, newData$activity),]
write.table(newData, file = "tidydata.txt", row.names = FALSE)
