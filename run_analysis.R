# Getting and Cleaning Data
# Programming Assignment 1
# Author: 1bird2bushes
# Date: 07/25/15


#Function to add a suffix
addSuffix<- function(x, suffix) {
  if (!(x %in% c("Subject","Activity"))) {
    paste(x,suffix, sep="")
  }
  else{
    x
  }
}


require(plyr)

#Get data
pathFile<-file.path(getwd(),"UCI HAR Dataset")
pathFileTest<-file.path(pathFile, "test")
pathFileTrain<-file.path(pathFile, "train")

xTest<-read.table(file.path(pathFileTest,"X_test.txt"))
yTest<-read.table(file.path(pathFileTest,"Y_test.txt"))
subjectTest<-read.table(file.path(pathFileTest,"subject_test.txt"))

xTrain<-read.table(file.path(pathFileTrain,"X_train.txt"))
yTrain<-read.table(file.path(pathFileTrain,"Y_train.txt"))
subjectTrain<-read.table(file.path(pathFileTrain,"subject_train.txt"))

#Get Activity Labels
activityLabels<-read.table(file.path(pathFile,"activity_labels.txt"),
                           col.names = c("Id", "Activity"))

#Get Features Labels
featureLabels<-read.table(file.path(pathFile,"features.txt"),
                          colClasses = c("character"))

#1.Merges the training and the test sets to create one data set.
trainData<-cbind(cbind(xTrain, subjectTrain), yTrain)
testData<-cbind(cbind(xTest, subjectTest), yTest)
sensorData<-rbind(trainData, testData)
sensorLabels<-rbind(rbind(featureLabels, c(562, "Subject")), c(563, "Id"))[,2]
names(sensorData)<-sensorLabels


#2. Extracts only the measurements on the mean and standard deviation for each measurement.
sensorDataMeanStdDev <- sensorData[,grepl("mean\\(\\)|std\\(\\)|Subject|Id", names(sensorData))]


#3. Uses descriptive activity names to name the activities in the data set
sensorDataMeanStdDev <- join(sensorDataMeanStdDev, activityLabels, by = "Id", match = "first")
sensorDataMeanStdDev <- sensorDataMeanStdDev[,-1]


#4. Appropriately labels the data set with descriptive names.
names(sensorDataMeanStdDev) <- gsub("([()])","",names(sensorDataMeanStdDev))
#norm names
names(sensorDataMeanStdDev) <- make.names(names(sensorDataMeanStdDev))


#5. From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject
finalData<-ddply(sensorDataMeanStdDev, c("Subject","Activity"), numcolwise(mean))

#improve column names
finalDataHeaders <- names(finalData)
finalDataHeaders<-sapply(finalDataHeaders, addSuffix, ".mean")
names(finalData)<-finalDataHeaders
write.table(finalData, file = "GCD_hw.txt", row.name=FALSE)