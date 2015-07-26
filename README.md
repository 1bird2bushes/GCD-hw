Getting & Cleaning Data Peer Assessment
=======================================

See the "Assessment Directions" for the directions from the course.

### Description
#### Downloading Data
Firstly, the data that we will be manipulating must be downloaded. The current
location of the source file is:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


#### Readying the Data
Second thing on the agenda is to get the data ready for analysis. I did this in 
a brutish way by manually unzipping the data. While I understand there is a very
easy way to incorporate an unzip, I felt it easier to do so manually.

#### Getting the Data
After this, I began to get the data into R by using ```read.table()``` and ```file.path()```.
At this point I also read the activity labels as well as the feature labels from their 
corresponding files.

The next step was to ```bind()``` the data column-wise and row-wise using ```cbind()``` 
and ```rbind()``` respectively. 

#### Extract Mean & Standard Deviation Measurements
The way I approached this problem was to initally find the data that had the word std,
mean, Subject, or ID and get rid of the rest using ```grepl()```

#### Finishing and Tidying the Data Output
Create a new tidy data set and use descriptive labels for easy reading. Write file using
```write.table()``` as a .txt file, with ```row.name=FALSE```

----
----
----

### Assessment Directions
You should create one R script called run_analysis.R that does the following:

1. Merges the training and the test sets to create one data set.
The first step in my procedure is to download the data. The process will check
to see if the zip file exists and if so it will give the user a warning message.
If not it will download the zip file.  
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names.
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#### Background
Wearable Computing Article - http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/

#### Data
Data Set - https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  
Data Origination - http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

#### Deliverables
1. Please upload a tidy data set according to the instructions in the project description. Please upload your data set as a separate file (do not cut and paste a dataset directly into the text box, as this may cause errors saving your submission).
2. Please submit a link to a Github repo with the code for performing your analysis. The code should have a file run_analysis.R in the main directory that can be run as long as the Samsung data is in your working directory. The output should be the tidy data set you submitted for part 1. You should include a README.md in the repo describing how the script works.