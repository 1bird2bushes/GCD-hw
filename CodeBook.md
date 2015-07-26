## Code Book

This section contains specific information each variable (column) included in the uploaded table, HAR-subject-activity-mean.txt.  Most of the variables are summarizations of variables with the same name and description in the Human Activity Recognition Using Smartphones Dataset (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).  The variables are described in the `features_info.txt` and `README.txt` in that dataset.  Please see those files for more details.  

Units (according to the `README.txt` file):

- For all accelerometer-related variables (containing the word "Acc" in the variable name), the units are in standard gravity units 'g'.
- For all gyroscope-related variables (containing the word "Gyro" in the variable name), the units are radians/second.

Selected variable name and description:

- subject: The integer id of one of the 30 subjects, ranging from 1 to 30.
- activity: The type of activity the subject was engaged in.  One of WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
- subject-activity-mean-tBodyAcc-mean()-X: The mean of all time window values for each combination of subject and activity for the variable tBodyAcc-mean()-X.
- subject-activity-mean-tBodyAcc-mean()-Y: The mean of all time window values for each combination of subject and activity for the variable tBodyAcc-mean()-Y.
- subject-activity-mean-tBodyAcc-mean()-Z: The mean of all time window values for each combination of subject and activity for the variable tBodyAcc-mean()-Z.
- subject-activity-mean-tBodyAcc-std()-X: The mean of all time window values for each combination of subject and activity for the variable tBodyAcc-std()-X.
- subject-activity-mean-tBodyAcc-std()-Y: The mean of all time window values for each combination of subject and activity for the variable tBodyAcc-std()-Y.
- subject-activity-mean-tBodyAcc-std()-Z: The mean of all time window values for each combination of subject and activity for the variable tBodyAcc-std()-Z.
- subject-activity-mean-tGravityAcc-mean()-X: The mean of all time window values for each combination of subject and activity for the variable tGravityAcc-mean()-X.
- subject-activity-mean-tGravityAcc-mean()-Y: The mean of all time window values for each combination of subject and activity for the variable tGravityAcc-mean()-Y.
- subject-activity-mean-tGravityAcc-mean()-Z: The mean of all time window values for each combination of subject and activity for the variable tGravityAcc-mean()-Z.
- subject-activity-mean-tGravityAcc-std()-X: The mean of all time window values for each combination of subject and activity for the variable tGravityAcc-std()-X.
- subject-activity-mean-tGravityAcc-std()-Y: The mean of all time window values for each combination of subject and activity for the variable tGravityAcc-std()-Y.
- subject-activity-mean-tGravityAcc-std()-Z: The mean of all time window values for each combination of subject and activity for the variable tGravityAcc-std()-Z.
- subject-activity-mean-tBodyAccJerk-mean()-X: The mean of all time window values for each combination of subject and activity for the variable tBodyAccJerk-mean()-X.
- subject-activity-mean-tBodyAccJerk-mean()-Y: The mean of all time window values for each combination of subject and activity for the variable tBodyAccJerk-mean()-Y.
- subject-activity-mean-tBodyAccJerk-mean()-Z: The mean of all time window values for each combination of subject and activity for the variable tBodyAccJerk-mean()-Z.
- subject-activity-mean-tBodyAccJerk-std()-X: The mean of all time window values for each combination of subject and activity for the variable tBodyAccJerk-std()-X.
- subject-activity-mean-tBodyAccJerk-std()-Y: The mean of all time window values for each combination of subject and activity for the variable tBodyAccJerk-std()-Y.
- subject-activity-mean-tBodyAccJerk-std()-Z: The mean of all time window values for each combination of subject and activity for the variable tBodyAccJerk-std()-Z.
- subject-activity-mean-tBodyGyro-mean()-X: The mean of all time window values for each combination of subject and activity for the variable tBodyGyro-mean()-X.
- subject-activity-mean-tBodyGyro-mean()-Y: The mean of all time window values for each combination of subject and activity for the variable tBodyGyro-mean()-Y.
- subject-activity-mean-tBodyGyro-mean()-Z: The mean of all time window values for each combination of subject and activity for the variable tBodyGyro-mean()-Z.
- subject-activity-mean-tBodyGyro-std()-X: The mean of all time window values for each combination of subject and activity for the variable tBodyGyro-std()-X.
- subject-activity-mean-tBodyGyro-std()-Y: The mean of all time window values for each combination of subject and activity for the variable tBodyGyro-std()-Y.
- subject-activity-mean-tBodyGyro-std()-Z: The mean of all time window values for each combination of subject and activity for the variable tBodyGyro-std()-Z.
- subject-activity-mean-tBodyGyroJerk-mean()-X: The mean of all time window values for each combination of subject and activity for the variable tBodyGyroJerk-mean()-X.
- subject-activity-mean-tBodyGyroJerk-mean()-Y: The mean of all time window values for each combination of subject and activity for the variable tBodyGyroJerk-mean()-Y.
- subject-activity-mean-tBodyGyroJerk-mean()-Z: The mean of all time window values for each combination of subject and activity for the variable tBodyGyroJerk-mean()-Z.
- subject-activity-mean-tBodyGyroJerk-std()-X: The mean of all time window values for each combination of subject and activity for the variable tBodyGyroJerk-std()-X.
- subject-activity-mean-tBodyGyroJerk-std()-Y: The mean of all time window values for each combination of subject and activity for the variable tBodyGyroJerk-std()-Y.
- subject-activity-mean-tBodyGyroJerk-std()-Z: The mean of all time window values for each combination of subject and activity for the variable tBodyGyroJerk-std()-Z.
- subject-activity-mean-tBodyAccMag-mean(): The mean of all time window values for each combination of subject and activity for the variable tBodyAccMag-mean().
- subject-activity-mean-tBodyAccMag-std(): The mean of all time window values for each combination of subject and activity for the variable tBodyAccMag-std().
- subject-activity-mean-tGravityAccMag-mean(): The mean of all time window values for each combination of subject and activity for the variable tGravityAccMag-mean().
- subject-activity-mean-tGravityAccMag-std(): The mean of all time window values for each combination of subject and activity for the variable tGravityAccMag-std().
- subject-activity-mean-tBodyAccJerkMag-mean(): The mean of all time window values for each combination of subject and activity for the variable tBodyAccJerkMag-mean().
- subject-activity-mean-tBodyAccJerkMag-std(): The mean of all time window values for each combination of subject and activity for the variable tBodyAccJerkMag-std().
- subject-activity-mean-tBodyGyroMag-mean(): The mean of all time window values for each combination of subject and activity for the variable tBodyGyroMag-mean().
- subject-activity-mean-tBodyGyroMag-std(): The mean of all time window values for each combination of subject and activity for the variable tBodyGyroMag-std().
- subject-activity-mean-tBodyGyroJerkMag-mean(): The mean of all time window values for each combination of subject and activity for the variable tBodyGyroJerkMag-mean().
- subject-activity-mean-tBodyGyroJerkMag-std(): The mean of all time window values for each combination of subject and activity for the variable tBodyGyroJerkMag-std().
- subject-activity-mean-fBodyAcc-mean()-X: The mean of all time window values for each combination of subject and activity for the variable fBodyAcc-mean()-X.
- subject-activity-mean-fBodyAcc-mean()-Y: The mean of all time window values for each combination of subject and activity for the variable fBodyAcc-mean()-Y.
- subject-activity-mean-fBodyAcc-mean()-Z: The mean of all time window values for each combination of subject and activity for the variable fBodyAcc-mean()-Z.
- subject-activity-mean-fBodyAcc-std()-X: The mean of all time window values for each combination of subject and activity for the variable fBodyAcc-std()-X.
- subject-activity-mean-fBodyAcc-std()-Y: The mean of all time window values for each combination of subject and activity for the variable fBodyAcc-std()-Y.
- subject-activity-mean-fBodyAcc-std()-Z: The mean of all time window values for each combination of subject and activity for the variable fBodyAcc-std()-Z.
- subject-activity-mean-fBodyAcc-meanFreq()-X: The mean of all time window values for each combination of subject and activity for the variable fBodyAcc-meanFreq()-X.
- subject-activity-mean-fBodyAcc-meanFreq()-Y: The mean of all time window values for each combination of subject and activity for the variable fBodyAcc-meanFreq()-Y.
- subject-activity-mean-fBodyAcc-meanFreq()-Z: The mean of all time window values for each combination of subject and activity for the variable fBodyAcc-meanFreq()-Z.
- subject-activity-mean-fBodyAccJerk-mean()-X: The mean of all time window values for each combination of subject and activity for the variable fBodyAccJerk-mean()-X.
- subject-activity-mean-fBodyAccJerk-mean()-Y: The mean of all time window values for each combination of subject and activity for the variable fBodyAccJerk-mean()-Y.
- subject-activity-mean-fBodyAccJerk-mean()-Z: The mean of all time window values for each combination of subject and activity for the variable fBodyAccJerk-mean()-Z.
- subject-activity-mean-fBodyAccJerk-std()-X: The mean of all time window values for each combination of subject and activity for the variable fBodyAccJerk-std()-X.
- subject-activity-mean-fBodyAccJerk-std()-Y: The mean of all time window values for each combination of subject and activity for the variable fBodyAccJerk-std()-Y.
- subject-activity-mean-fBodyAccJerk-std()-Z: The mean of all time window values for each combination of subject and activity for the variable fBodyAccJerk-std()-Z.
- subject-activity-mean-fBodyAccJerk-meanFreq()-X: The mean of all time window values for each combination of subject and activity for the variable fBodyAccJerk-meanFreq()-X.
- subject-activity-mean-fBodyAccJerk-meanFreq()-Y: The mean of all time window values for each combination of subject and activity for the variable fBodyAccJerk-meanFreq()-Y.
- subject-activity-mean-fBodyAccJerk-meanFreq()-Z: The mean of all time window values for each combination of subject and activity for the variable fBodyAccJerk-meanFreq()-Z.
- subject-activity-mean-fBodyGyro-mean()-X: The mean of all time window values for each combination of subject and activity for the variable fBodyGyro-mean()-X.
- subject-activity-mean-fBodyGyro-mean()-Y: The mean of all time window values for each combination of subject and activity for the variable fBodyGyro-mean()-Y.
- subject-activity-mean-fBodyGyro-mean()-Z: The mean of all time window values for each combination of subject and activity for the variable fBodyGyro-mean()-Z.
- subject-activity-mean-fBodyGyro-std()-X: The mean of all time window values for each combination of subject and activity for the variable fBodyGyro-std()-X.
- subject-activity-mean-fBodyGyro-std()-Y: The mean of all time window values for each combination of subject and activity for the variable fBodyGyro-std()-Y.
- subject-activity-mean-fBodyGyro-std()-Z: The mean of all time window values for each combination of subject and activity for the variable fBodyGyro-std()-Z.
- subject-activity-mean-fBodyGyro-meanFreq()-X: The mean of all time window values for each combination of subject and activity for the variable fBodyGyro-meanFreq()-X.
- subject-activity-mean-fBodyGyro-meanFreq()-Y: The mean of all time window values for each combination of subject and activity for the variable fBodyGyro-meanFreq()-Y.
- subject-activity-mean-fBodyGyro-meanFreq()-Z: The mean of all time window values for each combination of subject and activity for the variable fBodyGyro-meanFreq()-Z.
- subject-activity-mean-fBodyAccMag-mean(): The mean of all time window values for each combination of subject and activity for the variable fBodyAccMag-mean().
- subject-activity-mean-fBodyAccMag-std(): The mean of all time window values for each combination of subject and activity for the variable fBodyAccMag-std().
- subject-activity-mean-fBodyAccMag-meanFreq(): The mean of all time window values for each combination of subject and activity for the variable fBodyAccMag-meanFreq().
- subject-activity-mean-fBodyBodyAccJerkMag-mean(): The mean of all time window values for each combination of subject and activity for the variable fBodyBodyAccJerkMag-mean().
- subject-activity-mean-fBodyBodyAccJerkMag-std(): The mean of all time window values for each combination of subject and activity for the variable fBodyBodyAccJerkMag-std().
- subject-activity-mean-fBodyBodyAccJerkMag-meanFreq(): The mean of all time window values for each combination of subject and activity for the variable fBodyBodyAccJerkMag-meanFreq().
- subject-activity-mean-fBodyBodyGyroMag-mean(): The mean of all time window values for each combination of subject and activity for the variable fBodyBodyGyroMag-mean().
- subject-activity-mean-fBodyBodyGyroMag-std(): The mean of all time window values for each combination of subject and activity for the variable fBodyBodyGyroMag-std().
- subject-activity-mean-fBodyBodyGyroMag-meanFreq(): The mean of all time window values for each combination of subject and activity for the variable fBodyBodyGyroMag-meanFreq().
- subject-activity-mean-fBodyBodyGyroJerkMag-mean(): The mean of all time window values for each combination of subject and activity for the variable fBodyBodyGyroJerkMag-mean().
- subject-activity-mean-fBodyBodyGyroJerkMag-std(): The mean of all time window values for each combination of subject and activity for the variable fBodyBodyGyroJerkMag-std().
- subject-activity-mean-fBodyBodyGyroJerkMag-meanFreq(): The mean of all time window values for each combination of subject and activity for the variable fBodyBodyGyroJerkMag-meanFreq().

Quoting from `features_info.txt`, here is an additional description of the variables:

> The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filt er with a corner frequency of 0.3 Hz. 
> 
> Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
> 
> Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
> 
> These signals were used to estimate variables of the feature vector for each pattern:  '-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
> 
> - tBodyAcc-XYZ
> - tGravityAcc-XYZ
> - tBodyAccJerk-XYZ
> - tBodyGyro-XYZ
> - tBodyGyroJerk-XYZ
> - tBodyAccMag
> - tGravityAccMag
> - tBodyAccJerkMag
> - tBodyGyroMag
> - tBodyGyroJerkMag
> - fBodyAcc-XYZ
> - fBodyAccJerk-XYZ
> - fBodyGyro-XYZ
> - fBodyAccMag
> - fBodyAccJerkMag
> - fBodyGyroMag
> - fBodyGyroJerkMag
> 
> The set of variables that were estimated from these signals are: 
> 
> - mean(): Mean value
> - std(): Standard deviation
> - meanFreq(): Weighted average of the frequency components to obtain a mean frequency
 