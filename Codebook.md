# Codebook for Getting and Cleaning Data Course Project

As part of creating a tidy data set for the course project, this code book is
provided.  It includes:

* _Codebook_: A description of each of the variables, including units
* _Summary choices_: Information about the summary choices
* _Study Design_: Information about the experimental study design

## Codebook

This dataset includes the mean and standard deviation variables from the
HAR study. These were selected by including all variables with "mean" or 
"std" in their name from the original dataset, and replacing certain characters
"(", ")", and "-" with an underscore "\_". Of the original 561 variables, the
following 86 met the critera for inclusion:

* tBodyAcc\_mean\_X
* tBodyAcc\_mean\_Y
* tBodyAcc\_mean\_Z
* tBodyAcc\_std\_X
* tBodyAcc\_std\_Y
* tBodyAcc\_std\_Z
* tGravityAcc\_mean\_X
* tGravityAcc\_mean\_Y
* tGravityAcc\_mean\_Z
* tGravityAcc\_std\_X
* tGravityAcc\_std\_Y
* tGravityAcc\_std\_Z
* tBodyAccJerk\_mean\_X
* tBodyAccJerk\_mean\_Y
* tBodyAccJerk\_mean\_Z
* tBodyAccJerk\_std\_X
* tBodyAccJerk\_std\_Y
* tBodyAccJerk\_std\_Z
* tBodyGyro\_mean\_X
* tBodyGyro\_mean\_Y
* tBodyGyro\_mean\_Z
* tBodyGyro\_std\_X
* tBodyGyro\_std\_Y
* tBodyGyro\_std\_Z
* tBodyGyroJerk\_mean\_X
* tBodyGyroJerk\_mean\_Y
* tBodyGyroJerk\_mean\_Z
* tBodyGyroJerk\_std\_X
* tBodyGyroJerk\_std\_Y
* tBodyGyroJerk\_std\_Z
* tBodyAccMag\_mean\_
* tBodyAccMag\_std\_
* tGravityAccMag\_mean\_
* tGravityAccMag\_std\_
* tBodyAccJerkMag\_mean\_
* tBodyAccJerkMag\_std\_
* tBodyGyroMag\_mean\_
* tBodyGyroMag\_std\_
* tBodyGyroJerkMag\_mean\_
* tBodyGyroJerkMag\_std\_
* fBodyAcc\_mean\_X
* fBodyAcc\_mean\_Y
* fBodyAcc\_mean\_Z
* fBodyAcc\_std\_X
* fBodyAcc\_std\_Y
* fBodyAcc\_std\_Z
* fBodyAcc\_meanFreq\_X
* fBodyAcc\_meanFreq\_Y
* fBodyAcc\_meanFreq\_Z
* fBodyAccJerk\_mean\_X
* fBodyAccJerk\_mean\_Y
* fBodyAccJerk\_mean\_Z
* fBodyAccJerk\_std\_X
* fBodyAccJerk\_std\_Y
* fBodyAccJerk\_std\_Z
* fBodyAccJerk\_meanFreq\_X
* fBodyAccJerk\_meanFreq\_Y
* fBodyAccJerk\_meanFreq\_Z
* fBodyGyro\_mean\_X
* fBodyGyro\_mean\_Y
* fBodyGyro\_mean\_Z
* fBodyGyro\_std\_X
* fBodyGyro\_std\_Y
* fBodyGyro\_std\_Z
* fBodyGyro\_meanFreq\_X
* fBodyGyro\_meanFreq\_Y
* fBodyGyro\_meanFreq\_Z
* fBodyAccMag\_mean\_
* fBodyAccMag\_std\_
* fBodyAccMag\_meanFreq\_
* fBodyBodyAccJerkMag\_mean\_
* fBodyBodyAccJerkMag\_std\_
* fBodyBodyAccJerkMag\_meanFreq\_
* fBodyBodyGyroMag\_mean\_
* fBodyBodyGyroMag\_std\_
* fBodyBodyGyroMag\_meanFreq\_
* fBodyBodyGyroJerkMag\_mean\_
* fBodyBodyGyroJerkMag\_std\_
* fBodyBodyGyroJerkMag\_meanFreq\_
* angle\_tBodyAccMean\_gravity\_
* angle\_tBodyAccJerkMean\_gravityMean\_
* angle\_tBodyGyroMean\_gravityMean\_
* angle\_tBodyGyroJerkMean\_gravityMean\_
* angle\_X\_gravityMean\_
* angle\_Y\_gravityMean\_
* angle\_Z\_gravityMean\_

This is a selection of variables from the HAR data, which are described in detail
in the file _features\_info.txt_ in the data set.  Quoting from that file:

> The features selected for this database come from the accelerometer and
> gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals
> (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then
> they were filtered using a median filter and a 3rd order low pass Butterworth
> filter with a corner frequency of 20 Hz to remove noise. Similarly, the
> acceleration signal was then separated into body and gravity acceleration
> signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth
> filter with a corner frequency of 0.3 Hz. 
> 
> Subsequently, the body linear acceleration and angular velocity were derived in
> time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the
> magnitude of these three-dimensional signals were calculated using the
> Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag,
> tBodyGyroJerkMag). 
> 
> Finally a Fast Fourier Transform (FFT) was applied to some of these signals
> producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag,
> fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain
> signals). 
> 
> These signals were used to estimate variables of the feature vector for each
> pattern:  '-XYZ' is used to denote 3-axial signals in the X, Y and Z
> directions.
> 
> tBodyAcc-XYZ
> tGravityAcc-XYZ
> tBodyAccJerk-XYZ
> tBodyGyro-XYZ
> tBodyGyroJerk-XYZ
> tBodyAccMag
> tGravityAccMag
> tBodyAccJerkMag
> tBodyGyroMag
> tBodyGyroJerkMag
> fBodyAcc-XYZ
> fBodyAccJerk-XYZ
> fBodyGyro-XYZ
> fBodyAccMag
> fBodyAccJerkMag
> fBodyGyroMag
> fBodyGyroJerkMag
> 
> The set of variables that were estimated from these signals are: 
> 
> mean(): Mean value
> std(): Standard deviation

In addition, two variables were integrated with the data set. A descriptive
factor for the activity observed during each measurement, and the subject
participating in
that activity.

* activity
* subject

## Summary choices

The summary made in creating this dataset based on the HAR data were dictated
primarily by the instructions of the assignment.  In particular:

> 5\. Creates a second, independent tidy data set with the average of each
>    variable for each activity and each subject.

So, each of the 86 variables above from the HAR data is averaged (mean) across
all overvations for each subject-activity pair, resulting in a total
of 180 observations (30 subjects x 6 activities).

## Study Design

This dataset merely cleans and aggregates data from the HAR project, and as
such has no tudy design of it's own.  The study design for that project is
available at:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
