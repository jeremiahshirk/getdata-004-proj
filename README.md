# Course Project for Getting and Cleaning Data

This repository contains three items required for the course project:

* README.md
* Codebook
* run_analysis.R

In addition, a tidy data set was uploaded along with this URL during the
assignment submission.

## README

This file

## Codebook

The Codebook contains:

* A description of each of the variables, including units
* Information about the summary choices
* Information about the experimental study design

## run_analysis.R

This script assumes that the raw data from the _Human Activity Recognition
Using Smartphones_ is downloaded and unzipped in the working directory along
with run_analysis.R.

The HAR project data is described at:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The raw data to download and unzip is at:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

### Steps to use run_analysis.R

1. Download the HAR dataset
1. Unzip the dataset
1. Ensure that the unzipped directory 'UCI HAR Dataset' is in your working
   directory, along with run_analysis.R
1. Execute run_analysis.R. It is known to work with R version 3.1.0
   (2014-04-10), _Spring Dance_

### Assignment criteria

To meet the assignment critera, run_analysis.R does the following (quoting from
the assignment):

> 1. Merges the training and the test sets to create one data set.
> 1. Extracts only the measurements on the mean and standard deviation for each
>    measurement. 
> 1. Uses descriptive activity names to name the activities in the data set
> 1. Appropriately labels the data set with descriptive variable names. 
> 1. Creates a second, independent tidy data set with the average of each
>    variable for each activity and each subject. 

These steps are also noted in the comments in the script.

## Tidy data set

Running the script in the same directory with the raw data will output a tidy
data set.  It is tidy, and adheres to the four principles outlined at:
https://github.com/jtleek/datasharing#the-tidy-data-set

* Each measured variable is in one column
* Each observation is in a different row
* There is one table for each kind of variable (in this case, there is only one
  kind)

The file created, _tidy.txt_, is tab-delimited with an initial row containing
variable names.
