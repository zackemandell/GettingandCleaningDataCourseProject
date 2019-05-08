
dir <- setwd("C:/Users/zmandell/Desktop/Coursera/Cleaning Data - Course 3/Data/")


###Packages Used###
library(tidyr)
library(dplyr)


####Data Ingestion
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#destination zip
destfile<-paste0(getwd(),"/","dataweek4.zip")
#download zip file
download.file(url,destfile)
#unzip file
unzip("dataweek4.zip",list = TRUE)


## Read tables into R ##
features<-read.table("features.txt",header = FALSE)
train_data<-read.table("train/X_train.txt",header = FALSE)
test_data<-read.table("test/X_test.txt",header = FALSE)
train_label<-read.table("train/y_train.txt",header = FALSE)
test_label<-read.table("test/y_test.txt",header = FALSE)
activity_label<-read.table("activity_labels.txt",header = FALSE)
subject_train<-read.table("train/subject_train.txt",header = FALSE)
subject_test<-read.table("test/subject_test.txt",header = FALSE)


## Join test data with train data ##
total <- rbind(test_data,train_data)

## Assign Column Names to Total from Features Table ##
colnames(total) <- c(as.character(features[,2]))

## Join Test and Train Subject Identifiers and name column 'Subjects'##
total_subjects <- rbind(subject_test,subject_train)
colnames(total_subjects) <- c("Subjects")

## Bind subjects to data ## 
full_table <- cbind(total_subjects,total)

## Build flags and select columns that only include mean() or std() ##
mean_data_flag <- grep("mean()",colnames(full_table),fixed=TRUE)
SD_data_flag <- grep("std()",colnames(full_table),fixed=TRUE)
MeanSD <-full_table[,c(mean_data_flag,SD_data_flag)]


## Add Activity code to Test and Train data, labeling Column as Activity ## 
activity_data_tables <- rbind(test_label,train_label)
total_activity <- cbind(activity_data_tables,total_subjects,MeanSD)
colnames(total_activity)[1]<-"Activity"

## Replace Activity code with Activity Label ## 
activity_label[,2]<-as.character(activity_label[,2])
for(i in 1:length(total_activity[,1])){
  total_activity[i,1]<-activity_label[total_activity[i,1],2]
}


##  Build tidy data set of mean of each variable by Subject, Activity ## 
tidy <- aggregate(total_activity[,3] ~Subjects+Activity,data=total_activity,FUN="mean")

for(i in 4:ncol(total_activity)){
  tidy[,i] <- aggregate(total_activity[,i] ~ Subjects+Activity,data=total_activity, FUN="mean")[,3]
}
  
## Rename columns based on variable name ## 
colnames(tidy)[3:ncol(tidy)] <-colnames(MeanSD)



