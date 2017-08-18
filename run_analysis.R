#This code run the analysis described in the final assignment of "Getting and Cleaning Data".
library(dplyr)
library(data.table)

#dowload the zip file *I did it by hand but one could do it also with the script below*
setwd()
if(!file.exists("./UCI HAR Dataset")){
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./Dataset.zip")

# Unzip dataSet to /data directory
unzip(zipfile="./Dataset.zip",exdir="./UCI HAR Dataset")
}

#We start by importing the data for the test dataset
#getting the working directory
directory=getwd()
#getting the "working"test" directory
directory_test=paste0(directory,"/test/")
#extracting the list of files
primaryfilestest=list.files(directory_test,pattern=".txt$")

#reading the three test files
subject_file_test<-read.table(paste0(directory_test,primaryfilestest[[1]]))
x_file_test<-read.table(paste0(directory_test,primaryfilestest[[2]]))
y_file_test<-read.table(paste0(directory_test,primaryfilestest[[3]]))

#reading the activity and features files
activity_Labels<-read.table(paste0(directory,"/activity_labels.txt"))
features<-read.table(paste0(directory,"/features.txt"))
  
#setting the column names
colnames(x_file_test)<-features[,2]
colnames(y_file_test)<-"activityid"
colnames(subject_file_test)<-"subjectid"

#binding together the data
data_test<-cbind(y_file_test,subject_file_test,x_file_test)

#same story for the training 
directory=getwd()
directory_train=paste0(directory,"/train/")
primaryfilestrain=list.files(directory_train,pattern=".txt$")

subject_file_train<-read.table(paste0(directory_train,primaryfilestrain[[1]]))
x_file_train<-read.table(paste0(directory_train,primaryfilestrain[[2]]))
y_file_train<-read.table(paste0(directory_train,primaryfilestrain[[3]]))

colnames(x_file_train)<-features[,2]
colnames(y_file_train)<-"activityid"
colnames(subject_file_train)<-"subjectid"
colnames(activity_Labels) <- c('activityid','activitytype')

data_train<-cbind(y_file_train,subject_file_train,x_file_train)

#merge the two datasets by attaching the rows
data_all<-rbind(data_test,data_train)

#setting the column names
colNames<-colnames(data_all)

#preparing a vector of logicals to extract the columns we're interested in i.e.
#activities, subject, mean and std
formeanandstd<-(grepl("activityid",colNames) | grepl("subjectid",colNames) | grepl("mean..",colNames) | grepl("std..",colNames))
#we extract here the columns we want
finaldata<-data_all[,formeanandstd==TRUE]
#we merge finally the activities labels with the previous data
finaldata_activity<-merge(finaldata,activity_Labels,by="activityid")
#we extract the mean of the different measurements for each activity and subject
newdataset <- aggregate(. ~subjectid + activityid, finaldata_activity, mean)
#we reorder the table according to the subject and activity
newtidydataset <- newdataset[order(newdataset$subjectid, newdataset$activityid),]

names(newtidydataset)
#we print the table into a new text file
write.table(newtidydataset,paste0(directory,"/new_tidy_set.txt"),row.name=FALSE)
