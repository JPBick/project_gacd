#Setting WD and checking
#setwd("D:/Programing_data_R/gacd/project")
getwd()

#install.packages("dplyr") if not installed
library(dplyr)

#Loading the Data--- Note: To use this script original ZIP_File have to be unzipped in the WD


train_raw=read.table("UCI HAR Dataset/train/X_train.txt")
test_raw= read.table("UCI HAR Dataset/test/X_test.txt")
dim(train_raw)
dim(test_raw)
activity_train<-read.table("UCI HAR Dataset/train/y_train.txt")
activity_test<-read.table("UCI HAR Dataset/test/y_test.txt")
dim(activity_train)
dim(activity_test)
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt")
dim(subject_train)
dim(subject_test)
activity_labels<-read.table("UCI HAR Dataset/activity_labels.txt")
activity_labels$V2


#Merging and labeling the sets, Feature.txt is used to label the columns. The new collumns for Subjects and the Activies are labeld as well.
measurements_all<- rbind(train_raw,test_raw)
dim(measurements_all)
varnames<- read.table("UCI HAR Dataset/features.txt")
names(measurements_all)<-varnames$V2
#View(measurements_all)

activity_all<-rbind(activity_train,activity_test)
dim(activity_all)
names(activity_all)<-"activity"

subject_all<-rbind(subject_train,subject_test)
dim(subject_all)
names(subject_all)<-"subject"


dataset_all<-cbind(subject_all,activity_all,measurements_all)
dim(dataset_all)
#View(dataset_all)

#So far the train and the test data are merged and 2 columens for Subject and Activity are added. 
#In the next step the activity code with numbers from 1 to 6 are now coded with their names taken from activity_labels.txt



dataset_all$activity<-as.factor(dataset_all$activity)
levels(dataset_all$activity)<-activity_labels$V2
levels(dataset_all$activity)



#Subsetting the Dataset regarding the variables mean() an std(). This means for each variable the mean() and the std() column will be extracted.
#For this purpose the agrep function is used to match column names with mean() and std() to subset the dataset

sub_std=agrep ("std ()" , colnames(dataset_all),max.distance = 0.2)
sub_mean= agrep ("mean ()" , colnames(dataset_all),max.distance = 0.2)

subset_ids=sort(c(1,2,sub_std,sub_mean))  #Subset IDs (Column IDs) contains now Subject , Activity and all clomumns which matched

data_subset=dataset_all[subset_ids]
#View(data_subset)
#levels(data_subset$activity)




#For the next step the dplyr package is required. See top.
#The data will now be grouped by the levels in subject and activity column and the mean of each activity and for each subject is calculated
datadf<-tbl_df(data_subset)
by_act_sub<-group_by(datadf,subject,activity)
final_data<-by_act_sub%>%summarise_each(funs(mean))
#final_data
#View(final_data)


#Now a renaming of the columns is required in order to have desrcibing column names of the new variables. Since the new variables are the mean of 
# each specific activity the new colnames are added with  "mean of [old variable name] activities" via the paste function

oldnames=colnames(final_data)
newnames=oldnames
for(ii in 3 : length(oldnames)){newnames[ii]=paste ("mean of", oldnames[ii], "activities")}
names(final_data)<-newnames
#View(final_data)


#Now an output file will be produced

write.table(final_data, file="course_project_output.txt",col.names = TRUE, row.name=FALSE) 

#For reading in the output file test=read.table("course_project_output.txt", header = T) is suggestet
#View(test)
