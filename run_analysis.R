library(plyr)
library(dplyr)
library(tidyr)
library(data.table)

# Binding data, activity and Subject on "test" data set
DT.test<-read.table("./data/test/X_test.txt",colClasses = "numeric",nrows=2947)
DT.ytest<-read.table("./data/test/y_test.txt",colClasses = "numeric",nrows=2947)
names(DT.ytest)<-"ActivityID"
DT.stest<-read.table("./data/test/subject_test.txt",colClasses = "numeric",nrows=2947)
names(DT.stest)<-"SubjectID"

test<-bind_cols(DT.test,DT.ytest,DT.stest)

# Binding data, activity and Subject on "train" data set
DT.train<-read.table("./data/train/X_train.txt",colClasses = "numeric",nrows=7352)
DT.ytrain<-read.table("./data/train/y_train.txt",colClasses = "numeric",nrows=7352)
names(DT.ytrain)<-"ActivityID"
DT.strain<-read.table("./data/train/subject_train.txt",colClasses = "numeric",nrows=7352)
names(DT.strain)<-"SubjectID"

train<-bind_cols(DT.train,DT.ytrain,DT.strain)


# Using descriptive activity names to name the activities in the data set

DT.actlab<-read.table("./data/activity_labels.txt")
names(DT.actlab)<-c("ActivityID","Activity")

DT.all<-rbind(test,train)
DT.join<-join(DT.all, DT.actlab)
all.tbl<-tbl_df(DT.join)

# Extracting only the measurements on the mean and standard deviation for each measurement and labeling the data set with descriptive variable names
DT.var<-read.table("./data/features.txt")
extr <- grep("-([Mm][Ee][Aa][Nn]|[Ss][Tt][Dd])\\(",DT.var$V2,value=FALSE)

restdata<-select(all.tbl,extr,Activity,SubjectID)
setnames(restdata,seq(along=extr),as.character((DT.var$V2[extr])))

# independent tidy data set with the average of each variable for each activity and each subject
tidier <- restdata %>%
  gather(feature_var_dir, measure, 1:66) %>%
  separate(feature_var_dir, c("feature", "var", "dir")) %>%
  group_by(SubjectID,Activity,feature,var,dir) %>%
  summarize(avg_meas = mean(measure)) %>%
  #   mutate(dir = as.numeric(factor(dir)))%>% 
  spread(var,avg_meas) %>%
  arrange(SubjectID, Activity, feature, dir) %>%
  print
