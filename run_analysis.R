library(dplyr)


#Nombre de las variables
features<-read.table("./Dataset/features.txt")
#Nombre de las actividades
activity_labels<-read.table("./Dataset/activity_labels.txt")

X_train<-read.table("./Dataset/train/X_train.txt")
y_train<-read.table("./Dataset/train/y_train.txt")
subject_train<-read.table("./Dataset/train/subject_train.txt")

y_test<-read.table("./Dataset/test/y_test.txt")
X_test<-read.table("./Dataset/test/X_test.txt")
subject_test<-read.table("./Dataset/test/subject_test.txt")


#### 1.Merges the training and the test sets to create one data set.
X_train<-X_train%>%
        mutate(activity=y_train$V1,.before = V1)

X_test<-X_test%>%
        mutate(activity=y_test$V1,.before = V1)

#### 4. Appropriately labels the data set with descriptive variable names.
data<-bind_rows(X_train,X_test)
names(data)<-c("activity",features$V2)

#### 2.Extracts only the measurements on the mean and standard deviation for each measurement.
data<-data%>%
        select(grep(".mean\\(\\)(.)?|.std\\(\\)(.)?|activity",names(data)))

#### 3.Uses descriptive activity names to name the activities in the data set
data$activity<-factor(data$activity,
                  levels = c(1,2,3,4,5,6),
                  labels = c("WALKING",
                             "WALKING_UPSTAIRS",
                             "WALKING_DOWNSTAIRS",
                             "SITTING",
                             "STANDING",
                             "LAYING"))

#### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Subjects<-bind_rows(subject_train,subject_test)

tidydata<-data%>%
        mutate(subject=Subjects$V1,.after = activity)%>%
        group_by(activity,subject)%>%
        summarize_each(funs(mean))
        

#### The tidy data set created in step 5 of the instructions.
write.table(tidydata,"./tidydata.txt",row.names = FALSE)




