library(dplyr)
library(reshape2)
library(data.table)

#Let's import the features so that we can then use them to connect to our activities as measurements
featureNames <- read.table("features.txt")

#Lets read the activity labels
activity_label <- read.table("activity_labels.txt")

#these are the training subjects that we will read into a data frame 
subjects_training <- read.table("train/subject_train.txt")

#these are the actual readings for the training subjects that we will read into a data frame 
readings_training <- read.table("train/X_train.txt")

#This will read the activities for the training subjects 
activity_training <- read.table("train/y_train.txt")

#Associate the readings with the respective subjects by adding the subject column to the reading DF
readings_training$subject <- select(subjects_training, V1)

#Associate the activity training with the Activity label
activity_training <- select(merge(x=activity_training,y =activity_label),V2)

#Now to create the complete dataset
readings_training$activity <- select(activity_training,V2)

#Now we repeat the same thing for test data

#these are the test subjects that we will read into a data frame 
subjects_test <- read.table("test/subject_test.txt")

#these are the actual readings that we will read into a data frame 
readings_test <- read.table("test/X_test.txt")

#This will read the activities for the reading
activity_test <- read.table("test/y_test.txt")

#Associate the readings with the respective subjects by adding the subject column to the reading DF
readings_test$subject <- select(subjects_test, V1)

#Associate the activity training with the Activity label
activity_test <- select(merge(x=activity_test,y =activity_label),V2)

#Now to create the complete dataset
readings_test$activity <- select(activity_test,V2)

#Let's convert the data frames to numeric and factor respectively so that we can use rbind
readings_test[,562] <- sapply(readings_test[,562], as.numeric)
readings_training[,562] <- sapply(readings_training[,562], as.numeric)
readings_test[,563] <- sapply(readings_test[,563], as.factor)
readings_training[,563] <- sapply(readings_training[,563], as.factor)

#Now we can combine all the readings
total_readings <- rbind(readings_training, readings_test)

#Now to add subject and activity to feature names
featureNames[,2]= sapply(featureNames[,2],as.character)
featureNames[562,2] <- "subject"
featureNames[563,2] <- "activity"

#Giving names to total_Readings
names(total_readings) <- featureNames$V2


#find terms containing mean and std. deviation. Also find terms containing mean frequency so we can 
#exclude these terms
Mean_Std <- grep("-[Mm]ean|-[Ss]td",names(total_readings))
meanFreq <- grep("[Mm]ean[Ff]req",names(total_readings))
columns_tokeep <- setdiff(Mean_Std, meanFreq)
columns_tokeep <- append(columns_tokeep,c(562,563))

#Ordering the data nicely so subject and activity are the first two columns and we only retain 
#the required columns
total_readings <- total_readings[columns_tokeep]

#Now to place activity first
total_readings <- select(total_readings, 68,67,1:66)

total_readings <- data.table(total_readings)

#Let's clean up the names so that they can satisfy the variable requirements of the course
names_of_dataset <- names(total_readings)
names_of_dataset <- gsub("-","",names_of_dataset)
names_of_dataset <- gsub("\\(","",names_of_dataset)
names_of_dataset <- gsub("\\)","",names_of_dataset)
names_of_dataset <- gsub("Gyro","Gyroscope ",names_of_dataset)
names_of_dataset <- gsub("Acc","Acceleration ",names_of_dataset)
names_of_dataset <- sub("fBodyBody","fBody",names_of_dataset)
names_of_dataset <- sub("fBody","frequency based body ",names_of_dataset)
names_of_dataset <- sub("tBody","time based body ",names_of_dataset)
names_of_dataset <- sub("fGravity","frequency based gravity ",names_of_dataset)
names_of_dataset <- sub("tGravity","time based gravity ",names_of_dataset)
names_of_dataset <- sub("Magmean","Magnitude Mean",names_of_dataset)
names_of_dataset <- sub("Magstd","Magnitude Std Deviation",names_of_dataset)
names_of_dataset <- sub("Jerkmean","jerk mean",names_of_dataset)
names_of_dataset <- sub("JerkMagnitude","jerk magnitude",names_of_dataset)
names_of_dataset <- tolower(names_of_dataset)

#Let's bring the modified names to the data set
names(total_readings) <- names_of_dataset

#Let's melt this so that readings are the variables by subject and by activity
melt_readings <- melt(total_readings, id=c("subject","activity"))

#Let's cast this so that we obtain tidy data which outlines means of each variable by subject
#and by activity
tidy_data <- dcast(melt_readings,subject+activity~variable, mean)

#Let's write this out as a txt file
write.table(tidy_data, file = "tidy.txt",row.names = F)

