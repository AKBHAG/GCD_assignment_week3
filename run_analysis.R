run_analysis <- function() {
  
  if(!file.exists("UCI HAR Dataset")){         
    dir.create("UCI HAR Dataset")
  }
  
  fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  
  download.file(fileUrl, destfile = "C:/Users/AKBHAG/Desktop/Data fluency/UCI HAR Dataset.zip", method ="auto")
  
  dateDownloaded<-date()
  
  unzip("C:/Users/AKBHAG/Desktop/Data fluency/UCI HAR Dataset.zip", list = TRUE)
  
  setwd("C:/Users/AKBHAG/Desktop/Data fluency/UCI HAR Dataset")
  
  X_test<-read.table("C:/Users/AKBHAG/Desktop/Data fluency/UCI HAR Dataset/test/X_test.txt")
  
  y_test<-read.table("C:/Users/AKBHAG/Desktop/Data fluency/UCI HAR Dataset/test/y_test.txt")
  
  subject_test<-read.table("C:/Users/AKBHAG/Desktop/Data fluency/UCI HAR Dataset/test/subject_test.txt")  
  
  X_train<-read.table("C:/Users/AKBHAG/Desktop/Data fluency/UCI HAR Dataset/train/X_train.txt")
  
  y_train<-read.table("C:/Users/AKBHAG/Desktop/Data fluency/UCI HAR Dataset/train/y_train.txt")
  
  subject_train<-read.table("C:/Users/AKBHAG/Desktop/Data fluency/UCI HAR Dataset/train/subject_train.txt")
  
  features<-read.table("C:/Users/AKBHAG/Desktop/Data fluency/UCI HAR Dataset/features.txt")
  
  activity<-read.table("C:/Users/AKBHAG/Desktop/Data fluency/UCI HAR Dataset/activity_labels.txt")
  dim(X_test)
  dim(y_test)
  dim(subject_test)
  dim(X_train)
  dim(y_train)
  dim(subject_train)
  dim(features)
  dim(activity)
  
  colnames(X_train) <- t(features[2])
  
  colnames(X_test) <- t(features[2])
  
  X_train$activities <- y_train[, 1]
  
  View(X_train)
  
  X_train$participants <- subject_train[, 1]
  
  X_test$activities <- y_test[, 1]
  
  X_test$participants <- subject_test[, 1]
  
  
  Master <- rbind(X_train, X_test)
  
  duplicated(colnames(Master))
  
  Master <- Master[, !duplicated(colnames(Master))]
  
  Mean <- grep("mean()", names(Master), value = FALSE, fixed = TRUE)
  
  Mean <- append(Mean, 471:477)
  
  InstrumentMeanMatrix <- Master[Mean]
  
  STD <- grep("std()", names(Master), value = FALSE)
  
  InstrumentSTDMatrix <- Master[STD]
  
  
  Master$activities <- as.character(Master$activities)
  Master$activities[Master$activities == 1] <- "Walking"
  Master$activities[Master$activities == 2] <- "Walking Upstairs"
  Master$activities[Master$activities == 3] <- "Walking Downstairs"
  Master$activities[Master$activities == 4] <- "Sitting"
  Master$activities[Master$activities == 5] <- "Standing"
  Master$activities[Master$activities == 6] <- "Laying"
  Master$activities <- as.factor(Master$activities)
  
  names(Master)
  
  names(Master) <- gsub("Acc", "Accelerator", names(Master))
  names(Master) <- gsub("Mag", "Magnitude", names(Master))
  names(Master) <- gsub("Gyro", "Gyroscope", names(Master))
  names(Master) <- gsub("^t", "time", names(Master))
  names(Master) <- gsub("^f", "frequency", names(Master))
  
  
  Master$participants <- as.character(Master$participants)
  Master$participants[Master$participants == 1] <- "Participant 1"
  Master$participants[Master$participants == 2] <- "Participant 2"
  Master$participants[Master$participants == 3] <- "Participant 3"
  Master$participants[Master$participants == 4] <- "Participant 4"
  Master$participants[Master$participants == 5] <- "Participant 5"
  Master$participants[Master$participants == 6] <- "Participant 6"
  Master$participants[Master$participants == 7] <- "Participant 7"
  Master$participants[Master$participants == 8] <- "Participant 8"
  Master$participants[Master$participants == 9] <- "Participant 9"
  Master$participants[Master$participants == 10] <- "Participant 10"
  Master$participants[Master$participants == 11] <- "Participant 11"
  Master$participants[Master$participants == 12] <- "Participant 12"
  Master$participants[Master$participants == 13] <- "Participant 13"
  Master$participants[Master$participants == 14] <- "Participant 14"
  Master$participants[Master$participants == 15] <- "Participant 15"
  Master$participants[Master$participants == 16] <- "Participant 16"
  Master$participants[Master$participants == 17] <- "Participant 17"
  Master$participants[Master$participants == 18] <- "Participant 18"
  Master$participants[Master$participants == 19] <- "Participant 19"
  Master$participants[Master$participants == 20] <- "Participant 20"
  Master$participants[Master$participants == 21] <- "Participant 21"
  Master$participants[Master$participants == 22] <- "Participant 22"
  Master$participants[Master$participants == 23] <- "Participant 23"
  Master$participants[Master$participants == 24] <- "Participant 24"
  Master$participants[Master$participants == 25] <- "Participant 25"
  Master$participants[Master$participants == 26] <- "Participant 26"
  Master$participants[Master$participants == 27] <- "Participant 27"
  Master$participants[Master$participants == 28] <- "Participant 28"
  Master$participants[Master$participants == 29] <- "Participant 29"
  Master$participants[Master$participants == 30] <- "Participant 30"
  Master$participants <- as.factor(Master$participants)
  
  Master.dt <- data.table(Master)
  
  library(data.table)
  
  TidyData <- Master.dt[, lapply(.SD, mean), by = 'participants,activities']
  
  write.table(TidyData, file = "Tidy.txt", row.names = FALSE)
  
}
