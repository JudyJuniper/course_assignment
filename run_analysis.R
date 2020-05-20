#Course Project
#This run_analysis takes Samsung data (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) and does the following steps:
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#PLEASE SEE THE READ.ME BEFORE CONTINUING. 
#WORKING DIRECTORY MUST BE THE "UCI HAR Dataset" FOLDER (UNZIPPED BY THE ABOVE LINK)
#YOU WILL ALSO NEED THE newHeaders.csv THAT IS INCLUDED IN THE GITHUB (SORRY THAT IS HOW I WENT ABOUT RE-NAMING THE HEADERS...)

library(dplyr)

####################################### Load test data, rename subjectTest to Test ID and yTest to Activity Test ID for clarifying column headings
subjectTest <- read.table("./test/subject_test.txt", header=FALSE)
names(subjectTest)[1] <- "Test ID"
xTest <- read.table("./test/X_test.txt", header=FALSE)
yTest <- read.table("./test/y_test.txt", header=FALSE)
names(yTest)[1] <- "Activity Test ID"

####################################### Load train data, rename subjectTrain and yTrain
subjectTrain <- read.table("./train/subject_train.txt", header=FALSE)
names(subjectTrain)[1] <- "Train ID"
xTrain <- read.table("./train/X_train.txt", header=FALSE)
yTrain <- read.table("./train/y_train.txt", header=FALSE)
names(yTrain)[1] <- "Activity Train ID"

######################################### bind test data.... make new column designating these are Test values
testData <- cbind(subjectTest, yTest, xTest)
dim(testData)
head(testData, n=10)
names(testData)
testData <- mutate(testData, TestOrTrain = "Test")                    #to delete?

######################################### bind train data.... make new column designating these are Train values
trainData <-cbind(subjectTrain, yTrain, xTrain)
dim(trainData)
head(trainData, n=10)
names(trainData)
trainData <- mutate(trainData, TestOrTrain = "Train")                 #to delete?

######################################### merge test & train data by Train ID and Test ID (volunteer participant ID)
mergedData = merge(testData, trainData, by.x="Test ID", by.y="Train ID", all=TRUE)   #.x values correspond to testing data, .y values correspond to training data
any(is.na(mergedData$`Test ID`))
dim(mergedData)
head(mergedData)

TestOrTrainList <- coalesce(mergedData$TestOrTrain.x, mergedData$TestOrTrain.y)
mergedData <- mutate(mergedData, TestOrTrainList)

######################################### colaesce the training and testing activity codes so they are one list
actList <- coalesce(mergedData$`Activity Test ID`, mergedData$`Activity Train ID`)
mergedData <- mutate(mergedData, actList)

######################################## extract columns for mean, std vaues... see documentation for exact column names that were extracted from both test and train datasets (x & y)
filteredData <- mergedData %>% select("Test ID", "actList", "TestOrTrainList", V1.x:V6.x, V41.x:V46.x, V81.x:V86.x, V121.x:V126.x, V161.x:V166.x, V201.x, V202.x, V214.x, V215.x, V227.x, V228.x, V240.x, V241.x, V253.x, V254.x, V266.x:V271.x, V345.x:V350.x, V424.x:V429.x, V452.x:V454.x, V503.x, V504.x, V516.x, V517.x, V529.x, V530.x, V542.x, V543.x,
                                      V1.y:V6.y, V41.y:V46.y, V81.y:V86.y, V121.y:V126.y, V161.y:V166.y, V201.y, V202.y, V214.y, V215.y, V227.y, V228.y, V240.y, V241.y, V253.y, V254.y, V266.y:V271.y, V345.y:V350.y, V424.y:V429.y, V452.y:V454.y, V503.y, V504.y, V516.y, V517.y, V529.y, V530.y, V542.y, V543.y)
dim(filteredData)
names(filteredData)
########################################renames number values in Activity Test ID with their corresponding DESCRIPTIVE WORDS (1 = Walking, 2 = Walking Upstairs, etc)... see documentation for more details

filteredData$`actList`[filteredData$`actList` == 1] <- "Walking"
filteredData$`actList`[filteredData$`actList` == 2] <- "Walking Upstairs"
filteredData$`actList`[filteredData$`actList` == 3] <- "Walking Downstairs"
filteredData$`actList`[filteredData$`actList` == 4] <- "Sitting"
filteredData$`actList`[filteredData$`actList` == 5] <- "Standing"
filteredData$`actList`[filteredData$`actList` == 6] <- "Laying"

###################################################### rename headers using newHeaders.csv
newHeaders = (read.csv("./newHeaders.csv", sep = "", header = FALSE))
newHeaders <- as.character(newHeaders[,2])
head(newHeaders)

library(data.table)
filteredData <- setNames(filteredData, (newHeaders))

####################################################### AVG up all the values 
#Filters the filteredData by each activity name (Walking, Walking Upstairs, etc). 
#For each row of the filtered activity set (ie all Walking records), access each subjectId record using the while loop (looping 1 thru 30)
#Within each filtered Walking set, further filter by subjectID dependent on the j <31 while loop
#For each subjectID, take all the columns of interest (values in columns 4:141) and find the mean. Replace with 0 if not applicable
#Rebuild the first three columns that we lost while doing the mean (the SUbject_ID, the Activity Name), cbind these columns with the averaged data
#Build a new dataframe, walkingAvgFinalFinal, that continually adds the previous record to the finalfinal dataframe
#Do this for each activity type 

j = 1
walkingAvgFinalFinal = NULL
walkRows <- filter(filteredData, filteredData$Activity_Name == "Walking")
for (i in 1:nrow(walkRows)) {
  while (j < 31) {
    walkingVal <- filter(walkRows, Subject_ID == j)
    walkingAvgTemp <- walkingVal[4:141] %>% replace(is.na(.),0) %>% summarise_all(mean)
    Subject_ID <- j
    Activity_Name <- "Walking"
    walkingAvgFinal <- cbind(Subject_ID, Activity_Name, walkingAvgTemp)
    walkingAvgFinalFinal = rbind(walkingAvgFinalFinal, walkingAvgFinal)
    j=j+1
  }
}

j = 1
walkingUpAvgFinalFinal = NULL
walkUpRows <- filter(filteredData, filteredData$Activity_Name == "Walking Upstairs")
for (i in 1:nrow(walkUpRows)) {
  while (j < 31) {
    walkingUpVal <- filter(walkUpRows, Subject_ID == j)
    walkingUpAvgTemp <- walkingUpVal[4:141] %>% replace(is.na(.),0) %>% summarise_all(mean)
    Subject_ID <- j
    Activity_Name <- "Walking Upstairs"
    walkingUpAvgFinal <- cbind(Subject_ID, Activity_Name, walkingUpAvgTemp)
    walkingUpAvgFinalFinal = rbind(walkingUpAvgFinalFinal, walkingUpAvgFinal)
    j=j+1
  }
}     

j = 1
walkingDownAvgFinalFinal = NULL
walkDownRows <- filter(filteredData, filteredData$Activity_Name == "Walking Downstairs")
for (i in 1:nrow(walkDownRows)) {
  while (j < 31) {
    walkingDownVal <- filter(walkDownRows, Subject_ID == j)
    walkingDownAvgTemp <- walkingDownVal[4:141] %>% replace(is.na(.),0) %>% summarise_all(mean)
    Subject_ID <- j
    Activity_Name <- "Walking Downstairs"
    walkingDownAvgFinal <- cbind(Subject_ID, Activity_Name, walkingDownAvgTemp)
    walkingDownAvgFinalFinal = rbind(walkingDownAvgFinalFinal, walkingDownAvgFinal)
    j=j+1
  }
}      

j = 1
sitAvgFinalFinal = NULL
sitRows <- filter(filteredData, filteredData$Activity_Name == "Sitting")
for (i in 1:nrow(sitRows)) {
  while (j < 31) {
    sitVal <- filter(sitRows, Subject_ID == j)
    sitAvgTemp <- sitVal[4:141] %>% replace(is.na(.),0) %>% summarise_all(mean)
    Subject_ID <- j
    Activity_Name <- "Sitting"
    sitAvgFinal <- cbind(Subject_ID, Activity_Name, sitAvgTemp)
    sitAvgFinalFinal = rbind(sitAvgFinalFinal, sitAvgFinal)
    j=j+1
  }
}

j = 1
standAvgFinalFinal = NULL
standRows <- filter(filteredData, filteredData$Activity_Name == "Standing")
for (i in 1:nrow(standRows)) {
  while (j < 31) {
    standVal <- filter(standRows, Subject_ID == j)
    standAvgTemp <- standVal[4:141] %>% replace(is.na(.),0) %>% summarise_all(mean)
    Subject_ID <- j
    Activity_Name <- "Standing"
    standAvgFinal <- cbind(Subject_ID, Activity_Name, standAvgTemp)
    standAvgFinalFinal = rbind(standAvgFinalFinal, standAvgFinal)
    j=j+1
  }
}

j = 1
layAvgFinalFinal = NULL
layRows <- filter(filteredData, filteredData$Activity_Name == "Laying")
for (i in 1:nrow(layRows)) {
  while (j < 31) {
    layVal <- filter(layRows, Subject_ID == j)
    layAvgTemp <- layVal[4:141] %>% replace(is.na(.),0) %>% summarise_all(mean)
    Subject_ID <- j
    Activity_Name <- "Laying"
    layAvgFinal <- cbind(Subject_ID, Activity_Name, layAvgTemp)
    layAvgFinalFinal = rbind(layAvgFinalFinal, layAvgFinal)
    j=j+1
  }
}

############################# combine all the finalfinal dataframes that have so far been broken down by activity type
final = NULL
final =rbind(final, walkingAvgFinalFinal, walkingUpAvgFinalFinal, walkingDownAvgFinalFinal, sitAvgFinalFinal, standAvgFinalFinal,  layAvgFinalFinal)

############################# add one more column to designate if a subject was part of the Testing or Training group, for easier filtering and data use
final <- mutate(final, test_or_train = "NA")
final <- final %>%
  select(test_or_train, everything())

for (i in 1:nrow(final)){
  if ( final$Subject_ID[i] == 1|| final$Subject_ID[i]==3||final$Subject_ID[i]==5||final$Subject_ID[i]==6||final$Subject_ID[i]==7||final$Subject_ID[i]==8||
       final$Subject_ID[i]==11||final$Subject_ID[i]==14||final$Subject_ID[i]==15||final$Subject_ID[i]==16||final$Subject_ID[i]==17||final$Subject_ID[i]==19||
       final$Subject_ID[i]==21||final$Subject_ID[i]==22||final$Subject_ID[i]==23||final$Subject_ID[i]==25||final$Subject_ID[i]==26||final$Subject_ID[i]==27||
       final$Subject_ID[i]==28||final$Subject_ID[i]==29||final$Subject_ID[i]==30) {
    final$test_or_train[i] = "Training Group"
    i = i+1
  } else {
    final$test_or_train[i] = "Testing Group"
    i = i+1
  }
}

##################### write to file as final.txt  
write.table(final, file ="./final.txt", row.names=FALSE)
