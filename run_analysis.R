library(dplyr)
library(tidyr)
library(readr)

# function that converts values from file to list of numeric

convertNumericList <- function(data){
    splitValue <- strsplit(data, " ")
    lapply(splitValue, function(y) as.numeric(y[!y %in% ""]))
}

# function that finds the activity label

findActivityLabel <- function(id){
    sub("^[0-9] ", "",grep(paste(id), activityLabels, value = TRUE))
}


# function that retrieves the data at the specified index within the list

retrieveData <- function(dataset, index){
    sapply(dataset, function(data) data[index])
}

# function that summarizes based on the given column

summarizeData <- function(dataset, name){
    colName <- paste0("mean_", name)
    dataset%>%group_by(subject, activity)%>%summarise(!!colName := mean(!!sym(name)))
}

# read features label

features <- read_lines(".\\features.txt")
featuresFilter <- grep("-mean\\(\\)|-std\\(\\)", features)

# read activity label

activityLabels <- read_lines(".\\activity_labels.txt")

# read training data sets

x <- convertNumericList(read_lines(".\\train\\X_train.txt"))
bodyaccx <- convertNumericList(read_lines(".\\train\\Inertial Signals\\body_acc_x_train.txt"))
bodyaccy <- convertNumericList(read_lines(".\\train\\Inertial Signals\\body_acc_y_train.txt"))
bodyaccz <- convertNumericList(read_lines(".\\train\\Inertial Signals\\body_acc_z_train.txt"))
bodygyrox <- convertNumericList(read_lines(".\\train\\Inertial Signals\\body_gyro_x_train.txt"))
bodygyroy <- convertNumericList(read_lines(".\\train\\Inertial Signals\\body_gyro_y_train.txt"))
bodygyroz <- convertNumericList(read_lines(".\\train\\Inertial Signals\\body_gyro_z_train.txt"))
totalaccx <- convertNumericList(read_lines(".\\train\\Inertial Signals\\total_acc_x_train.txt"))
totalaccy <- convertNumericList(read_lines(".\\train\\Inertial Signals\\total_acc_y_train.txt"))
totalaccz <- convertNumericList(read_lines(".\\train\\Inertial Signals\\total_acc_z_train.txt"))

# create training data set

datasetTrain <- data.frame(subject = as.numeric(read_lines(".\\train\\subject_train.txt")),
                           activity = read_lines(".\\train\\y_train.txt"),
                           type = "Train")

# modify activity column value to activity names

datasetTrain <- datasetTrain %>% mutate(activity = sapply(activity, function(x) findActivityLabel(x)))

# include only measurement columns with mean and standard deviation calculation

for(i in featuresFilter){
    colName <- sub("^[0-9]+ ", "", features[i])
    datasetTrain <- datasetTrain %>% mutate(!!colName := retrieveData(x, i)) 
}

# tidy up signal values to be separated to different columns

for(i in seq_along(1:120)){
    datasetTrain <- datasetTrain %>% mutate(!!paste0("bodyaccelsignalx-", i) := retrieveData(bodyaccx, i),
                                            !!paste0("bodyaccelsignaly-", i) := retrieveData(bodyaccy, i),
                                            !!paste0("bodyaccelsignalz-", i) := retrieveData(bodyaccz, i),
                                            !!paste0("bodygyrosignalx-", i) := retrieveData(bodygyrox, i),
                                            !!paste0("bodygyrosignaly-", i) := retrieveData(bodygyroy, i),
                                            !!paste0("bodygyrosignalz-", i) := retrieveData(bodygyroz, i),
                                            !!paste0("bodygyrosignalx-", i) := retrieveData(bodygyrox, i),
                                            !!paste0("phoneaccelsignalx-", i) := retrieveData(totalaccx, i),
                                            !!paste0("phoneaccelsignaly-", i) := retrieveData(totalaccy, i),
                                            !!paste0("phoneaccelsignalz-", i) := retrieveData(totalaccz, i))
}

# read test data sets

x <- convertNumericList(read_lines(".\\test\\X_test.txt"))
bodyaccx <- convertNumericList(read_lines(".\\test\\Inertial Signals\\body_acc_x_test.txt"))
bodyaccy <- convertNumericList(read_lines(".\\test\\Inertial Signals\\body_acc_y_test.txt"))
bodyaccz <- convertNumericList(read_lines(".\\test\\Inertial Signals\\body_acc_z_test.txt"))
bodygyrox <- convertNumericList(read_lines(".\\test\\Inertial Signals\\body_gyro_x_test.txt"))
bodygyroy <- convertNumericList(read_lines(".\\test\\Inertial Signals\\body_gyro_y_test.txt"))
bodygyroz <- convertNumericList(read_lines(".\\test\\Inertial Signals\\body_gyro_z_test.txt"))
totalaccx <- convertNumericList(read_lines(".\\test\\Inertial Signals\\total_acc_x_test.txt"))
totalaccy <- convertNumericList(read_lines(".\\test\\Inertial Signals\\total_acc_y_test.txt"))
totalaccz <- convertNumericList(read_lines(".\\test\\Inertial Signals\\total_acc_z_test.txt"))

datasetTest <- data.frame(subject = as.numeric(read_lines(".\\test\\subject_test.txt")),
                          activity = read_lines(".\\test\\y_test.txt"),
                          type = "Test")

# modify activity column value to activity names

datasetTest <- datasetTest %>% mutate(activity = sapply(activity, function(x) findActivityLabel(x)))

# include only measurement columns with mean and standard deviation calculation

for(i in featuresFilter){
    colName <- sub("^[0-9]+ ", "", features[i])
    datasetTest <- datasetTest %>% mutate(!!colName := retrieveData(x, i)) 
}

# tidy up signal values to be separated to different columns

for(i in seq_along(1:120)){
    datasetTest <- datasetTest %>% mutate(!!paste0("bodyaccelsignalx-", i) := retrieveData(bodyaccx, i),
                                          !!paste0("bodyaccelsignaly-", i) := retrieveData(bodyaccy, i),
                                          !!paste0("bodyaccelsignalz-", i) := retrieveData(bodyaccz, i),
                                          !!paste0("bodygyrosignalx-", i) := retrieveData(bodygyrox, i),
                                          !!paste0("bodygyrosignaly-", i) := retrieveData(bodygyroy, i),
                                          !!paste0("bodygyrosignalz-", i) := retrieveData(bodygyroz, i),
                                          !!paste0("bodygyrosignalx-", i) := retrieveData(bodygyrox, i),
                                          !!paste0("phoneaccelsignalx-", i) := retrieveData(totalaccx, i),
                                          !!paste0("phoneaccelsignaly-", i) := retrieveData(totalaccy, i),
                                          !!paste0("phoneaccelsignalz-", i) := retrieveData(totalaccz, i))
}

# merge data sets

dataset <- rbind(datasetTrain, datasetTest)

summaryCol <- colnames(dataset)[4:length(colnames(dataset))]
summarydataset <- summarizeData(dataset,summaryCol[1])

for(i in 2:length(summaryCol)){
    summarydataset <- merge(summarydataset, summarizeData(dataset, summaryCol[i]), by=c("subject", "activity"))
}

rm(list=setdiff(ls(), c("dataset", "summarydataset")))
