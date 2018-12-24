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

# read test data sets

x <- convertNumericList(read_lines(".\\test\\X_test.txt"))

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

# merge data sets

dataset <- rbind(datasetTrain, datasetTest)

summaryCol <- colnames(dataset)[4:length(colnames(dataset))]
summarydataset <- summarizeData(dataset,summaryCol[1])

for(i in 2:length(summaryCol)){
    summarydataset <- merge(summarydataset, summarizeData(dataset, summaryCol[i]), by=c("subject", "activity"))
}

rm(list=setdiff(ls(), c("dataset", "summarydataset")))
