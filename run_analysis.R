run_analysis<-function(){
	
		# NOTES about this script:
		# Temporary data frames used to aid in troubleshooting
		# The temp data frames are:  toBeColumnNames, MeanStdTest and MeanStdTrain
		# As I am not yet an expert statistician, I choose to include all measurements that 
		# contain "mean" or "std" in the name, thus the people who use this data downstream 
		# can make the determination if a particular measurement is useful or needed
	
	# Preliminaries:  Load the dplyr package, set the data directory
	library(dplyr)
	dataDir<-"./UCI HAR Dataset"
	
	# 1 Read all the files needed for processing:  8 total files
		# The Test and Train datasets consist of 3 files each:  
		#  X_(test/train) - the measured data for each sensor signal
		#  Y_(test/train) - the activity data as a numeric value
		#  subject_(test/train) - the ID of the subject who carried out the experiment
	
	rawTest<-read.table("./UCI HAR Dataset/test/X_test.txt", header=FALSE)
		# Due to a possible system limitation while testing, a delay is included to avoid loss of data
	Sys.sleep(3)
	YTest<-read.table("./UCI HAR Dataset/test/Y_test.txt", header=FALSE)
	subjectTest<-read.table("./UCI HAR Dataset/test/subject_test.txt", header=FALSE)
	dim(rawTest);dim(YTest);dim(subjectTest);head(rawTest[,1:7])
			
	rawTrain<-read.table("./UCI HAR Dataset/train/X_train.txt", header=FALSE)
		# Due to a possible system limitation while testing, a delay is included to avoid loss of data
	Sys.sleep(5)
	YTrain<-read.table("./UCI HAR Dataset/train/y_train.txt", header=FALSE)
	subjectTrain<-read.table("./UCI HAR Dataset/train/subject_train.txt", header=FALSE)
	dim(rawTrain);dim(YTrain);dim(subjectTrain);head(rawTrain[,1:7])

		#  The activity_labels.txt file is common to both datasets and defines the 6 labels for the activities
		# LAYING, SITTING, STANDING, WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS
	activityNames<-read.table("./UCI HAR Dataset/activity_labels.txt", header=FALSE)

	
		# The features.txt file is common to both datasets and defines the 561 labels of the captured measurements.
		# Since only those measurements that contain "mean" or "std" in the name are summarized in the final data,
		# this reduces the number of measurements to 86.
		# see Code Book for complete list of measurements used in this script
	featureVariables<-read.table("./UCI HAR Dataset/features.txt", header=FALSE)
	dim(activityNames);dim(featureVariables)
	
		# 2 Clean up all data to prepare for merging	
		# take the list of features.txt and turn it into label names for use as column names in rawTest

	toBeColumnNames <- data.frame(lapply(featureVariables, as.character), stringsAsFactors=FALSE)
		# then clean it up for correct naming conventions, i.e. remove all parenthesis, commas and dashes
		# NOTE:  the names were not reduced to lower case,
		# as I believe CamelCase variable names improves readability
	toBeColumnNames<-toBeColumnNames[,2]
	toBeColumnNames<-gsub("(", "", toBeColumnNames, fixed=TRUE)
	toBeColumnNames<-gsub(")", "", toBeColumnNames, fixed=TRUE)
	toBeColumnNames<-gsub(",", "", toBeColumnNames, fixed=TRUE)
	toBeColumnNames<-gsub("-", "", toBeColumnNames, fixed=TRUE)	
	colnames(rawTest)<-toBeColumnNames
	colnames(rawTrain)<-toBeColumnNames
	head(rawTest[,c(1:5,558:561)]);head(rawTrain[,c(1:5,558:561)])
	
		# Extract only the needed measurements
		# get the columns that contain the word "mean" and "std" from the Test data and store into temporary data frame
	MeanStdTest<-rawTest[grep("[Mm]ean|[Ss]td",names(rawTest),value=TRUE,ignore.case=TRUE)]
		# add the subject Column
	MeanStdTest<-cbind(subjectTest,MeanStdTest)
	colnames(MeanStdTest)[1] <- 'Subject'
		# add the activity column
	MeanStdTest<-cbind(YTest,MeanStdTest)
	colnames(MeanStdTest)[1] <- 'Activity'
	dim(MeanStdTest)
	
		# get the columns that contain the word "mean" and "std" from the Train data and store into temporary data frame
	MeanStdTrain<-rawTrain[grep("[Mm]ean|[Ss]td",names(rawTrain),value=TRUE,ignore.case=TRUE)]
		# add the subject Column
	MeanStdTrain<-cbind(subjectTrain,MeanStdTrain)
	colnames(MeanStdTrain)[1] <- 'Subject'
		# add the activity column
	MeanStdTrain<-cbind(YTrain,MeanStdTrain)
	colnames(MeanStdTrain)[1] <- 'Activity'
	dim(MeanStdTrain)

		# 3 merge the two datasets
		# combine the Test and Train measurement data into the tidyData data frame
	tidyData <-rbind(MeanStdTest,MeanStdTrain)
	head(tidyData[,1:6])
	
		# replace all the numerical Activity values with Activity Label
	for (i in 1:6){
		word <-as.character(activityNames[i,2])
		tidyData$Activity[tidyData$Activity == i] <- word 
	}
	
		# 4 group and summarize the data
		# Group the data by Activity and Subject and summarize each measurement using the mean
	tidyData <- tidyData %>% group_by(Activity,Subject) %>% summarise_each(funs(mean))
	
	# 5 write the tidy data to a file
	write.table(tidyData, file="tidyData.txt", row.name=FALSE)
	dim(tidyData)
}
