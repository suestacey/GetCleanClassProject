# Class Project
#

This is the assigned project for the Getting and Cleaning Data course, a class in the Data Science Specialization, the third course in the Johns Hopkins Data Science Specialization.

The following components are included with this repo:

1. run_analysis.R 
The script that performs the collection and clean up of the data sets.

2.  tidyData.txt
The independent data set that averages each measured variable by activity and subject.

3.  CodeBook.pdf
Description of variables, data and process for creating the independent data set

to examine the data in the tidyData.txt, run this command at the R prompt:
>tidyData<-read.table("tidyData.txt",header=TRUE)