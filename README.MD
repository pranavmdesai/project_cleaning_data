# How to run the script

## What does the script do
Given a folder which has the activity data using smartphones, this script allows you to create a tidy data set where the output file consists of the mean readings of various factors from the smartphone for a subject and the type of activity they performed


##Script location
The script run_analysis.R should be placed in the same top level directory
as the data

##Pre-Requisites
You will need the following packages - dplyr, data.table and reshape2
If you don't have these copy+paste the below commands in R to install them

    install.packages("data.table")
    install.packages("dplyr")
    install.packages("reshape2")

##Once installed, run the script
The script collects data from the features.txt which is the name of the reading taken using the cell phone

It will further read the names of the activities performed by the individuals from the activity_labels.txt

After this it will load in data from the test and train folders and while merging them provide relevant names.

Finally there is a melt process that we perform on the total, so that we can easily aggregate the data

The code is documented, so it should indicate what each section is performing
