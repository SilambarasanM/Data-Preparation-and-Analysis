getExpectancy <- function(csv_file, path="."){
	#Make sure system is connected to internet while running this code to install libraries
	#Install and load the required R libraries
	if (!require("plyr")) {
	install.packages("plyr", dependencies = TRUE)
	library(plyr)
	}

	#Loading the data points for Life Expectancy Data
	if(!path == ".")
		setwd(path)

	csv_data<-read.csv(csv_file, header=TRUE, stringsAsFactor=FALSE)

	#Ignore NA data and convert text to date format
	expectancy_data<-subset(csv_data, !is.na(csv_data$Community.Area.Number))
	expectancy_data<-expectancy_data[,c("Community.Area.Number", "X2010.Life.Expectancy")]
	expectancy_data<-expectancy_data[order(expectancy_data$Community.Area.Number),]
	colnames(expectancy_data)<- c("Community.Area","Life.Expectancy")
	return(expectancy_data)
}