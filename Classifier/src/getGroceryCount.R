getGroceryCount<-function(csv_file, path=".", draw_map = TRUE){
	#Make sure system is connected to internet while running this code to install libraries
	#Install and load the required R libraries
	if (!require("plyr")) {
		install.packages("plyr", dependencies = TRUE)
		library(plyr)
	}

	#Loading the data points for Life Expectancy Data
	if(!path == ".")
		setwd(path)
	
	#Loading the data points for Grocery Store Locations
	csv_data<-read.csv(csv_file, header=TRUE, stringsAsFactor=FALSE)
	grocery_data<-count(csv_data,vars = c("COMMUNITY.AREA"))
	colnames(grocery_data)<-c("Community.Area","Grocery.Count")
	grocery_data<-grocery_data[ order(grocery_data$Community.Area), ]
	return(grocery_data)
}