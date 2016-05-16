getCrimeCount<-function(csv_file, path="."){
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

	#Loading the data points for Crime Incidents
	crimes_data<-count(csv_data,vars = c("Community.Area"))
	crimes_data<-crimes_data[-c(1),]
	crimes_data<-crimes_data[order(crimes_data$Community.Area), ]
	colnames(crimes_data)<-c("Community.Area","Crime.Count")
	return(crimes_data)
}