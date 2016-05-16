getHardshipIndex<-function(csv_file, path="."){
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
	hardship_data<-subset(csv_data, !is.na(csv_data$HARDSHIP.INDEX))
	hardship_data<-hardship_data[,c("Community.Area.Number", "HARDSHIP.INDEX")]
	colnames(hardship_data)<-c("Community.Area", "Hardship.Index")
	hardship_data<-hardship_data[order(hardship_data$Community.Area),]
	return(hardship_data)
}
