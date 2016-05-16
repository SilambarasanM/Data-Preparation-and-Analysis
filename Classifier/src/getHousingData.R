getHousingData<-function(csv_file, path="."){
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
	housing_data<-aggregate(csv_data$Units, by=list(csv_data$Community.Area.Number), FUN=sum)
	colnames(housing_data)<-c("Community.Area","Housing.Units")
	housing_data<-housing_data[ order(housing_data$Community.Area), ]
	return(housing_data)
}