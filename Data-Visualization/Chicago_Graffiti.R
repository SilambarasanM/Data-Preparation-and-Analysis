#Make sure system is connected to internet while running this code to install libraries
#Install and load the required R libraries
if (!require("plyr")) {
 install.packages("plyr", dependencies = TRUE)
 library(plyr)
 }
if (!require("ggmap")) {
 install.packages("ggmap", dependencies = TRUE)
 library(ggmap)
 }
if (!require("gplots")) {
 install.packages("gplots", dependencies = TRUE)
 library(gplots)
 }
if (!require("rgdal")) {
 install.packages("rgdal", dependencies = TRUE)
 library(rgdal)
 }

#Loading the data points for 2015 Graffiti Removal Incidents
setwd("C:\\Users\\admin\\Documents\\R Scripts\\Graffiti")
csv_data<-read.csv("Graffiti_Removal_2015.csv", header=TRUE, stringsAsFactor=FALSE)

#Ignore NA data and convert text to date format
csv_data<-subset(csv_data, !is.na(csv_data$Community.Area))
csv_data$Creation.Date = as.Date(csv_data$Creation.Date, "%m/%d/%Y")
csv_data$Completion.Date = as.Date(csv_data$Completion.Date, "%m/%d/%Y")
csv_data$Month = paste(format(csv_data$Creation.Date, format="%m"), format(csv_data$Creation.Date, format="%B"),sep="-")

#Load the data for the community areas
setwd("C:\\Users\\admin\\Documents\\R Scripts\\Graffiti\\Community_Areas")
area_data<-read.csv("community_areas.csv", header=TRUE, stringsAsFactor=FALSE)
area_data<-area_data[,c("AREA_NUMBE","COMMUNITY")]
colnames(area_data)<-c("Community.Area","Community.Name")

#Counting the number of incidents by community area and month
graffiti_data<-count(csv_data,vars = c("Community.Area","Month"))
graffiti_data<-merge(graffiti_data, area_data, by=c("Community.Area"))

#Setting 0 to community areas when no incidents available for a month
months<-unique(graffiti_data$Month)
community<-unique(graffiti_data$Community.Name)
mat_data<-matrix(0, nrow=length(community), ncol=length(months))
rownames(mat_data)<-community
colnames(mat_data)<-months

for(i in 1: length(community))
{
	for(j in 1: length(months))
	{
		val<-subset(graffiti_data$freq, (graffiti_data$Community.Name==community[i] & graffiti_data$Month %in% months[j]))
		if(!length(val) > 0)
		{
			val<-0
			tca<-subset(area_data$Community.Area, area_data$Community.Name==community[i] )
			tdf<-data.frame("Community.Area"=tca, "Month"=months[j], "freq"=0, "Community.Name"=community[i])
			graffiti_data <- rbind( graffiti_data, tdf)
		}
		mat_data[community[i],  months[j]]<-val
	}
}

colnames(graffiti_data)<-c("Community.Area", "Month", "IncidentCount","Community.Name")

#Plotting Heatmap of Community area and month
ggplot(graffiti_data, aes(Month, Community.Name)) + geom_tile(aes(fill = IncidentCount), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")  + labs(x = "Period",y = "Community", title="2015 Graffiti Removal Data in Months") + scale_x_discrete(expand = c(0, 0)) +scale_y_discrete(expand = c(0, 0))

#Loading Chicago Data
chicago <- get_map(location = 'chicago', zoom = 'auto', maptype="roadmap")

sfn <- readOGR(".","community_areas") 
sfn<-spTransform(sfn, CRS("+proj=longlat +datum=WGS84"))
sfn<-fortify(sfn)
map_value<-count(csv_data,vars = c("Community.Area"))
colnames(map_value)<-c("id","IncidentCount")
map_data<-merge(sfn, map_value, by=c("id"))

#Plotting heatmap on the map
dev.new()
ggmap(chicago) + geom_polygon(aes(x = long, y = lat, group=id, fill=IncidentCount),data = map_data, color ="black",alpha = .7, size = .2) + labs(title="2015 Graffiti Removal by Community Areas") + scale_fill_gradient(high = "#132B43", low = "#56B1F7")  
