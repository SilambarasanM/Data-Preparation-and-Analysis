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
if (!require("clusterSim")) {
 install.packages("clusterSim", dependencies = TRUE)
 library(rgdal)
 }
#Loading Source Files
path = "C:\\Users\\admin\\Documents\\R Scripts\\Classifier"
setwd(path)
source("getGroceryCount.R")
source("getExpectancy.R")
source("getCrimeCount.R")
source("getHousingData.R")
source("getHardshipIndex.R")

grocery_count<-getGroceryCount("GroceryStores_2013.csv", path)
life_expectancy<-getExpectancy("Life_Expectancy_2000.csv", path)
crime_count<-getCrimeCount("Crimes_2015.csv", path)
housing_data<-getHousingData("Affordable_Rental_Housing_Developments.csv", path)
hardship_index<-getHardshipIndex("SocioEconomicData_2008_2012.csv", path)

#Loading Community Areas Information
setwd("C:\\Users\\admin\\Documents\\R Scripts\\Classifier\\Community_Areas")
area_data<-read.csv("community_areas.csv", header=TRUE, stringsAsFactor=FALSE)
area_data<-area_data[,c("AREA_NUMBE","COMMUNITY")]
colnames(area_data)<-c("Community.Area","Community.Name")

#Filling Missing values
community<-unique(area_data$Community.Area)
for(i in 1: length(community)) {
	if(!community[i] %in% grocery_count$Community.Area) {
		gcount <- data.frame(Community.Area = community[i], Grocery.Count = 0)
        	grocery_count <- rbind(grocery_count, gcount)
	} 
	
	if(!community[i] %in% housing_data$Community.Area) {
		hd <- data.frame(Community.Area = community[i], Housing.Units = 0)
        	housing_data <- rbind(housing_data, hd)
	}
}
grocery_count<-grocery_count[ order(grocery_count$Community.Area), ]
housing_data<-housing_data[ order(housing_data$Community.Area), ]

#Loading Community Areas Map Info
sfn <- readOGR(".","community_areas", stringsAsFactors=FALSE)
sfn<-spTransform(sfn, CRS("+proj=longlat +datum=WGS84"))

ids<-sapply(slot(sfn, "polygons"), function(x) slot(x, "ID"))
can<-sfn$area_numbe
lookup<-do.call(rbind, Map(data.frame, Community.Area=can, id=ids))
lookup$Community.Area<-as.numeric(levels(lookup$Community.Area))[lookup$Community.Area]
lookup$id<-as.numeric(levels(lookup$id))[lookup$id]
lookup<-lookup[order(lookup$Community.Area),]
sfn<-fortify(sfn)

#Merging all data into one data frame - Profiling each Community Area
community_profile<- data.frame(grocery_count$Community.Area, grocery_count$Grocery.Count, life_expectancy$Life.Expectancy, crime_count$Crime.Count, housing_data$Housing.Units, hardship_index$Hardship.Index)
colnames(community_profile)<-c("Community.Area", "Grocery.Count", "Life.Expectancy", "Crime.Count", "Housing.Units", "Hardship.Index")

normalized_data<-community_profile[,-1]
normalized_data<-data.Normalization(normalized_data, type="n10", normalization="column")
data_matrix<-data.matrix(normalized_data)
data_matrix <- t(data_matrix)
data_matrix <- data_matrix * 100
barplot(data_matrix, main="Features of Community Areas", xlab="Community Areas", ylab="Percentage", col=rainbow(5), legend = rownames(data_matrix))


#Assigning correct Polygon IDs to the Community Areas
grocery_count$id<-"0"
life_expectancy$id<-"0"
crime_count$id<-"0"
housing_data$id<-"0"
hardship_index$id<-"0"
community_profile$id<-"0"

for (i in 1: length(community)){
	grocery_count$id[i] <- lookup$id[i]
	life_expectancy$id[i] <- lookup$id[i]
	crime_count$id[i] <- lookup$id[i]
	housing_data$id[i] <- lookup$id[i]
	hardship_index$id[i] <- lookup$id[i]
	community_profile$id[i]  <- lookup$id[i]
}

#Merging Community areas to corresponding Community Map info
grocery_map_data<-merge(sfn, grocery_count, by=c("id"))
expectancy_map_data<-merge(sfn, life_expectancy, by=c("id"))
crime_map_data<-merge(sfn, crime_count, by=c("id"))
housing_map_data<-merge(sfn, housing_data, by=c("id"))
hardship_map_data<-merge(sfn, hardship_index, by=c("id"))



#Loading Chicago Map
chicago <- get_map(location = 'chicago', zoom = 'auto', maptype="roadmap")

ggmap(chicago) + geom_polygon(aes(x = long, y = lat, group=id, fill=Grocery.Count),data = grocery_map_data, color ="black",alpha = .7, size = .2) + labs(title="Grocery Store Density by Community Areas (2013)") + scale_fill_gradient(high = "#56B1F7", low = "white")  
dev.new()
ggmap(chicago) + geom_polygon(aes(x = long, y = lat, group=id, fill=Life.Expectancy),data = expectancy_map_data, color ="black",alpha = .7, size = .2) + labs(title="Life Expectancy by Community Areas (2010)") + scale_fill_gradient(high = "#56B1F7", low = "#132B43")  
dev.new()
ggmap(chicago) + geom_polygon(aes(x = long, y = lat, group=id, fill=Crime.Count),data = crime_map_data, color ="black",alpha = .7, size = .2) + labs(title="Crime Incidents by Community Areas (2015)") + scale_fill_gradient(high = "#132B43", low = "#56B1F7")  
dev.new()
ggmap(chicago) + geom_polygon(aes(x = long, y = lat, group=id, fill=Housing.Units),data = housing_map_data, color ="black",alpha = .7, size = .2) + labs(title="Affordable Housing Developments by Community Areas (2013)") + scale_fill_gradient(high = "#132B43", low = "#56B1F7")  
dev.new()
ggmap(chicago) + geom_polygon(aes(x = long, y = lat, group=id, fill=Hardship.Index),data = hardship_map_data, color ="black",alpha = .7, size = .2) + labs(title="Hardship Index by Community Areas (2008-2012)") + scale_fill_gradient(high = "#132B43", low = "#56B1F7")  

#Classification Rules
community_profile$class<-0
for (i in 1:nrow(community_profile)){

	if (community_profile$Hardship.Index[i] < 25){
		if((community_profile$Life.Expectancy[i] > 77.6) | ((community_profile$Grocery.Count[i] > 10) | (community_profile$Crime.Count[i] <= 1315))){
			community_profile$class[i] = 1
		}
	}
	if (community_profile$class[i] == 0){
		if ((community_profile$Hardship.Index[i] <=50) & ((community_profile$Housing.Units[i] >350) | (community_profile$Crime.Count[i] > 3383) | (community_profile$Life.Expectancy[i] > 77.6))){
				community_profile$class[i] = 2	
		}	
	}
	if (community_profile$class[i] == 0){
		if ((community_profile$Hardship.Index[i] <=50) & (community_profile$Crime.Count[i] <=3383)){
				community_profile$class[i] = 3	
		}	
		else{
			if ((community_profile$Hardship.Index[i] <=75) & ((community_profile$Housing.Units[i] >350) | (community_profile$Crime.Count[i] > 3383) | (community_profile$Life.Expectancy[i] > 77.6))){
				community_profile$class[i] = 3	
			}
		}
	}
	if (community_profile$class[i] == 0){
		if ((community_profile$Hardship.Index[i] <=75) & (community_profile$Crime.Count[i] <=3383)){
				community_profile$class[i] = 4
		}	
		else{
			if((community_profile$Hardship.Index[i] > 75) & ((community_profile$Housing.Units[i] >350) | (community_profile$Crime.Count[i] > 3383) | (community_profile$Life.Expectancy[i] > 77.6))){
				community_profile$class[i] = 4	
			}
		}
	}
	if (community_profile$class[i] == 0)
		community_profile$class[i] = 5
}

#Plotting the Classifications on the Chicago Map
community_profile<-merge(sfn, community_profile, by=c("id"))
dev.new()
ggmap(chicago) + 
geom_polygon(aes(x = long, y = lat, group=id, fill=class),data = community_profile, color ="black",alpha = .7, size = .2) + 
labs(title="Community Areas Classification") +
scale_fill_gradient(high = "#132B43", low = "#56B1F7")