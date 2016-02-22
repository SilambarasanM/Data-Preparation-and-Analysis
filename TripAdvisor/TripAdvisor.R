library("jsonlite")
library("plyr")

#First set the location of the JSON files here
setwd("C:\\Users\\admin\\Documents\\R Scripts\\TripAdvisor\\json")
filesList=list.files(pattern="*.json")

#Local Variables Initialization
json_list<-NULL
location<-NULL
hotel_names<-NULL
hotel_price<-NULL
street_names<-NULL
city_names<-NULL
state_names<-NULL
country_names<-NULL
postal_codes<-NULL
hotel_address<-NULL
skippedFiles<-NULL

#List of US States
USStates<-c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA",
"KS", "KY", "LA", "ME", "MD", "MA",  "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM",
"NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA",
"WV", "WI", "WY")

#Function to extract different parts of address from HTML tags in JSON files
getLocality<-function(address, part)
{
	if(part=="city")
	{
		cLen<-12
		checkStr<-"v:locality"
	}	
	if(part=="country")
	{
		cLen<-16
		checkStr<-"v:country-name"
	}
	if(part=="state")
	{
		cLen<-10
		checkStr<-"v:region"
	}
	if(part=="street")
	{
		cLen<-18
		checkStr<-"v:street-address"
	}
	if(part=="postal")
	{
		cLen<-15
		checkStr<-"v:postal-code"
	}

	#Extraction Logic
	if(regexpr(checkStr,hAddress)!= -1)
	{
		cStart<-regexpr(checkStr,hAddress)+cLen
		cStop<-regexpr("</span>", substr(hAddress,cStart, nchar(hAddress)))-2+cStart
		return(substr(hAddress,cStart,cStop))
	}
	else
		return(NA)
}

#Function to concatenate address into one string
concatenate <- function(...,sep=", ") {
     L <- list(...)
     L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
     ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
                 gsub(paste0(sep,sep),sep,
                      do.call(paste,c(L,list(sep=sep)))))
     is.na(ret) <- ret==""
     ret
     }

#Iterating through all JSON files to extract required data
for(i in 1: length(filesList))
{
	hAddress<-NULL
	print(paste0(Sys.time(),": ","Loading file ", i ,": ", filesList[i]))
	json_file <- filesList[i]
	json_data <- fromJSON(txt=json_file)
	json_frame <-tryCatch({as.data.frame(json_data)}, error=function(e) NULL )

	#Processing only the files with the required data
	if(!is.null(json_frame) && !is.null(json_frame$HotelInfo.Address)) 
	{
		#json_list is the variable that contains the entire data from JSON files
		json_list<-c(json_list,json_frame)

		#Hotel Names
		hName<-as.character(json_frame$HotelInfo.Name[1])
		hotel_names<-c(hotel_names, hName)

		#Hotel Price
		hP<-as.character(json_frame$HotelInfo.Price[1])
		hP[hP=="0"]<-NA
		hotel_price<-c(hotel_price, hP)

		#Hotel Address
		hAddress<-as.character(json_frame$HotelInfo.Address[1]) 
		ctName<-getLocality(hAddress,"city")
		cName<-getLocality(hAddress,"country")
		sName<-getLocality(hAddress,"state")
		stName<-getLocality(hAddress,"street")
		pCode<-getLocality(hAddress,"postal")
		hAdd<-concatenate(stName, ctName, sName, cName, pCode)
		hotel_address<-c(hotel_address, hAdd)
		postal_codes<-c(postal_codes,pCode)
		city_names<-c(city_names, ctName)
		street_names<-c(street_names, stName)
		state_names<-c(state_names, sName)
		country_names<-c(country_names, cName)

	}
	else 
	{	#Ignoring files with insufficient data
		print(paste("Insufficient data. File",filesList[i],"skipped"))
		skippedFiles<-c(skippedFiles,filesList[i])
	}
}

#Unique Cities List
location<-data.frame(city_names,state_names,country_names)
names(location)<-c("city","state","country")
ucity_names<-unique(city_names)
print("Distinct City names identified. ")
ucity_names<-list(UniqueCityNames=ucity_names[!is.na(ucity_names)])
write.csv(ucity_names,file="2a_DistinctCities.csv")

#US Cities List
USCities<-city_names[state_names %in% USStates]
USCities<-unique(USCities)
print("US City names filtered. ")
USCities<-list(USCityNames=USCities[!is.na(USCities)])
write.csv(USCities,file="2b_USCities.csv")

#Cities with more than 30 hotels
HiFreqCT<-count(location,c("city"))
HiFreqCT<-HiFreqCT[HiFreqCT["freq"]>30,]
colnames(HiFreqCT) <- c("Cities", "No.ofHotels")
print("Shortlisted cities with more than 30 hotels.")
HiFreqCT<-HiFreqCT[!is.na(HiFreqCT["Cities"]),]
write.csv(HiFreqCT,file="3_Cities with more than 30 hotels.csv", row.names=FALSE)

#Gathering Hotel Details
HotelDetails<- matrix(c(hotel_names, hotel_price, hotel_address),ncol=3)
colnames(HotelDetails) <- c("HotelName","Price","Address")
HotelDetails<- as.table(HotelDetails)	#Table containing Hotel Details
print("Hotel Details Ready!")
write.csv(HotelDetails,file="4_HotelDetails.csv", row.names=FALSE)

#Clear Workspace Memory
#rm(list=ls())