library(tidyverse)
library(stringr)
library(ggplot2)
library(ggrepel)

#Obtaining the data
iNat <- read_csv("/Users/akshayraju/Downloads/observations-197440.csv")
print.data.frame(iNat)

keeps <- c("id","observed_on","place_county_name","scientific_name")
iNat <- iNat[keeps]

AQI <- read_csv("/Users/akshayraju/Downloads/AirQualityDataCSV.csv")

#Removing headers and then creating a new df that has the first three rows content pasted together
#Then set that as the column headers for the data
names(AQI) <- NULL
AQI2 <- paste(AQI[1, ],AQI[2, ],AQI[3, ], sep = ",")
colnames(AQI) <- AQI2

#Extracting the average row and deleting unwanted data
notwhatiwant <- c(1:3)
AQI <- AQI[-notwhatiwant, ]

whatiwant3 <- c(727)
AQIMean <- AQI[whatiwant3, ]

#Pivoting the table so that observations are a seperate column (tidy data)
AQI3Mean <- pivot_longer(AQIMean,cols = 3:47, names_to = "Site",values_to = "Pollutant Levels")
AQI3Mean <- separate(AQI3Mean,Site,sep = ",",into = c("Site","Pollutant","Unit"))
removecols <- c(1,2)
AQI3Mean <- AQI3Mean[,-removecols]

#Ozone Pollutant ------------------------------

#Selecting for ozone pollutant
ozonerows <- str_which(AQI3Mean$Pollutant,"O3")
AQIOzone <- AQI3Mean[ozonerows, ]

#Assigning boroughs to each test site
borough <- c("New York","Richmond","Bronx","Bronx","Queens")
AQIOzone$Borough <- borough

#Creating a Table for Boroughs and their Area in Sq Km
nycboroughs <- c("New York","Richmond","Bronx","Queens","Kings")
countysize <- c(58.8, 148.9, 109.3, 281.5, 179.7)
countydata <- data.frame(nycboroughs,countysize)

#Creating a summary table by grouping by borough and taking the average values of each borough
AQIOzone$`Pollutant Levels` <- as.numeric(AQIOzone$`Pollutant Levels`)

AQIOzoneSum <- 
  AQIOzone %>%
    group_by(Borough) %>%
    summarise(Average = mean(`Pollutant Levels`))

#Joining the boroughs area table with the ozone summary data
AQIOzone1 <- left_join(AQIOzoneSum,countydata, by = c("Borough" = "nycboroughs"))

#Counting the number of pigeon observations per borough
iNat1 <- count(iNat,place_county_name)

#Merging pigeon observations to ozone data by borough
iNat2 <- inner_join(AQIOzone1,iNat1, by = c("Borough" = "place_county_name"))
iNat2$PigeonsPerSqKm <- iNat2$n/AQIOzone1$countysize

#Graph time!
ggplot (data = iNat2, aes(x = Average, y = PigeonsPerSqKm)) +
  geom_point(color = "blue") +
  labs(x = "Ozone Pollution (ppm)", y = "Number of Pigeons Per Square Kilometer") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_text_repel(aes(label = Borough), size = 4) +
  ggtitle("The Effect of Ozone Pollution on the Density of Pigeons by Borough")

#Calculating summary statistics on the graph
linearreg <- lm (Average ~ PigeonsPerSqKm, data = iNat2)
summary(linearreg)
cf <- coef(linearreg)
intercept <- as.numeric(cf[1])
slope <- as.numeric(cf[2])

MeanOzone <- mean(iNat2$Average)
MedianOzone <- median(iNat2$Average)
RangeOzone <- max(iNat2$Average) - min(iNat2$Average)
STDOzone <- sd(iNat2$Average)
STDErrOzone <- STDOzone / sqrt(length(iNat2$Average))

#PM25C Pollutant ------------------------------

#Selecting for PM25C pollutant
PMrows <- str_which(AQI3Mean$Pollutant,"PM25C")
AQIPM <- AQI3Mean[PMrows, ]
AQIPM <- na.omit(AQIPM)

#Assigning boroughs to each test site
borough2 <- c("New York","Richmond","New York","Queens","Bronx","Richmond","Kings","Queens")
AQIPM$Borough <- borough2

#Creating a summary table by grouping by borough and taking the average values of each borough
AQIPM$`Pollutant Levels` <- as.numeric(AQIPM$`Pollutant Levels`)

AQIPMSum <- 
  AQIPM %>%
  group_by(Borough) %>%
  summarise(Average = mean(`Pollutant Levels`))

#Joining the boroughs area table with the ozone summary data
AQIPM1 <- left_join(AQIPMSum,countydata, by = c("Borough" = "nycboroughs"))

#Merging pigeon observations to ozone data by borough
iNat3 <- inner_join(AQIPM1,iNat1, by = c("Borough" = "place_county_name"))
iNat3$PigeonsPerSqKm <- iNat3$n/AQIPM1$countysize

#Graph time part 2!
ggplot (data = iNat3, aes(x = Average, y = PigeonsPerSqKm)) +
  geom_point(color = "blue") +
  labs(x = "PM25C Pollution (ug/m3LC)", y = "Number of Pigeons Per Square Kilometer") +
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  geom_text_repel(aes(label = Borough), size = 4) +
  ggtitle("The Effect of PM25C Pollution on the Density of Pigeons by Borough")

#Calculating summary statistics on the graph
linearreg2 <- lm (Average ~ PigeonsPerSqKm, data = iNat3)
summary(linearreg2)
cf2 <- coef(linearreg2)
intercept2 <- as.numeric(cf2[1])
slope2 <- as.numeric(cf2[2])

MeanPM <- mean(iNat3$Average)
MedianPM <- median(iNat3$Average)
RangePM <- max(iNat3$Average) - min(iNat3$Average)
STDPM <- sd(iNat3$Average)
STDErrPM <- STDOzone / sqrt(length(iNat3$Average))




#Ignore everything below this
#Code graveyard (ooooh ~spooky~)

#group_by(AQI,Date)
#summarize(AQI4, .groups = "Site","Date")

#notwhatiwant2 <- c(721:730)
#AQI <- AQI[-notwhatiwant2, ]

#AQI3Mean <- AQI3Mean[ ,-5]

#AQI3 <- pivot_longer(AQI,cols = 3:47, names_to = "Site",values_to = "Pollutant Levels")
#AQI3 <- separate(AQI3,Site,sep = ",",into = c("Site","Pollutant","Unit"))
#names(AQI3)[1] <- "Date"
#names(AQI3)[2] <- "Time"
#AQI3 <- AQI3[ ,-5]

#AQI4 <- pivot_wider(AQI3, names_from = "Pollutant", values_from = "Pollutant Levels")
#AQI4$Date <- as.Date(AQI4$Date)

#group_by(AQI4,Site)
#group_by(AQI4,Date)
#summarize(AQI4, .groups = "Site","Date")

#AQI3$Site <- str_replace(AQI3$Site,'c','')
#AQI3$Site <- str_replace(AQI3$Site,'\\(','')
#AQI3$Site <- str_replace(AQI3$Site,'\\\\','')
#AQI3$Site <- gsub("\\\\", "", AQI3$Site)
  
  
  

  
  
  
  
  
  
  