## Load the necessary libraries ################################################

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

## Import the downloaded csv ##################################################

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",") # adjust path
wildschwein_BE
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE) # setting remove = FALSE preserves the original (E/N) columns, which come in handy later on
wildschwein_BE


## Task 1: Getting an overview
wildschwein_BE<- group_by(wildschwein_BE,TierID)
wildschwein_BE <- mutate(wildschwein_BE,timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))
wildschwein_BE
str(wildschwein_BE)
summary(wildschwein_BE)

# no of individuals tracked: 
wildschwein_BE$TierID<-as.factor(wildschwein_BE$TierID)
wildschwein_BE$TierName<-as.factor(wildschwein_BE$TierName)
summary(wildschwein_BE)
# answer: 3, Rosa, Ruth and Sabi


# for how long tracked: 
time_difference <- difftime(max(wildschwein_BE$DatetimeUTC),min(wildschwein_BE$DatetimeUTC))
time_difference
# answer: 338.6 days

# concurrently or sequentially tracked? Gaps?
wildschwein_BE_CS <- wildschwein_BE %>%
  group_by(TierName) %>%
  summarise(min=min(DatetimeUTC),
         max=max(DatetimeUTC),
         time_difference=difftime(max,min))
wildschwein_BE_CS

ggplot(wildschwein_BE, aes(TierName, DatetimeUTC))+
       geom_point()


ggplot(wildschwein_BE, aes(timelag))+
  geom_histogram(binwidth=100)+
  scale_y_log10()
  
#answer: as far as i can tell sabi was tracked for the longest period and the others were additionally tracked some time after sabi and from this time on concurrently.
#There are gaps within the timelaps

# temporal sampling interval between the locations?

wilschwein_BE_mean<-wildschwein_BE%>%     # Take wildschwein...
  group_by(TierID) %>%                    # ...group it by TierID
  summarise(                              # Summarise the data...
    mean_timelag = mean(timelag,na.rm = T)# ...by calculating the mean timelag
  )

wilschwein_BE_mean$mean_timelag/60
# answer: the temportal samplin intervall is in average 21-26 min



##Task2
wildschwein_BE<-wildschwein_BE%>%
  group_by(TierName)%>%
  mutate(E2=lead(E,n=1))%>%
  mutate(N2=lead(N,n=1))%>%
  mutate(steplength=sqrt((E-E2)^2+(N-N2)^2))%>%
  mutate(speed=steplength/timelag)

summary(wildschwein_BE)

# The unit of speed is in metre per second, since the timlag was set to seconds and the coordinate system EPSG:2056 is in metres. 





#other 

ggplot(wildschwein_BE, aes(colour=TierName))+
  geom_sf()+
  coord_sf(datum=2056)

# second try: 

ggplot(wildschwein_BE, aes(colour=log(timelag)))+
  geom_sf()+
  coord_sf(datum=2056)



