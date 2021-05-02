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


## Task 3
caro <- read_delim("caro60.csv",",") # adjust path
caro
caro <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE) # setting remove = FALSE preserves the original (E/N) columns, which come in handy later on
caro

caro_1 <- mutate(caro,timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))
caro_1<-caro_1%>%
  mutate(E2=lead(E,n=1))%>%
  mutate(N2=lead(N,n=1))%>%
  mutate(steplength=sqrt((E-E2)^2+(N-N2)^2))%>%
  mutate(speed=steplength/timelag)




seq_3<-seq(from=1,to=200,by=3)
caro_3<-slice(caro,seq_3)

seq_6<-seq(from=1,to=200,by=6)
caro_6<-slice(caro,seq_6)

seq_9<-seq(from=1,to=200,by=9)
caro_9<-slice(caro,seq_9)

caro_3 <- mutate(caro_3,timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))

caro_3<-caro_3%>%
  mutate(E2=lead(E,n=1))%>%
  mutate(N2=lead(N,n=1))%>%
  mutate(steplength=sqrt((E-E2)^2+(N-N2)^2))%>%
  mutate(speed=steplength/timelag)


caro_6 <- mutate(caro_6,timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))

caro_6<-caro_6%>%
  mutate(E2=lead(E,n=1))%>%
  mutate(N2=lead(N,n=1))%>%
  mutate(steplength=sqrt((E-E2)^2+(N-N2)^2))%>%
  mutate(speed=steplength/timelag)

caro_9 <- mutate(caro_9,timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))

caro_9<-caro_9%>%
  mutate(E2=lead(E,n=1))%>%
  mutate(N2=lead(N,n=1))%>%
  mutate(steplength=sqrt((E-E2)^2+(N-N2)^2))%>%
  mutate(speed=steplength/timelag)


p_caro<-ggplot(data=caro, aes(E, N, color="1 minute"))+
  geom_point( alpha=0.5)
p_caro

p_caro_3<-p_caro+geom_point(data=caro_3, aes(color="3 minutes"))+
  geom_path(data=caro_3, aes(color="3 minutes"))

## attention: define the color in the aesthetics! HadleyWhickham: An aesthetic is a visual property of the objects in your plot. Aesthetics include things like the size, the shape, or the color of your points. 


p_caro_6<-p_caro+geom_point(data=caro_6, aes(color="6 minutes"))+
  geom_path(data=caro_6, aes(color="6 minutes") )

p_caro_3
p_caro_6

p_caro_9<-p_caro+geom_point(data=caro_9, aes(color="9 minutes"))+
  geom_path(data=caro_9, aes(color="9 minutes" ))


p_caro_9


p_speed<-ggplot(caro_1, aes(DatetimeUTC,speed,color="1 minute"))+
  geom_line()+
  geom_line(data=caro_3, aes(color="3 minutes"))+
  geom_line(data=caro_6,aes(color="6 minutes"))+
  geom_line(data=caro_9,aes(color="9 minutes"))+
  labs(color="Trajectory")

p_speed

#interpretation: the smaller the granularity (e.g. 9 minutes), the smaller the speed. This occurs because we subset several locations which are then connected linarly. 
# the animal seems to walk less in the same time interval which reduces  speed obviously.

















# #other 
# 
# ggplot(wildschwein_BE, aes(colour=TierName))+
#   geom_sf()+
#   coord_sf(datum=2056)
# 
# # second try: 
# 
# ggplot(wildschwein_BE, aes(colour=log(timelag)))+
#   geom_sf()+
#   coord_sf(datum=2056)





