# #####################################################
# Store initial directory & change to the new directory
# -----------------------------------------------------
setwd("C:\\xampp\\htdocs\\d3\\1. My works\\Accidents")
initial.dir<-getwd()
datadir <- "1. Data/"
sourcedir <- "0. Scripts/"

# #####################################################
# Load libraries
# -----------------------------------------------------
library(reshape2)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
# #####################################################

#Chosen year
Year <- 2013

#######################################################
################### WEATHER DATA ######################
#######################################################

#Read in weather data
weather <- read.table(paste(datadir,"wheather data de Bilt.txt",sep=""), header=F, 
                      sep=",", stringsAsFactors=F)
names(weather) <- c("STN","YYYYMMDD","DDVEC","FHVEC","FG","FHX","FHXH","FHN","FHNH","FXX",
                    "FXXH","TG","TN","TNH","TX","TXH","T10N","T10NH","SQ","SP","Q","DR",
                    "RH","RHX","RHXH","PG","PX","PXH","PN","PNH","VVN","VVNH","VVX","VVXH",
                    "NG","UG","UX","UXH","UN","UNH","EV24","X")

weather <- weather[,c("YYYYMMDD","RH","TG")]
#TG = Etmaalgemiddelde temperatuur (in 0.1 graden Celsius)
#RH = Etmaalsom van de neerslag (in 0.1 mm) (-1 voor <0.05 mm)

#Fix precipitation and change to mm
weather$RH[is.na(weather$RH)] <- 0
weather$RH[weather$RH == -1] <- 0
weather$RH <- weather$RH/10

#Set temperatur to full degrees
weather$TG <- weather$TG/10

#Create actual date & select only 2010 - 2014
weather$YYYYMMDD <- as.Date(as.character(weather$YYYYMMDD), "%Y%m%d")
weather$year <- as.numeric(format(weather$YYYYMMDD, "%Y"))
weather <- weather[which(weather$year >= 2010 & weather$year <= 2014),]

names(weather) <- c("date","rain","temp","year")
row.names(weather) <- NULL

#######################################################
################### SUNRISE DATA ######################
#######################################################

# #Webscrape the sunrise and Sunset times of Amsterdam for chosen year
# library(rvest)
# library(stringr)
# 
# Months <- c("january","february","march","april","may","june","july","august",
#             "september","october","november","december")
# 
# sunrise <- data.frame(date = character(), morningTwilight = character(),
#                       sunrise = character(), sunset=character(),
#                       eveningTwilight = character(), stringsAsFactors=F)
# for (i in 1:length(Months)) {
#   for(j in 1:31) {
#     
#     if(Months[i] %in% c("february","april","june","september","november") & j == 31) next
#     if(Months[i] == "february" & j %in% c(29, 30)) next
#     
#     
#     url <- html(paste("http://www.sunrise-and-sunset.com/en/netherlands/amsterdam/",Year,"/",Months[i],"/",j,sep=""))
#     
#     webData <- (url %>% html_nodes("tbody tr td") %>% html_text())[1:(3*7)]
#     webDF <- data.frame(date=paste("2013",str_sub(paste("0",i,sep=""),start=-2, end=-1),
#                                    str_sub(paste("0",j,sep=""),start=-2, end=-1),sep=""), 
#                         morningTwilight=webData[8],
#                         sunrise=webData[11],sunset=webData[12],
#                         eveningTwilight=webData[15])
#     
#     sunrise <- rbind(sunrise, webDF)
#     
#   }#for j
# }#for i
# write.csv2(sunrise, file=paste(datadir,"sunrise times",Year,".csv",sep=""), row.names=F)

sunrise <- read.csv2(paste(datadir,"sunrise times",Year,".csv",sep=""), stringsAsFactors=F)

sunrise$morningTwilight <- strptime(paste(sunrise$date,sunrise$morningTwilight), "%Y%m%d %H:%M")
sunrise$sunrise <- strptime(paste(sunrise$date,sunrise$sunrise), "%Y%m%d %H:%M")
sunrise$sunset <- strptime(paste(sunrise$date,sunrise$sunset), "%Y%m%d %H:%M")
#sunrise$eveningTwilight <- strptime(paste(sunrise$date,sunrise$eveningTwilight), "%Y%m%d %H:%M")
sunrise$date <- as.Date(as.character(sunrise$date), "%Y%m%d")

sunrise$daylight <- as.numeric(difftime(sunrise$sunset, sunrise$sunrise, units="hours"))
sunrise$twilight <- as.numeric(difftime(sunrise$sunrise, sunrise$morningTwilight, units="hours"))


#######################################################
################### ACCIDENT DATA #####################
#######################################################

accidents <- read.table(paste(datadir,"ongevallen 2013.txt",sep=""), header=T, 
                        sep=",", stringsAsFactors=F)

accidents <- accidents[,c("DATUM_VKL","DAG_CODE","MND_NUMMER","JAAR_VKL")]
accidents$DATUM_VKL <- as.Date(as.character(accidents$DATUM_VKL), "%Y%m%d")
accidents$day <- as.numeric(format(accidents$DATUM_VKL, "%d"))

accidents <- dcast(accidents, DATUM_VKL + DAG_CODE + day + MND_NUMMER + JAAR_VKL ~ .,  
                   fun.aggregate=length)
names(accidents) <- c("date","weekday","day","month","year","accidents")

#######################################################
#################### MERGE DATA #######################
#######################################################

data <- merge(accidents, weather[,c("date","rain","temp")], by="date", all.x=T)
data <- merge(data, sunrise[,c("date","daylight","twilight")], by="date", all.x=T)
#data13 <- data[data$year == 2013,]

#######################################################
#################### PLOT DATA #######################
#######################################################

#Theme: styling elements for plots
plotTheme <-   
  theme_bw() +
  theme(
    panel.grid.major.x=element_blank(),
    #panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    panel.border = element_blank(),
    panel.background=element_blank()
  )


#Plots
p <- ggplot(data, aes(x=date, y=accidents)) + 
  geom_line() +
  stat_smooth(method="loess", level = 0.99, span=0.1) +
  plotTheme
plot(p)
ggsave(plot=p, filename="accidents.pdf", width=40, height=5, units="cm")

p <- ggplot(data, aes(x=date,y=daylight)) + 
  geom_ribbon(aes(ymin=daylight-twilight, ymax=daylight+twilight), alpha=0.3) +
  geom_line() +
  plotTheme
plot(p)
ggsave(plot=p, filename="daylight.pdf", width=40, height=10, units="cm")

p <- ggplot(data, aes(x=date,y=rain)) + 
  geom_bar(stat="identity") + 
  plotTheme
plot(p)
ggsave(plot=p, filename="rain.pdf", width=40, height=10, units="cm")

p <- ggplot(data, aes(x=date, y=accidents)) + 
  geom_line() +
  geom_point() +
  stat_smooth(method="loess", level = 0.99, span=0.1) +
  geom_bar(aes(x=date,y=rain*5), stat="identity") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b"))
plot(p)

