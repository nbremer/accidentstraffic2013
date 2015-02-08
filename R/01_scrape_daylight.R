#Webscrape the sunrise and Sunset times of Amsterdam for chosen year
library(rvest)
library(stringr)

Year <- 2013
Months <- c("january","february","march","april","may","june","july","august",
            "september","october","november","december")

sunrise <- data.frame(date = character(), morningTwilight = character(),
                      sunrise = character(), sunset=character(),
                      eveningTwilight = character(), stringsAsFactors=F)

for (i in 1:length(Months)) {
  for(j in 1:31) {
    
    if(Months[i] %in% c("february","april","june","september","november") & j == 31) next
    if(Months[i] == "february" & j %in% c(29, 30)) next
    
    
    url <- html(paste("http://www.sunrise-and-sunset.com/en/netherlands/amsterdam/",Year,"/",Months[i],"/",j,sep=""))
    
    webData <- (url %>% html_nodes("tbody tr td") %>% html_text())[1:(3*7)]
    webDF <- data.frame(date=paste("2013",str_sub(paste("0",i,sep=""),start=-2, end=-1),
                                   str_sub(paste("0",j,sep=""),start=-2, end=-1),sep=""), 
                        morningTwilight=webData[8],
                        sunrise=webData[11],sunset=webData[12],
                        eveningTwilight=webData[15])
    
    sunrise <- rbind(sunrise, webDF)
    
  }#for j
}#for i
write.csv2(sunrise, file=paste(datadir,"sunrise times",Year,".csv",sep=""), row.names=F)