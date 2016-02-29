## CODE TO GET THE IDS FROM SCRAPPING THE HTML PAGE 

webpage = readLines('Qdoba1.html')
webpage1 <- grep('<tr bizname="Qdoba">|<tr bizname="Qdoba Mexican Grill">',webpage)
webpage2 <- webpage1+1
##thepage1 <- grep('input type="checkbox"',webpage)
thepage2 <- webpage[webpage2]
thepage2 <- gsub("\"","",thepage2)
thepage2 <- gsub("\\s", "",thepage2)
thepage2 <- gsub("<td><inputtype=checkboxid=","",thepage2)
thepage2 <- gsub("></input></td>","",thepage2)
thepage2 <- as.data.frame(as.vector(thepage2))
write.csv(thepage2,"qdobaids.csv")


library(ggmap)
library(mapproj)
map <- get_map(location = 'USA', zoom = 4)
ggmap(map)

library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap)

plot(newmap,
     xlim = c(-20, 59),
     ylim = c(35, 71),
     asp = 1
)

routes <- read.csv("http://openflights.svn.sourceforge.net/viewvc/openflights/openflights/data/routes.dat", header=F)
colnames(routes) <- c("airline", "airlineID", "sourceAirport", "sourceAirportID", "destinationAirport", "destinationAirportID", "codeshare", "stops", "equipment")
head(routes)

airportD <- read.csv("C://Users//nareshpola//Desktop//business.csv",header=T)
mapPoints <- ggmap(map) + geom_point(aes(x = longitude, y = latitude, size = size), data = airportD, alpha = .5)
mapPoints


## CODE TO GET pos/neg/neutral review counts for each businessid
setwd("C://Users//nareshpola//Desktop//scripts//R")
bus <- read.csv("StarB-590-locations//report-datasets//SBbusiness1.csv",header=T,stringsAsFactors = F)
bus <- bus[,c("businessid","street","city","state","longitude","latitude","geolocation")]
busreviews <- read.csv("StarB-590-locations//report-datasets//SBbusreviews1.csv",header=T,stringsAsFactors = F)
busout <- join(bus,busreviews,type="inner")
cols <- c("street","city","state")
busout$location <- Reduce(function(...) paste(...,sep=","),busout[,cols])
busoutUniq <- unique(busout[,c("businessid","location","latitude","longitude","geolocation")])
busoutPos <- filter(busout,reviewtype=="positive")
busoutPos1 <- busoutPos %>% group_by(businessid) %>% summarize(posCount=n())
busoutNeg <- filter(busout,reviewtype=="negative")
busoutNeg1 <- busoutNeg %>% group_by(businessid) %>% summarize(negCount=n())
busoutNeu <- filter(busout,reviewtype=="neutral")
busoutNeu1 <- busoutNeu %>% group_by(businessid) %>% summarize(neuCount=n())
busout1 <- join(busoutPos1,busoutNeg1,type="left")
busout2 <- join(busout1,busoutNeu1,type="left")

busoutFinal <- join(busoutUniq,busout2,type="left")
write.csv(busoutFinal,"StarB-590-locations//report-datasets//SBbusrevcntFrTableu.csv")
