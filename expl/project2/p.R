##data location https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

NEI$fips <- as.factor(NEI$fips) 
NEI$SCC <- as.factor(NEI$SCC) 
NEI$Pollutant <- as.factor(NEI$Pollutant) 

#1 Have total emissions from PM2.5 decreased in the United States from 
#1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 
#emission from all sources for each of the years 1999, 2002, 2005, and 2008.

totalEmissions<-aggregate(NEI$Emissions, by=list(year=NEI$year), FUN=sum )

plot(totalEmissions$year, totalEmissions$x, type="l", ylab = "Year",
     xlab="Total Emissions")

#2do make pretty
#2 Have total emissions from PM2.5 decreased in the 
#Baltimore City, Maryland (fips == "24510") from 1999 to 2008?

baltimore<-NEI[NEI$fips =='24510',]
baltimoreEmissions<-aggregate(baltimore$Emissions, by=list(year=baltimore$year), FUN=sum )

plot(baltimoreEmissions$year, baltimoreEmissions$x, type="l", xlab = "Year",
     main="Baltimore Emissions")

#3
## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
## which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
##  Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make 
##  a plot answer this question.

#qplot(year,data=baltimore,fill=type)
#gives histogram, not too useful

baltimoreByType<-aggregate(baltimore$Emissions, by=list(year=baltimore$year, type=baltimore$type), FUN=sum )


p <- ggplot(baltimoreByType, aes(x=year, y=x, group=type))
p<-p+ geom_point() + geom_line(aes(colour = type))+facet_wrap(~type  , nrow = 2, ncol = 2) 
print(p)


#4
combustionCoal<-SCC[ grepl( "combustion", SCC$SCC.Level.One , ignore.case = TRUE) 
                     & (grepl( "coal", SCC$SCC.Level.Two , ignore.case = TRUE) |
                          grepl( "coal", SCC$SCC.Level.Three , ignore.case = TRUE) |
                          grepl( "coal", SCC$SCC.Level.Four , ignore.case = TRUE))
                        
                     ,]

c <- NEI[NEI$SCC %in% combustionCoal$SCC,]

cAgg<-aggregate(c$Emissions, by=list(year=c$year), FUN=sum )

plot(cAgg$year, cAgg$x, type="l", xlab = "Year", ylab="Emissions",
     main="Coal combustion-related Emissions")

#5

onRoadScc<-SCC[ grepl( "On-Road", SCC$EI.Sector , ignore.case = TRUE) ,]
m <- baltimore[baltimore$SCC %in% onRoadScc$SCC,]
mAgg <- aggregate(m$Emissions, by=list(year=m$year), FUN=sum )
plot(mAgg$year, mAgg$x, type="l", xlab = "Year", ylab="Emissions",
     main ="Baltimore Emissions from motor vehicle sources")

#6
la<-NEI[NEI$fips =='06037',]
la_m <-la[la$SCC %in% onRoadScc$SCC,]
la_m_agg <- aggregate(la_m$Emissions, by=list(year=la_m$year), FUN=sum )

#(plot2 <- ggplot(NULL, aes(x=year,y=x)) + 
#  geom_line(data = la_m_agg ,aes(colour = "red"),show.legend = FALSE) +
#  geom_line(data = mAgg,aes(colour = "green"),show.legend = FALSE) + 
#  annotate("text", x = 2000, y = 4000, label = "LA",colour = "red")
#+ 
#  annotate("text", x = 2000, y = 4130, label = "Baltimore",colour = "green")#
#
#)

mAgg$City="Baltimore"
la_m_agg$City="LA"

tt<-data.frame(year=c(la_m_agg$year, mAgg$year),city=c(la_m_agg$City, mAgg$City),x=c(la_m_agg$x, mAgg$x))


plot(tt$year, tt$x,type="n",ylab = "Emissions",
     xlab=" Year ")
lines(la_m_agg$year, la_m_agg$x, type="l", col="black")
lines(mAgg$year, mAgg$x, type="l", col="red")


legend("topright", pch = "-", col = c("red", "black"), 
       legend = c("Baltimore", "LA"))

#plot(la_m_agg$year, la_m_agg$x, type="l", xlab = "Year", ylab="Emissions",
#     main ="Baltimore Emissions from motor vehicle sources")

