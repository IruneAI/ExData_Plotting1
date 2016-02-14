plot1 <- function() {
  #importing data
  householdata <- read.csv("household_power_consumption.txt", sep=";",stringsAsFactors=FALSE)
  cls <- names(householdata)
  coercedata <- householdata[, cls[3:9]]
  #coercioning and handling missing values (?) ?--> NA
  householdata[,cls[3:9]] = apply(coercedata, 2, function(x) as.numeric(x));
  #handling Date Types
  householdata["Date"] <- as.Date(householdata$Date,"%d/%m/%Y")
  #subsetting data (dates 2007-02-01 and 2007-02-02)s
  subsetdata <- subset(householdata, householdata$Date >= "2007-02-01" & householdata$Date  <= "2007-02-02")
  #plot 1
  #open device
  png(filename="plot1.png", width = 480, height = 480, units = "px")
  hist(subsetdata$Global_active_power,main="Global Active Power", xlab = "Global Active Power (kilowatts)" , col="red")
  #close device
  dev.off()
}