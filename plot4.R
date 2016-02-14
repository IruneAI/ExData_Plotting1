plot4 <- function() {
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
  #open device
  png(filename="plot4.png", width = 480, height = 480, units = "px")
  par(mfrow= c(2,2))
  plot(as.POSIXct(paste(as.character.Date(subsetdata$Date), subsetdata$Time), format="%Y-%m-%d %H:%M:%S"),subsetdata$Global_active_power, type="l", ann=FALSE)
  title(ylab="Global Active Power")
  plot(as.POSIXct(paste(as.character.Date(subsetdata$Date), subsetdata$Time), format="%Y-%m-%d %H:%M:%S"),subsetdata$Voltage, type="l", ann=FALSE)
  title(ylab="Voltage", xlab = "datetime")
  yrang <- range(c(subsetdata$Sub_metering_1,subsetdata$Sub_metering_2,subsetdata$Sub_metering_3))
  plot(as.POSIXct(paste(as.character.Date(subsetdata$Date), subsetdata$Time), format="%Y-%m-%d %H:%M:%S"), subsetdata$Sub_metering_1, type="l", ylim = yrang,ann=FALSE)
  lines(as.POSIXct(paste(as.character.Date(subsetdata$Date), subsetdata$Time),format="%Y-%m-%d %H:%M:%S") ,subsetdata$Sub_metering_2, col = "red", type = "l")
  lines(as.POSIXct(paste(as.character.Date(subsetdata$Date), subsetdata$Time), format="%Y-%m-%d %H:%M:%S"),subsetdata$Sub_metering_3, col = "blue", type = "l")
  title(ylab="Energy sub metering")
  #handling legend
  legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty = 1, col= c("black", "red", "blue") , bty = 'n')
  Sys.setlocale("LC_TIME", "C")
  plot(as.POSIXct(paste(as.character.Date(subsetdata$Date), subsetdata$Time), format="%Y-%m-%d %H:%M:%S"),subsetdata$Global_reactive_power, type="l", ann=FALSE)
  title(ylab="Global_reactive_power",xlab = "datetime")
  #close device
  dev.off()
}