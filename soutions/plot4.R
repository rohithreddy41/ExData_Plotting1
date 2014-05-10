plot4 <- function(inputPath){
        
        powerData <- read.table(inputPath, header = TRUE, sep = ";", as.is=c(1:9));
        #converting to date so i can filter data based on dates in next step.
        powerData$Date <- as.Date(powerData$Date,"%d/%m/%Y")
        #filtering data for given date range
        powerDataDateRange <- subset(powerData, (Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02")));
        #converting back to character to concatenate both Date and Time variables into DateTime
        powerDataDateRange$Date <- as.character(powerDataDateRange$Date)
        #concatenating
        powerDataDateRange <- within(powerDataDateRange, DateTime <- paste(Date, Time, sep=" "))
        #conversion to POSIXlt
        powerDataDateRange$DateTime <- strptime(powerDataDateRange$DateTime, "%Y-%m-%d %H:%M:%S",tz = "")
        #plot against DateTime & Global_active_power

        #plot output type PNG
        png(filename="plot4.png", width=480,height=480) 
        
        par(mfrow = c(2,2))
        
        plot(powerDataDateRange$DateTime, powerDataDateRange$Global_active_power, type="l",xlab=NA,ylab="Global Active Power")
        
        plot(powerDataDateRange$DateTime, powerDataDateRange$Voltage, type="l",xlab="datetime",ylab="Voltage") 
        
        #plot against DateTime & Energy sub metering
        plot(powerDataDateRange$DateTime,powerDataDateRange$Sub_metering_1,col="black", type="l", xlab=NA,ylab="Energy sub metering")
        lines(powerDataDateRange$DateTime,powerDataDateRange$Sub_metering_2,col="red", type="l")
        lines(powerDataDateRange$DateTime,powerDataDateRange$Sub_metering_3,col="blue", type="l")
        legend("topright",cex=0.7,lty=1,bty="n", col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        plot(powerDataDateRange$DateTime, powerDataDateRange$Global_reactive_power, type="l",xlab="datetime",ylab="Global_reactive_power")
        dev.off()
}