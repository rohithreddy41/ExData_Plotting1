
plot2 <- function(inputPath){
        
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
        
        #plot output type PNG
        png(filename="plot2.png", width=480,height=480) 
        
        #plot against DateTime & Global_active_power
        plot(powerDataDateRange$DateTime, powerDataDateRange$Global_active_power, type="l",xlab=NA,ylab="Global Active Power(kilowatts)")    
        dev.off()
}
