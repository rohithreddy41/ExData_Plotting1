
plot1 <- function(inputPath){
        powerData <- read.table("/Users/hadoop/Desktop/CourseEraR/ExploratoryDataAnalysis/Week 1/CourseProj1/household_power_consumption.txt", header = TRUE, sep = ";", as.is=c(1:9));
        powerData$Date <- as.Date(powerData$Date,"%d/%m/%Y")
        powerDataDateRange <- subset(powerData, (Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02")));
        powerDataDateRange$Global_active_power <- as.numeric(powerDataDateRange$Global_active_power)
        
        #plot output type PNG
        png(filename="plot1.png", width=480,height=480) 
        
        hist(powerDataDateRange$Global_active_power,xlab="Global Active Power(kilowatts)", col="RED")   
        
        dev.off()
}
