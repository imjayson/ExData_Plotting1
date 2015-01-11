library(data.table)
plot1 <- function() {
	# read required data
	LINES_TO_READ <- 2* 24 * 60 # no of minutes in 2 days
	LINES_TO_SKIP <- 66637 # derived from grep("1/2/2007", readLines("household_power_consumption.txt"))[1]-1

	setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y"))
	setAs("character","myTime", function(from)  as.numeric(as.POSIXct(from, format="%H:%M:%S")))

	data <- read.table("household_power_consumption.txt", header=FALSE, quote="", stringsAsFactors=FALSE,comment.char="",sep=";",skip=LINES_TO_SKIP,nrows=LINES_TO_READ,
	           colClasses=c("myDate","myTime","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),
	           col.names=c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3"))

	# use png graphic device
	png("plot1.png",480,480)

	# draw histogram
	hist(data$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")

	# close and output png
	dev.off()
}