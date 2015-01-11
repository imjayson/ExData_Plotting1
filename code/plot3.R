library(data.table)
plot3 <- function() {
	# read required data
	LINES_TO_READ <- 2* 24 * 60 # no of minutes in 2 days
	LINES_TO_SKIP <- 66637 # derived from grep("1/2/2007", readLines("household_power_consumption.txt"))[1]-1

	setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y"))
	setAs("character","myTime", function(from)  { 
	  currTime <- strptime(from, format="%H:%M:%S")
	  (as.numeric(strftime(currTime,"%H")) * 60) + (as.numeric(strftime(currTime,"%M"))) 
	})


	data <- read.table("household_power_consumption.txt", header=FALSE, quote="", stringsAsFactors=FALSE,comment.char="",sep=";",skip=LINES_TO_SKIP,nrows=LINES_TO_READ,
	           colClasses=c("myDate","myTime","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),
	           col.names=c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3"))

	minsFromStart <-function(input) {
	  as.numeric(as.POSIXlt(input$Date))/60 - 19504800 + input$Time
	}

	# use png graphic device
	png("plot3.png",480,480)

	# empty chart
	plot(minsFromStart(data),data$Sub_metering_1,ylim=range(c(data$Sub_metering_1,data$Sub_metering_2,data$Sub_metering_3)),type="n",main="", ylab="Energy sub metering", xlab="",xaxt="n",yaxt="n")

	# draw custom axis
	axis(1, at=c(0,1440,2880), labels=c("Thu","Fri","Sat"))
	axis(2, at=seq(0, 30, by = 10)) 

	# draw lines
	lines(minsFromStart(data),data$Sub_metering_1)
	lines(minsFromStart(data),data$Sub_metering_2, col="red")
	lines(minsFromStart(data),data$Sub_metering_3, col="blue")

	# draw legend
	legend("topright", pch=NA, lty=1, col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

	# close and output png
	dev.off()

}