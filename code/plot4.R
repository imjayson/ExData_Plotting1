library(data.table)
plot4 <- function() {
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
	png("plot4.png",480,480)

	# set 2x2
	par(mfcol = c(2,2))

	# top left - reuse plot2
	# empty chart
	plot(minsFromStart(data),data$Global_active_power, type="n",main="", ylab="Global Active Power (kilowatts)", xlab="",xaxt="n")

	# draw custom axis
	axis(1, at=c(0,1440,2880), labels=c("Thu","Fri","Sat"))

	# draw histogram
	lines(minsFromStart(data),data$Global_active_power)


	# bottom left - reuse plot3 - remove border

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
	legend("topright", pch=NA, lty=1, col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), box.lwd=0)


	# top right
	# empty chart
	plot(minsFromStart(data),data$Voltage,ylim=c(234,246),type="n",main="",xlab="datetime", ylab="Voltage", xaxt="n",yaxt="n")

	# draw custom axis
	axis(1, at=c(0,1440,2880), labels=c("Thu","Fri","Sat"))
	axis(2, at=seq(234, 246, by = 4)) 

	# draw lines
	lines(minsFromStart(data),data$Voltage)


	# bottom right
	# empty chart
	plot(minsFromStart(data),data$Global_reactive_power,ylim=c(0,0.5),type="n",main="",xlab="datetime", ylab="Global_reactive_power",xaxt="n",yaxt="n")

	# draw custom axis
	axis(1, at=c(0,1440,2880), labels=c("Thu","Fri","Sat"))
	axis(2, at=seq(0, .5, by = .1)) 

	# draw lines
	lines(minsFromStart(data),data$Global_reactive_power)



	# close and output png
	dev.off()

}