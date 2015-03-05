## This function reads the "household_power_consumption" data file and creates the
## third plot for Course Project 1 of the Exploratory Data Analysis Course
## Author: Miguel Alvarez

plot3 <- function(){
      ##  Load the data file into a dataframe
      powerFrame <- read.csv("household_power_consumption.txt", 
                             na.strings = "?", sep = ";")
      
      ##  Convert Time field into a POSIXlt time
      powerFrame <- transform(powerFrame, 
                              Time = strptime(paste(Date, Time, sep = " "), 
                                              format = "%d/%m/%Y %H:%M:%S"))

      ##  Create a subset frame for the two day period to be plotted
      subsetFrame <- na.omit(powerFrame[((powerFrame$Time >= as.POSIXlt("2007-02-01")) & 
                                    (powerFrame$Time < as.POSIXlt("2007-02-03"))),])

      ##  Open the png file device
      png(file = "plot3.png")
      
      ##  Make the plot
      with(subsetFrame, plot(Time, Sub_metering_1, xlab = "", 
                            ylab = "Energy sub metering", type = "l"))
      with(subsetFrame, points(Time, Sub_metering_2, type = "l", col = "red"))
      with(subsetFrame, points(Time, Sub_metering_3, type = "l", col = "blue"))
      legend("topright", lwd = 1, col = c("black", "red", "blue"), 
                  legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
      
      ##  Close the device
      dev.off()
}