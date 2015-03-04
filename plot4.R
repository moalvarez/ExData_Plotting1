## This function reads the "household_power_consumption" data file and creates the
## fourth plot for Course Project 1 of the Exploratory Data Analysis Course
## Author: Miguel Alvarez

plot4 <- function(){
      ##  Load the data file into a dataframe
      powerFrame <- read.csv("household_power_consumption.txt", 
                             na.strings = "?", sep = ";")
      
      ##  Convert Time field into a POSIXlt time
      powerFrame <- transform(powerFrame, 
                              Time = strptime(paste(Date, Time, sep = " "), 
                                              format = "%d/%m/%Y %H:%M:%S"))
      
      ##  Create a subset frame for the two day period to be plotted
      subsetFrame <- na.omit(powerFrame[((powerFrame$Time >= as.POSIXlt("2007-02-01")) & 
                                               (powerFrame$Time < as.POSIXlt("2007-02-03"))),],50)
      
      ##  Open the png file device
      png(file = "plot4.png")
      
      ##  Set plotting to a 2 x 2 matrix of plots on page
      par(mfrow = c(2,2))
      
      #   Plot Global Active Power in first slot (top left) 
      plot( mySubFrame$Time, mySubFrame$Global_active_power, xlab = "", 
            ylab = "Global Active Power", type = "l")
      
      #   Plot Voltage second slot (top right)
      plot( mySubFrame$Time, mySubFrame$Voltage, xlab = "datetime", 
                  ylab = "Voltage", type = "l")
      
      #   Plot Energy Sub Metering in third slot (bottom left)
      with(mySubFrame, plot(Time, Sub_metering_1, xlab = "", 
                            ylab = "Energy sub metering", type = "l"))
      with(mySubFrame, points(Time, Sub_metering_2, type = "l", col = "red"))
      with(mySubFrame, points(Time, Sub_metering_3, type = "l", col = "blue"))
      legend("topright", lwd = 1, bty = "n", col = c("black", "red", "blue"), 
             legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

      #   Plot Global Reactive Power in fourth slot (bottom right)
      plot( mySubFrame$Time, mySubFrame$Global_reactive_power, xlab = "datetime", 
                  ylab = "Global_reactive_power", type = "l")
      
      ##  Close the device
      dev.off()
}