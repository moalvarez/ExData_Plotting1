## This function reads the "household_power_consumption" data file and creates the
## second plot for Course Project 1 of the Exploratory Data Analysis Course
## Author: Miguel Alvarez

plot2 <- function(){
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
      png(file = "plot2.png")
      
      ##  Make the plot
      plot( subsetFrame$Time, subsetFrame$Global_active_power, xlab = "", 
                        ylab = "Global Active Power (kilowatts)", type = "l")
      
      ##  Close the device
      dev.off()
}