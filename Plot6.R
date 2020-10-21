#Exploratory Data Analysis Assignment by Marija Peciulyte
# 2020 October 21

#Install needed packages

package_list = c("tidyverse", "ggplot2", "gridExtra")
new_packages <- package_list[!(package_list %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(ggplot2)
library(gridExtra)
#Upload the data files to R
## download the zip file
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
file_n = "EPA_data.zip"

if(!file.exists(file_n)){
        download.file(url, destfile = file_n)
}

## extract zip and load the files into R
if(!exists(c("NEI", "SCC"), where = parent.frame())) {
        unzip(zipfile = file_n)
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
}

# 6. Compare emissions from motor vehicle sources in Baltimore City with 
# emissions from motor vehicle sources in Los Angeles County, California 
# (fips == "06037"). Which city has seen greater changes over time in motor 
# vehicle emissions?

## Comment: because it asks relative changes between two cities, it is best to 
## see how the change occurred comparing it to the base measure for each city.
## Hence, I will use percentage movement which notes the increase or decrease in 
## emissions in each city from the base year (1999).

## Find the codes for motor vehicles in SCC df

##the condition 
a <- grep("Highway Vehicles", SCC$SCC.Level.Two)

mot_codes <- SCC$SCC[a]

## filter the NEI dataset according to motor vehicle codes in Baltimore 
motor_Balt <- subset(NEI, fips == "24510" & SCC %in% mot_codes, 
                     select = c(SCC, year, Emissions, type))

# sum grouped entries by year
motor_Balt_sums <- motor_Balt %>% 
        group_by(year) %>% 
        summarize(motor_emissions = sum(Emissions, na.rm = TRUE))


## filter the NEI dataset according to motor vehicle codes in LA 
motor_LA <- subset(NEI, fips == "06037" & SCC %in% mot_codes, 
                     select = c(SCC, year, Emissions, type))

# sum grouped entries by year
motor_LA_sums <- motor_LA %>% 
        group_by(year) %>% 
        summarize(motor_emissions = sum(Emissions, na.rm = TRUE))

#Create a percentage change variable in Baltimore and LA
## Calculates the change from the base year
motor_Balt_sums <- mutate(motor_Balt_sums, 
                          p_change = (motor_emissions-motor_emissions[1])/motor_emissions[1])

motor_LA_sums <- mutate(motor_LA_sums,
                        p_change = (motor_emissions-motor_emissions[1])/motor_emissions[1])
#Plot the graph
png(filename = "Plot6.png", width = 600)

gB <- ggplot(data = motor_Balt_sums, aes(x = year, y = p_change)) +
        geom_line() + 
        geom_point() +
        theme_bw() +
        labs(title = "Baltimore motor vehicle emissions",
             x = "Year",
             y = "Change from the base year (%)") +
        scale_y_continuous(limits = c(-1, 1), 
                           breaks = seq(-1, 1, by = 0.2),
                           labels = seq(-100, 100, by = 20)) +
        theme(legend.position = "none",
              panel.grid.major.y = element_line(color = "gray"))


gLA <- ggplot(data = motor_LA_sums, aes(x = year, y = p_change)) +
        geom_line() + 
        geom_point() +
        theme_bw() +
        labs(title = "Los Angeles motor vehicle emissions",
             x = "Year",
             y = "Change from the base year (%)") +
        scale_y_continuous(limits = c(-1, 1), 
                           breaks = seq(-1, 1, by = 0.2),
                           labels = seq(-100, 100, by = 20)) +
        theme(legend.position = "none",
              panel.grid.major.y = element_line(color = "gray"))

grid.arrange(gB, gLA, ncol = 2)


dev.off()     
