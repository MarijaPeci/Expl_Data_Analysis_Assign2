#Exploratory Data Analysis Assignment by Marija Peciulyte
# 2020 October 21

#Install needed packages

package_list = c("tidyverse", "ggplot2")
new_packages <- package_list[!(package_list %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(ggplot2)
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

# 5. How have emissions from motor vehicle sources changed from 1999–2008 in 
# Baltimore City?

## Find the codes for motor in SCC df

##the condition 
a <- grep("Highway Vehicles", SCC$SCC.Level.Two)

mot_codes <- SCC$SCC[a]

## filter the NEI dataset according to motor vehicle codes and measures in Baltimore
motor_Balt <- subset(NEI, fips == "24510" & SCC %in% mot_codes, 
                    select = c(SCC, year, Emissions, type))

# sum grouped entries by year
motor_Balt_sums <- motor_Balt %>% 
        group_by(year) %>% 
        summarize(motor_emissions = sum(Emissions, na.rm = TRUE))

#Plot the graph
png(filename = "Plot5.png")
g2 <- ggplot(data = motor_Balt_sums, aes(x = year, 
                                        y = motor_emissions
))

g2 + geom_line(color = "darkblue") + 
        geom_point(color = "darkblue") +
        theme_bw() +
        labs(title = "Changes in Baltimore motor vehicle emissions",
             x = "Year",
             y = "Emissions in tons") +
        scale_y_continuous(limits = c(0, 400)) +
        theme(legend.position = "bottom")

dev.off()     
