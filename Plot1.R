#Exploratory Data Analysis Assignment by Marija Peciulyte
# 2020 October 21

#Install needed packages

package_list = "tidyverse"
new_packages <- package_list[!(package_list %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)

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

# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission 
# from all sources for each of the years 1999, 2002, 2005, and 2008.


#calculate the total emission for each year and store into df

total_df <- NEI %>% 
        group_by(year) %>% 
        summarize(emissions = sum(Emissions, na.rm = TRUE))

#Plot the graph
png(filename = "Plot1.png")
par(mfrow = c(1,1), mar = c(4,4,2,1))
plot(x = total_df$year, y = total_df$emissions, 
     type = "l",
     main = "PM2.5 emission change throughout years in the U.S.",
     xlab = "Year",
     ylab = "Emmissions (millions of tons)",
     lwd = 2,
     xlim = c(1998, 2009),
     ylim = c(3*10^6, 8*10^6),
     yaxt = "n"
)
y_labs <- as.character(seq(3, 8, 0.5))
axis(side = 2, at = seq(3*10^6,8*10^6, 0.5*10^6), labels = y_labs)
dev.off()     
