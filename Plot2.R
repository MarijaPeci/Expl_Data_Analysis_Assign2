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

# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a 
# plot answering this question.

#create a subset of Baltimore(fips = 24510)

sub_Baltimore <- subset(NEI, fips == "24510", select = c(year, Emissions))

#calculate the total emission for each year and store into df
Balt_df <- sub_Baltimore %>% 
        group_by(year) %>%
        summarize(emission = sum(Emissions, na.rm = TRUE))

#Plot the graph
png(filename = "Plot2.png")
par(mfrow = c(1,1), mar = c(4,4,2,1))
plot(x = Balt_df$year, y = Balt_df$emission, 
     type = "l",
     main = "PM2.5 emission change throughout years in Baltimore",
     xlab = "Year",
     ylab = "Emmissions in tons",
     lwd = 2,
     xlim = c(1998, 2009)
)

dev.off()     
