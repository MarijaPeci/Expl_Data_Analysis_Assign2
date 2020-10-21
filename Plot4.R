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

# 4. Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?

## Find the codes for Coal-combustion in SCC df
coal_codes <- SCC$SCC[grep("Coal", SCC$EI.Sector)]

## filter the NEI dataset according to coal_codes
coal_data <- subset(NEI, SCC %in% coal_codes, 
                    select = c(year, Emissions, type))

# sum grouped entries by year and source type

coal_data_sums <- coal_data %>% 
        group_by(year) %>% 
        summarize(coal_emissions = sum(Emissions, na.rm = TRUE))

#Plot the graph
png(filename = "Plot4.png")
g1 <- ggplot(data = coal_data_sums, aes(x = year, 
                                   y = coal_emissions
                                   )
             ) 
g1 + geom_line(color = "darkblue") + 
        geom_point(color = "darkblue") +
        theme_bw() +
        labs(title = "Changes in the U.S. coal-combustion source emissions",
             x = "Year",
             y = "Emissions in thousands of tons") +
        scale_y_continuous(limits = c(0, 600000), labels = c("0", "200", "400", "600")) +
        theme(legend.position = "bottom")

dev.off()     
