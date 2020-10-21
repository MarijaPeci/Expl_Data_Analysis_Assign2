#Exploratory Data Analysis Assignment by Marija Peciulyte
# 2020 October 21

#Install needed packages

package_list = c("tidyverse", "ggplot2", "RColorBrewer")
new_packages <- package_list[!(package_list %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
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

# 3. Of the four types of sources indicated by the type (point, nonpoint, 
# onroad, nonroad) variable, which of these four sources have seen decreases 
# in emissions from 1999–2008 for Baltimore City? Which have seen increases in 
# emissions from 1999–2008? Use the ggplot2 plotting system to make a plot 
# answer this question.

## To answer this question, we have to create a dataset with three vars: emissions, source, year

#create a subset of Baltimore (fips = 24510) and sum grouped entries

sub_Baltimore <- subset(NEI, fips == "24510", select = c(year, Emissions, type))
Baltype_df <- sub_Baltimore %>% 
        group_by(year, type) %>% 
        summarize(source_emissions = sum(Emissions, na.rm = TRUE))

#Plot the graph
png(filename = "Plot3.png")
g <- ggplot(data = Baltype_df, aes(x = year, 
                                   y = source_emissions, 
                                   color = type)) 
g + geom_line(size = 1) + 
    geom_point() +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    labs(title = "Changes in Baltimore PM2.5 source emissions",
         x = "Year",
         y = "Emissions in tons") +
    theme(legend.position = "bottom")
    
dev.off()     
