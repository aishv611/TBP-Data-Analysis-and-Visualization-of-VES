# Importing required libraries
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("ggplot2)

# Loading libraries into the file
library(readxl)
library(dplyr)
library(ggplot2)

# Getting and Setting the directory
getwd()

setwd(choose.dir())
getwd()

# Importing excel file using readxl library
ExptData <- read_xlsx(choose.files())
is.na.data.frame(ExptData)# Checking for missing or blank values from the data imported
View(ExptData) # Viewing the imported data as a whole

# Checking the type and dimensionalities
typeof(ExptData)
ncol(ExptData)
dim.data.frame(ExptData)
is.data.frame(ExptData)

# Understanding the structure and summary of data
str(ExptData)
summary(ExptData)

# Assigning column names
names(ExptData) <- c("Sno", "current_ed", "potential_ed", "vr_direct", "vr_reverse", "cr_direct", "cr_reverse")
names(ExptData)

# Checking the first and last 5 rows
head(ExptData, n = 5)
tail(ExptData, n = 5)

# Performing calculations required on the columns
# Mutating and adding additional columns to the existing dataframe
ExptData <- mutate(ExptData, Avg_pd = (vr_direct + vr_reverse)/2, Avg_curr = (cr_direct + cr_reverse)/2)
ExptData <- mutate(ExptData, R = (Avg_pd/Avg_curr))
ExptData <- mutate(ExptData, K = ((pi * ((current_ed ** 2) - (potential_ed ** 2)))/ (potential_ed * 2)))
ExptData <- mutate(ExptData, Apparent_res = K*R) 

# Rounding the table values to 3 decimal places
ExptData <- round(ExptData, 3)

# Viewing the table
View(ExptData)

# Maximum and minimum values of Apparent Resistivity
max(ExptData$Apparent_res)
min_val <- min(ExptData$Apparent_res)
min_val

# Depth of the water table
depth <- ExptData[ExptData$Apparent_res == min_val,"current_ed"]
names(depth) <- NULL
depth <- as.integer(depth)
typeof(depth)
depth

# Plotting the graph between "Distance" and "Apparent resistivity"
logPlot <- ggplot(data = ExptData, aes(x = current_ed, y = Apparent_res)) + geom_line() + geom_point(size = 3)

# Adding labels and plot title
logPlot <- logPlot + xlab("Distance") + 
  ylab("Apparent Resistivity") +
  ggtitle("L/2 vs. Apparent Resistivity")

# Setting log-scale
logPlot <- logPlot + scale_x_continuous(expand = c(0,0), trans = "log10", limits = c(1,10000))+
  scale_y_continuous(expand = c(0,0), trans = "log10", limits = c(1,1000))

# Adding aesthetic layer
# Styling the labels and title
logPlot <- logPlot + theme(axis.title.x = element_text(colour = "red", size = 20, family = "serif"),
        axis.title.y = element_text(colour = "Dark blue", size = 20, family = "serif"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(colour = "Dark Green", size = 27, family = "serif", hjust = 0.5))

# Setting background and grid for the plot
logPlot <- logPlot + theme(plot.background = element_rect(fill = "grey"))
logPlot <- logPlot + theme(panel.grid.major = element_line(colour = "black"))

# Viewing the end plot
logPlot <- logPlot + geom_hline(yintercept = min(ExptData$Apparent_res), colour = "red")
logPlot <- logPlot + geom_vline(xintercept = as.integer(depth), colour = "blue")
logPlot

# Depth of the water table from the graph
print(paste("The depth of the water table is", depth,"mts"))

