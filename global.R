# Load required libraries
library(shiny)
library(data.table)
library(DT)
library(shinythemes)
library(plotly)
library(viridis)
library(png)
library(ggplot2)

library(grid)


# Read a subset of the train data
library('plotly')
library('htmlwidgets')
library('RCurl')

image_file <- "/Users/charan/Downloads/1722_S3.png"
txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")

fb <- (fread("/Users/charan/Applications/Analytics/coordinate_generator/csvfile4.csv", integer64 = "character", showProgress = FALSE))
coords <- fread("/Users/charan/Applications/Analytics/coordinate_generator/csvfile_coords.csv", integer64 = "character", showProgress = FALSE)
coords[,time := floor(as.numeric(as.POSIXct(time,format="%Y-%m-%dT%H:%M:%OS"))/60)]
coords$time = floor((coords$time %%(60*24)))
coords<-unique(coords)
print(length(unique(coords$macid)))
#coords$y=nrow(img)-coords$y
img <- readPNG(image_file)
coords$distance=coords$distance*nrow(img)/max(coords[["distance"]])


#fb %>% filter(x >1, x <1.25, y >2.5, y < 2.75) -> fb

# Read a subset of the train data
#dataFn <- "trainSubset.rds"
dataSubset<-setDT(fb)
#dataSubset <- readRDS(dataFn)
# Load required libraries
# Extract the unique macids for subsetting in the app graphs
# macIds <- unique(dataSubset$place_id)
macIds <- table(dataSubset$macid)
errors <- unique(dataSubset$error)
macIds <- names(sort(macIds, decreasing = TRUE))

# List of analysis variables
analysisVars <- c("accuracy", "x", "y", "time", "hour", "day")
analysisVarsComp <- c(analysisVars, "logAccuracy")

# Add the hour and day of week to the raw 

dataSubset[,time1 := floor(as.numeric(as.POSIXct(time,format="%Y-%m-%dT%H:%M:%OS"))/60)]
dataSubset[,hour := floor((time1 %% (60*24))/60)]
dataSubset[,day := floor((time1 %% (60*24*7)) / (60*24))]
dataSubset[,time := floor((time1 %%(60*24)))]
# Add the log transformed accuracy
dataSubset[,logAccuracy := log(accuracy)]

# Add the accuracy groups
dataSubset[,accuracyGroup := cut(accuracy, breaks=c(0,45,85,1e5))]


dataSubset<-unique(dataSubset)
print(length(unique(dataSubset$macid)))
