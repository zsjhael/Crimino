library(shiny)
library(leaflet)
library(rsconnect)
library(dplyr)
library(data.table)
library(lubridate)
library(shinythemes)
library(igraph)
library(ggplot2)
library(ggthemes)
library(threejs)
library(bslib)

#rsconnect::setAccountInfo(name='crimino', token='189602F95F6264E8E5E9AF9DFD5DE1D8', secret='Usyjx5HUgHab5yHNx5zJkeDiyiEvB4ti8TuLyTiY')
#deployApp(appName = "Crimino")

#https://www.bootstrap-live-customizer.com/

load(file = "Crimes.RData")

sample.crimes <- sample.int(nrow(dt.crimes), size = 20000)
dt.crimes.map <- dt.crimes[sample.crimes]

# Create lists with 
l.Location.Description <- unique(dt.crimes.map$Location.Description)
l.Type.Highlight <- c(unique(dt.crimes.map$Type.Highlight))

# Save palette for crime types color coding
l.colors <- c("red", "navy", "green", "yellow", "turquoise", "tan", "orange", 
              "purple", "darkslategrey", "steelblue", "pink")
pal <- colorFactor(l.colors, domain = dt.crimes.map$Type.Highlight)

# T


