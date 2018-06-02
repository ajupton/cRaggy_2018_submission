library(tidyverse)
library(igraph)
library(ggraph)
library(colorRamps)
library(extrafont)

# Import data, forked from https://github.com/smithjd/cRaggy-example
url <- "https://s3.amazonaws.com/biketown-tripdata-public/BiketownPublicTripData201804.zip"

download.file(url, dest = "dataset.zip", mode = "wb")

unzip("dataset.zip", exdir = "./")

public_trip_data <- list.files(
  path = "PublicTripData",
  pattern = "*.csv",
  full.names = T ) %>%
  #map_df(~ read_csv(., col_types = cols(.default = "c")))
  map_df(~ read_csv(., col_types = "iccddDTcddDTcicdTcl"))

glimpse(public_trip_data)

# How many missing values?
# colSums(sapply(public_trip_data, is.na))

# Begin constructing a Biketown Network to explore network topology
# What is the overall "flow" of Biketown bikes in the Portland bike hub network?

# Filter to only start/end hub and geo coords to extract node info - 367,024 bike trips
hub_data <- public_trip_data %>%
  select(StartHub, StartLatitude, StartLongitude, EndHub, EndLatitude, EndLongitude) %>%
  filter(StartHub != "<NA>" & EndHub != "<NA>") %>% # drop trips that didn't start/end at a hub
  group_by(StartHub, EndHub) 

# Pull hub data only to make a weighted, directed edgelist where weight is number of trips between nodes
# Looks like there are 13,870 unique hub to hub combinations where at least one trip was taken
# I'll retain pop-up or disabled hubs since they connect to permanent hubs
el_hubs <- hub_data %>%
  select(StartHub, EndHub) %>%
  filter(StartHub != EndHub) %>% # get rid of recursive edges
  group_by(StartHub, EndHub) %>%
  summarise(weight = n()) 
  # %>% arrange(desc(weight)) # strongest edge is 695 trips

# Convert nodes to factor and weight to numeric for igraph
el_hubs$StartHub <- as.factor(el_hubs$StartHub)
el_hubs$EndHub <- as.factor(el_hubs$EndHub)
el_hubs$weight <- as.numeric(el_hubs$weight)

# igraph takes a matrix as input, we'll make one from the edgelist
el_hubs_m <- as.matrix(el_hubs)

# Create a table for each hub's geographic coordinates
hub_lat_longs <- hub_data %>%
  group_by(StartHub) %>%
  select(StartHub, StartLatitude, StartLongitude) %>%
  distinct(StartHub, .keep_all = TRUE) 

# Create data frame of hub positions amenable to igraph's layout specifications
hub_positions <- data.frame(nodes = hub_lat_longs$StartHub, 
                            x = hub_lat_longs$StartLongitude, 
                            y = hub_lat_longs$StartLatitude)

# Define a node table, edge list matrix, and edge weight vector
node_list <- data.frame(hub_positions)
edge_list <- el_hubs_m[, 1:2]
edge_list_weight <- full_join(as.data.frame(edge_list), el_hubs) # join numeric weight to edge list

# Create igraph object and set graphical parameters
g <- graph_from_data_frame(vertices = node_list, d = edge_list, directed = TRUE)
V(g)$label <- NA # drop node labels - too messy otherwise
V(g)$size <- log(strength(g, mode = "all"))*.01 # size the nodes based on weighted degree, but small
V(g)$frame.color <- NA # drop the node border 
V(g)$color <- "orangered" # color nodes the closest color to Nike orange
E(g)$arrow.mode <- 0 # drop arrows from directed edges
E(g)$weight <- edge_list_weight$weight # assign weight to graph edges
E(g)$width <- log(E(g)$weight)*.1 # subtely using edge width based on weight

# Determine zoomed in coordinates for xlim and ylim
min(hub_positions$x)
max(hub_positions$x)
min(hub_positions$y)
max(hub_positions$y)

# Color function to highlight edge strength
colfunc <- colorRampPalette(c("white", "lightcyan", "skyblue1", "lightblue1", "lightskyblue", "blue", "blue3", "dark blue"), alpha = TRUE)
#table(colfunc(length(E(g)$weight))) # check out color assignments

# Set plotting parameters: background color to a dark gray and set font to a sans-serif family
par(bg = "grey25", family = "Franklin Gothic Medium") 

# Time to plot! 
plot.igraph(g, 
              rescale = FALSE,  # rescale would normalize geo coords so needs to be turned off
              asp = FALSE, # asp is the aspect ratio, also messes things up if not off
              layout = as.matrix(hub_positions[2:3]), # manually set layout based on lon and lat
              ylim = c(45.49, 45.57), # ylims based on max/min above
              xlim = c(-122.705, -122.62), #xlims based on max/min above
              edge.color = colfunc(length(E(g)$weight))) # applying color function here
title(main = "BIKETOWN Network", col.main = "ghostwhite",
      sub = "code @ https://bit.ly/2sqwmZT", col.sub = "ghostwhite", 
      cex.sub = 1, cex.main = 2)

# Export plot using R graphics editor as an SVG 1500 x 1125 pixels
# dev.off()

