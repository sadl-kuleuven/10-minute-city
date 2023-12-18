########################################################################################
############################### Calculation of isochrones ##############################
########################################################################################

#cpprouting method to calculate isochrones

#https://cran.r-project.org/web/packages/cppRouting/cppRouting.pdf
#https://github.com/vlarmet/cppRouting/blob/master/README.md#compute-isochrones
#https://r-spatial.org/r/2019/09/26/spatial-networks.html

#0# Clean workspace
rm(list=ls())

options(repos = list(CRAN="http://cran.rstudio.com/"))
options(digits = 15)

#installation of packages
if(!"remotes" %in% installed.packages()) {
  install.packages("remotes")
}

cran_pkgs = c(
  "cppRouting",
  "dplyr",
  "sf",
  "ggplot2",
  "concaveman",
  "ggmap",
  "microbenchmark",
  "reshape2",
  "sp",
  "rgdal"
)

library(cppRouting)
library(dplyr)
library(sf)
library(ggplot2)
library(concaveman)
library(ggmap)
library(tmap)
library(microbenchmark)
library(reshape2)
library(sp)
library(rgdal)

if(!"remotes" %in% installed.packages()) {
  install.packages("remotes")
}

cran_pkgs = c(
  "sp",
  "rgdal",
  "sf",
  "tidygraph",
  "igraph",
  "osmdata",
  "dplyr",
  "tibble",
  "ggplot2",
  "units",
  "tmap",
  "rgrass7",
  "link2GI",
  "nabor",
  "tidyverse"
)

remotes::install_cran(cran_pkgs)
library(sf)
library(tidygraph)
library(igraph)
library(dplyr)
library(tibble)
library(ggplot2)
library(units)
library(tmap)
library(osmdata)
library(rgrass7)
library(link2GI)
library(nabor)
#library(magritrr)

#preparation of the data

#selection1 <- c('walking')
selection1 <- c('pied')
#selection2 <- c('J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/InputData/Mobility/VILLO/villo.shp',
#               'J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/InputData/Mobility/CAMBIO/cambio.shp')
selection2 <- c("J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/Processing/1. DataPreparation/ALLData (tree)/Mobilité/Voiture-vélo partagé(e)/Station de Villo/villo.shp")
#selection3 <- c('J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/InputData/02_URBIS/ADMIN/RBC/UrbAdm_STREET_AXIS.shp')
selection3 <- c('J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/Processing/2. StreetMap/3. CleanedMap/OSM/Foot/Foot length/Foot length.shp')
selection4 <- c("J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/Output/Test Batch")
selection5 <- c('2')

#read urbis road network
urbis<-read_sf(file.path(selection3[1]))
#read locations of variables based on user parameters
#open all input files
for (i in 1:length(selection2)) {
  #a <- paste0(selection2[i])
  a <- sub(".*?/(.*?).shp.*", "\\1", selection2[i])
  assign(a, read_sf(file.path(selection2[i])))
}

#give each edge a unique index
edges <- urbis %>%
  mutate(edgeID = c(1:n()))

#create nodes at the start and end point of each edge
nodes <- edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L2) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))

#give each node a unique index
library(tidyverse)
nodes <- nodes %>%
  mutate(xy = paste(.$X, .$Y)) %>% 
  group_by(X, Y) %>% 
  mutate(nodeID =cur_group_id()) %>%
  #mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  select(-xy)

#combine the node indices with the edges
source_nodes <- nodes %>%
  filter(start_end == 'start') %>%
  pull(nodeID)

target_nodes <- nodes %>%
  filter(start_end == 'end') %>%
  pull(nodeID)

edges = edges %>%
  mutate(from = source_nodes, to = target_nodes)

#remove duplicate nodes
nodes <- nodes %>%
  distinct(nodeID, .keep_all = TRUE) %>%
  select(-c(edgeID, start_end)) %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs(st_crs(edges))

#change column names
roads <- edges[c('from','to','LENGTH')]
roads <- data.frame(roads)
roads <- roads[c('from','to','LENGTH')]
colnames(roads) <- c('from','to','weight')

coordinates <- nodes %>%
  st_coordinates() %>%
  as_tibble()
nodeID <- nodes[c('nodeID')]
coord <- cbind(coordinates,nodeID)
coord <- data.frame(coord)
coord <- coord[c('nodeID','X','Y')]
colnames(coord)<- c('ID','X','Y')

graph<-makegraph(roads,directed = F,coords = coord)

#count 80 secondes pre and post traject for bike
#walking or cycling 10 minutes
#if(selection1[1]=="walking") {
#    if (selection5[1]=="10") {
#        lim <- 716.67
#    }  else {
#        lim <- 1433.33
#    }
#} else if  (selection1[1]=="cycling") {
#    if (selection5[1]=="10"){
#        lim <- 1877.78
#    } else {
#        lim <- 4044.44
#    }
#}

minutes <- as.data.frame(selection5[1])
minutes[1] <- as.numeric(minutes[1])
#walking or cycling 10 minutes
if(selection1[1]=="walking") {
  lim <- (4.3*1000/60*minutes[1])
} else if (selection1[1]=="cycling") {
  lim <- (13*1000/3600*((minutes[1]*60)-80))
} 

lim <- as.numeric(lim)
lim <- as.data.frame(lim)
#selection5[1] <- as.character(selection5[1])
#selection5 <- as.data.frame(selection5[1])  

#ifelse (selection1[1]=="walking" && selection5[1]=="20"){
#  lim <- 1433.33
# }
#ifelse (selection1[1]=="cycling" && selection5[1]=="20"){
#  lim <- 4044.44
# }
library(nabor)
for (i in 1:length(selection2)) {
  location <- get(sub(".*?/(.*?).shp.*", "\\1", selection2[i]))
  #nearest node to each variable location (e.g. villo station)
  coords_all <- st_coordinates(location)
  #coords_all <- location %>%
  #  st_coordinates() %>%
  #  matrix(ncol = 2)
  coords <- nodes %>%
    st_coordinates()
  node_index <- knn(data = coords, query = coords_all, k = 1)
  node <- nodes[node_index$nn.idx, ]
  
  #Compute isochrones for 10 minutes walking at 4,3 km/h (716,67 meters)
  iso<-get_isochrone(graph,from = node$nodeID,lim = c(lim))
  #Convert nodes to concave polygons with concaveman package
  coord$ID <- as.character(coord$ID)
  poly<-lapply(iso,function(t){
    t<-data.frame(noeuds=t,stringsAsFactors = F)
    t<-left_join(t,coord,by=c("noeuds"="ID"))
    return(concaveman(summarise(st_as_sf(t,coords=c("X","Y"),crs=31370))))
  })
  
  poly <- st_zm(poly)
  poly<-do.call(rbind,poly)
  poly$time<-as.factor(names(iso))
  poly$cost.level <- 10
  poly$modus <- selection1[1]
  
  #delete geometries which are not correct due to errors in the road network 
  invalid <- st_coordinates(poly$polygons)
  invalid <- as.data.frame(invalid)
  invalid$L2 <- as.character(invalid$L2)
  invalid <- count(invalid, invalid$L2)
  invalid2 <- invalid[invalid$n<4, ]
  poly$ID <- row.names(poly)
  poly <- poly[!(poly$ID %in% invalid2$`invalid$L2`),]
  poly <- poly %>% select(-ID)
  
  #convert to shapefile
  shapefile<- as(st_geometry(poly), "Spatial")
  # Extract polygon ID's
  pid <- sapply(slot(shapefile, "polygons"), function(x) slot(x, "ID"))
  # Create dataframe with correct rownames
  p.df <- data.frame( ID=1:length(shapefile), row.names = pid)  
  # Try coersion again and check class
  variable <- SpatialPolygonsDataFrame(shapefile, p.df)
  variable$modus <- selection1[1]
  #variable$cost.level <- selection5[1]
  a <- sub(".*?/(.*?).shp.*", "\\1", selection2[i])
  b <- sub(".*?/(.*?).shp.*", "\\1", selection3[1])
  #write to shapefile
  #ADAPT!
  writeOGR(variable, layer = paste0(a,"_",selection1[1],"_",selection5[1],"_", b), file.path(selection4[1]), driver="ESRI Shapefile", overwrite_layer = T)
}

time <- selection5[1]
modus <- selection1[1]
fmeOutput<-data.frame(modus, lim, time)

