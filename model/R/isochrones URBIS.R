########################################################################################
############################### Calculation of isochrones ##############################
########################################################################################

#cpprouting method to calculate isochrones

#More information: 
#https://cran.r-project.org/web/packages/cppRouting/cppRouting.pdf
#https://github.com/vlarmet/cppRouting/blob/master/README.md#compute-isochrones
#https://r-spatial.org/r/2019/09/26/spatial-networks.html

#0# Clean workspace
rm(list=ls())

#set the number of digits 
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
  "rgdal", 
  "tidygraph",
  "igraph",
  "osmdata",
  "tibble",
  "units",
  "tmap",
  "rgrass7",
  "link2GI",
  "nabor",
  "tidyverse",
  "maptools",
  "lwgeom",
  "data.table",
  "magritrr"
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
library(tidygraph)
library(igraph)
library(tibble)
library(units)
library(tmap)
library(osmdata)
library(rgrass7)
library(link2GI)
library(nabor)
library(data.table)
library(magrittr)

#preparation of the data based on user parameters

selection1 <- c('pied')
selection2 <- c("J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/Futur/Processing/2024/input/Résidence seniors.shp")
selection3 <- c("J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/Processing/2. StreetMap/3. CleanedMap/Urbis cleaned/Urbis_cleaned.shp")
selection4 <- c("J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/Output/Test Batch")
selection5 <- c('20')
selection6 <- c('vivre ensemble')
selection7 <- c('senior')
selection8 <- c('2024')

#read shapefile of urbis road network 
#based on file path provided as user parameter for 'reseau_routier'
urbis<-read_sf(file.path(selection3[1]))


#Loop for all input variables. For each input variable an isochrone map is created. 
for (i in 1:length(selection2)) {
  
  #read shapefile of variable based on file path provided as user parameter 'variable'
  a <- sub(".*?/(.*?).shp.*", "\\1", selection2[i])
  assign(a, read_sf(file.path(selection2[i])))
  location <- get(sub(".*?/(.*?).shp.*", "\\1", selection2[i]))
  #possibility to set CRS but sometimes this can give errors
  #st_crs(urbis) = CRS("+init=epsg:31370")
  #st_crs(location) = CRS("+init=epsg:31370")
  
  #project points onto the road network using function snappointsToLines
  library("maptools") #load package
  location_sp <- as(location, 'Spatial') #convert to spatial objects
  location_sp <- SpatialPoints(location_sp) #convert to spatial points
  urbis_sp <- as(urbis, 'Spatial') #convert to spatial objects
  test <- snapPointsToLines(location_sp,urbis_sp) #project points (=variables) to lines (=road network)
  test2 <- as.data.frame(test) #convert back to dataframe
  
  #split road network based on the projected points. 
  #By doing this extra nodes will be created in the road network. 
  #this is a very time consuming step 
  library("lwgeom")
  test3 <- st_as_sf(test2,coords = c("X", "Y"))
  on_line_all = st_cast(test3, "POINT")
  buf_all <- st_combine(st_buffer(on_line_all,0.0000000001))
  parts_all = st_collection_extract(lwgeom::st_split(urbis$geometry, buf_all),"LINESTRING")
  
  parts_all = st_as_sf(
    data.frame(
      id = 1:length(parts_all),
      geometry = parts_all
    )
  )
  
  #calculate length
  parts_all$length <- st_length(parts_all)
  
  #change the urbis dataset with the newly created road network that contains additional edges
  urbis <- parts_all
  
  #give each edge of the road network a unique index
  edges <- urbis %>%
    mutate(edgeID = c(1:n()))
  
  #create nodes at the start and end point of each edge
  nodes <- edges %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(edgeID = L1) %>%
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
  roads <- edges[c('from','to','length')]
  roads <- data.frame(roads)
  roads <- roads[c('from','to','length')]
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
  
  # set the maximal distance based on the time threshold and the mode of transport
  # provided by the user parameters ('minute' and 'mode')
  temps <- as.data.frame(selection5[1])
  temps[1] <- as.numeric(temps[1])
  #distance is different depending on the mode of transport (walking or cycling)
  #velocity for walking: 4.3 km/h
  #velocity for cycling: 13 km/h + 80 seconds pre/post journey
  if(selection1[1]=="pied") {
    lim <- (4.3*1000/60*temps[1]) #4.3 km/h
  } else if (selection1[1]=="vélo") {
    lim <- (13*1000/3600*((temps[1]*60)-80)) #13 km/h + 80 seconds pre/post journey
  } 
  
  lim <- as.numeric(lim)
  lim <- as.data.frame(lim)
  
  #select the node ID's that match with the location of the variable 
  #(node from a location of a variable = point projected to the road network)
  y <- coord[order(coord$ID),]
  row.names(y) <- y$ID
  node <- data.frame()
  node_tot <- data.frame()
  for (j in 1:dim(test2)[1]) {
  node_loop <- node_tot
  node <- subset(y$ID, y$Y %like% test2[j,]$Y)
  node_tot <- rbind(node_loop, node)
  }
  node_isochrones <- node_tot[,1]
  node_isochrones <- data.frame(node_isochrones)
  colnames(node_isochrones) <- "nodeID"
  
  #Compute isochrones for X minutes walking at 4,3 km/h or cycling at 13 km/h
  #distance/time limit was previously calculated (line 185-196)
  #time consuming step
  iso<-get_isochrone(graph,from = node_isochrones$nodeID,lim = c(lim))
  #Convert nodes to concave polygons with concaveman package
  coord$ID <- as.character(coord$ID)
  poly<-lapply(iso,function(t){
    t<-data.frame(noeuds=t,stringsAsFactors = F)
    t<-left_join(t,coord,by=c("noeuds"="ID"))
    return(concaveman(summarise(st_as_sf(t,coords=c("X","Y"),crs=31370))))
  })
  
  #add attributes to the isochrone polygons
  poly <- st_zm(poly)
  poly<-do.call(rbind,poly)
  poly$time<-as.factor(names(iso))
  poly$minutes <- selection5[1]
  poly$mode <- selection1[1]
  poly$theme <- selection6[1]
  poly$soustheme <- selection7[1]
  poly$scenario <- selection8[1]
  
  #delete geometries which are not correct due to errors in the road network 
  invalid <- st_coordinates(poly$polygons)
  invalid <- as.data.frame(invalid)
  invalid$L2 <- as.character(invalid$L2)
  invalid <- count(invalid, invalid$L2)
  invalid2 <- invalid[invalid$n<3, ]
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
  # add user parameters as attributes - will later be used when creating heatmaps 
  variable$mode <- selection1[1]
  variable$theme <- selection6[1]
  variable$soustheme <- selection7[1]
  variable$minutes <- selection5[1]
  variable$scenario <- selection8[1]
  a <- sub(".*?/(.*?).shp.*", "\\1", selection2[i])
  b <- sub(".*?/(.*?).shp.*", "\\1", selection3[1])
  #write to shapefile 
  #destination folder specified as user parameter
  writeOGR(variable, layer = paste0(a,"_",selection1[1],"_",selection5[1],"_", b), file.path(selection4[1]), driver="ESRI Shapefile", overwrite_layer = T)
}

#output FME (to check if user parameters are correct)
time <- selection5[1]
modus <- selection1[1]
theme <- selection6[1]
soustheme <- selection7[1]
scenario <- selection8[1]
fmeOutput<-data.frame(time, modus, lim, theme, soustheme, scenario)

