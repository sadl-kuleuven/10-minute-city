#########################################################
##########     Public transport isochrones     ##########
#########################################################

getwd()
setwd("J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/OriginalInputData/Public transport travel time/STIB")

library(sf)
library(maptools)
library(tidyverse)
library(foreach)
library(chron)
library(tibbletime)
library(dplyr)
library(data.table)
library(cppRouting)
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
library(osmdata)
library(rgrass7)
library(link2GI)
library(nabor)

#Read data
stops <- read.table("stop_times.txt", header = T, sep = ",")
piscines <- st_read("J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/Processing/1. DataPreparation/ALLData (tree)/Vivre ensemble & Bien-être/Senior/Résidence/seniors.shp")
stib_bus <- st_read("J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/Processing/1. DataPreparation/ALLData (tree)/Mobilité/Transport public/STIB_Bus arrets/STIB_Bus.shp")
roads <- st_read("J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/Processing/2. StreetMap/3. CleanedMap/Urbis cleaned/Urbis_cleaned.shp")

#Prepare stop_times.txt data
arrival_time <- chron(times= stops$arrival_time)
times <- as.data.frame(arrival_time)
stops_subset <- subset(stops, select = c(trip_id, stop_id, stop_sequence))
stops_times <- cbind.data.frame(stops_subset, times)

#Add stop_times table to stib_bus shapefile
stib_bus_times <- merge(stib_bus, stops_times, by = "stop_id")

#Filter out afternoon trips
stib_bus_times_filter <- filter(stib_bus_times, arrival_time >= "14:00:00", arrival_time <= "19:00:00")

#Remove duplicate stop_id
stib_bus_times_unique <- stib_bus_times_filter[!duplicated(stib_bus_times_filter$stop_id),]

#Add ID number to piscines
piscines <- piscines %>% mutate(Id = row_number())


###################
##For all piscines
###################


m=2
datalist3 = list()
for(m in 1:nrow(piscines)){
  
  #Buffer distance around piscine
  l = 500 
  
  #Isolate the row as the origin point:
  row_interest <- filter(piscines, row_number()== m)
  
  Id <- row_interest$Id
  Id <- as.data.frame(Id)
  Name <- row_interest$NOM
  Name_piscine <- as.data.frame(Name)
  
  #Create the buffer:
  buffer <- row_interest %>% st_buffer(dist = l)
  
              
              #st_write(buffer, dsn ="J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/Processing/Isochrones_R", layer = "Piscine triton buffer", driver = "ESRI Shapefile", append = F, overwrite=T)
  
  
  #Extract the row numbers of the neighbors
  comps_idx <- st_intersects(buffer, stib_bus_times_unique)[[1]]
  comps_idx <- as.data.frame(comps_idx)
  
  #Remove piscines for which there is no overlap between buffer and any bus stop
  #if (dim(comps_idx)[1] == 0) {
  #next
  #}
  
  comps_idx <- as.integer(as.character(comps_idx$comps_idx))
  

  #Get all the neighbors:
  comps <- stib_bus_times_unique %>% filter(row_number() %in% comps_idx)
  comps <- comps %>% mutate(Id = row_number())
  
            #############################################################################################################################################################################################
            #Calculation walking time between piscine and bus stops within buffer around piscine
            ####################################################################################
  
            options(digits = 15)
  
            #Project piscine and bus stops onto the street network
            location1 <- row_interest
            location2 <- comps
            urbis <- roads
            st_crs(urbis) = CRS("+init=epsg:31370")
            st_crs(location1) = CRS("+init=epsg:31370")
            st_crs(location2) = CRS("+init=epsg:31370")
  
            library("maptools")
            location1_sp <- as(location1, 'Spatial')
            location1_sp <- SpatialPoints(location1_sp)
            location2_sp <- as(location2, 'Spatial')
            location2_sp <- SpatialPoints(location2_sp)
            urbis_sp <- as(urbis, 'Spatial')
            snap1 <- snapPointsToLines(location1_sp,urbis_sp)
            snap1 <- as.data.frame(snap1)
            snap2 <- snapPointsToLines(location2_sp,urbis_sp)
            snap2 <- as.data.frame(snap2)
            snap2 <- snap2 %>% mutate(Id = row_number())
            snap2_merged <- merge(snap2, comps, by = "Id")
  
            #Split the street network where piscine and bus stops project onto network
            library("lwgeom")
            test3 <- st_as_sf(snap1,coords = c("X", "Y"))
            on_line_all = st_cast(test3, "POINT")
            buf_all <- st_combine(st_buffer(on_line_all,0.0000000001))
            parts_all = st_collection_extract(lwgeom::st_split(urbis$geometry, buf_all),"LINESTRING")
  
            parts_all = st_as_sf(
                data.frame(
                id = 1:length(parts_all),
                geometry = parts_all
                )
            )
  
            parts_all$length <- st_length(parts_all)
  
            urbis <- parts_all
  
            library("lwgeom")
            test4 <- st_as_sf(snap2,coords = c("X", "Y"))
            on_line_all = st_cast(test4, "POINT")
            buf_all <- st_combine(st_buffer(on_line_all,0.0000000001))
            parts_all = st_collection_extract(lwgeom::st_split(urbis$geometry, buf_all),"LINESTRING")
  
            parts_all = st_as_sf(
                data.frame(
                id = 1:length(parts_all),
                geometry = parts_all
                )
            )
  
            parts_all$length <- st_length(parts_all)
  
            urbis <- parts_all
  
            #give each edge a unique index
            edges <- urbis %>%
            mutate(edgeID = c(1:n()))
  
            #create nodes at the start and end point of each edge
            nodes <- edges %>%
            st_coordinates() %>%
            as_tibble() %>%
            mutate(edgeID = L1) %>%
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
            roll <- edges[c('from','to','length')]
            roll <- data.frame(roll)
            roll <- roll[c('from','to','length')]
            colnames(roll) <- c('from','to','weight')
  
            coordinates <- nodes %>%
            st_coordinates() %>%
            as_tibble()
            nodeID <- nodes[c('nodeID')]
            coord <- cbind(coordinates,nodeID)
            coord <- data.frame(coord)
            coord <- coord[c('nodeID','X','Y')]
            colnames(coord)<- c('ID','X','Y')
  
            graph<-makegraph(roll,directed = F,coords = coord)
            
            ###################
            ##Shortest distance
            ###################
          
            #Loop over all bus stops within buffer
            datalistA <- list()
            h=2
  
            for(h in 1:nrow(snap2_merged)){ 
    
            test <- snap1
            test2 <- snap2_merged[h,]
           
            
            y <- coord[order(coord$ID),]
            row.names(y) <- y$ID
    
            node_location1 <- y %>% filter(y$Y %like% test$Y)
            node_location1B <- node_location1$ID[1]
    
            node_location2 <- y %>% filter(y$Y %like% test2$Y)
            node_location2B <- node_location2$ID[1]
    
            origin <- as.data.frame(node_location1B)
            destination <- as.data.frame(node_location2B)
            distance<-get_distance_pair(graph,from=origin,to=destination)
            distance <- as.data.frame(distance)
            distance <- distance[1,]
            distance <- as.data.frame(distance)
            temps <- (distance/(4.3*1000))*60
            names(temps)[1] <- "walk_time"
    
            datalistA[[h]] <- temps }
  
            ALL <- dplyr::bind_rows(datalistA) #Deze tabel bevat de tijd om te stappen van piscine naar alle bushaltes binnen buffer
  
            #Verbind opnieuw met comps
            comps <- cbind(comps, ALL)
  
            ################################################################################################################################################
  
  
  ###
  #Loop over all bus stops inside the buffer around piscine
  ###
  datalist2 <- list()
  i=1
  for(i in 1:nrow(comps)){
    #Extract trip nr corresponding to the selected bus stop and merge time table
    A <- filter(comps, row_number()== i)
    AA <- A[c("trip_id", "stop_id", "walk_time")]
    AA <- st_drop_geometry(AA)
    A_merged <- merge(AA, stib_bus_times_filter, by = "trip_id")
    A_merged_unique <- A_merged[!duplicated(A_merged$stop_sequence),]
    
    #Extract the arrival time corresponding to the selected bus stop
    halte <- chron(times = comps$arrival_time[i])
    
    #Calculate the time difference between the selected bus stop and each of the other stops on the same trip
    B <- c()
    for (k in 1:nrow(A_merged_unique)) {
      if (A_merged_unique$arrival_time[k] >= halte) {
        B[k] <- A_merged_unique$arrival_time[k] - halte
      } else {
        B[k] <- halte - A_merged_unique$arrival_time[k]
      }
    }

    #Convert the time difference to hh:mm:ss and add column to datatable
    BB <- chron(times = B)
    BBB <- 60 * 24 * as.numeric(times(BB))
    
    C <- cbind(A_merged_unique, BBB)
    
    #Extract stops at 20 minutes or less from the selected stop
    #reachable <- filter(C, C$BBB <= "20")  DEZE STAP WERKT NIET ???
    reachable <- C
    
    #Calculate 20minutes - BBB (travel time on bus) - walk_time (travel time by foot from piscine to bus stop)
    Diff20 <- 20-reachable$BBB-reachable$walk_time
    reachable <- cbind(reachable, Diff20)
    
    #Remove rows with Diff20 < 0
    reachable <- filter(reachable, reachable$Diff20 >= "0")
    
    #Convert from dataframe to sf again
    reachable_sf <- st_as_sf(reachable)
    
                  #st_write(reachable_sf, dsn ="J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/Processing/Isochrones_R", layer = "Piscines reachable stops", driver = "ESRI Shapefile", append = F, overwrite=T)
    Start_busstop <- comps[i,]$descr_fr
    Start_busstop <- as.data.frame(Start_busstop)
    
    ###
    #Isochrone around reachable stops
    ###
    reachable_sf <- reachable_sf %>% mutate(Id = row_number())
    datalist <- list()
    j=1
    #j=3
    for (j in 1:nrow(reachable_sf)){ 
      row_interest <- filter(reachable_sf, row_number() == j)
      
      End_busstop <- row_interest$descr_fr
      End_busstop <- as.data.frame(End_busstop)
      
      ########################################################################################################
      
      options(digits = 15)
      st_crs(row_interest) = CRS("+init=epsg:31370")
      
      #Snap points piscines on road network
      library("maptools")
      location_sp <- as(row_interest, 'Spatial')
      location_sp <- SpatialPoints(location_sp)
      roads_sp <- as(roads, 'Spatial')
      test <- snapPointsToLines(location_sp,roads_sp)
      test2 <- as.data.frame(test)
      
      #Split roadnetwork on snapped points
      library("lwgeom")
      test3 <- st_as_sf(test2,coords = c("X", "Y"))
      on_line_all = st_cast(test3, "POINT")
      buf_all <- st_combine(st_buffer(on_line_all,0.0000000001))
      parts_all = st_collection_extract(lwgeom::st_split(roads$geometry, buf_all),"LINESTRING")
      
      parts_all = st_as_sf(
        data.frame(
          id = 1:length(parts_all),
          geometry = parts_all
        )
      )
      
      parts_all$length <- st_length(parts_all)
      
      #This is the roadnetwork we use to calculate the isochrone
      roads <- parts_all
      
      
      #give each edge a unique index (edges = afzonderlijke lijntjes)
      edges <- roads %>%
        mutate(edgeID = c(1:n()))
      
      #create nodes at the start and end point of each edge (nodes vormen begin en eindpunt van een edge)
      nodes <- edges %>%
        st_coordinates() %>%
        as_tibble() %>%
        rename(edgeID = L1) %>%
        group_by(edgeID) %>%
        slice(c(1, n())) %>%
        ungroup() %>%
        mutate(start_end = rep(c('start', 'end'), times = n()/2))
      
      #give each node a unique index (nodes komen dubbel voor omdat edges op elkaar aansluiten)
      library(tidyverse)
      nodes <- nodes %>% 
        mutate(xy = paste(.$X, .$Y)) %>% 
        group_by(X, Y) %>% 
        mutate(nodeID =cur_group_id()) 
        #mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
      nodes <- nodes[,c(-5)]
      
      
      #combine the node indices with the edges (om de vorm van de functie graph goed te krijgen)
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
      
      #change column names (om de vorm van de graph functie goed te krijgen)
      roadsA <- edges[c('from','to','length')]
      roadsA <- data.frame(roadsA)
      roadsA <- roadsA[c('from','to','length')]
      colnames(roadsA) <- c('from','to','weight')
      
      coordinates <- nodes %>%
        st_coordinates() %>%
        as_tibble()
      nodeID <- nodes[c('nodeID')]
      coord <- cbind(coordinates,nodeID)
      coord <- data.frame(coord)
      coord <- coord[c('nodeID','X','Y')]
      colnames(coord)<- c('ID','X','Y')
      
      graph<-makegraph(roadsA,directed = F,coords = coord)
      
      #walking min/speed/distance
      temps <- row_interest$Diff20
      lim <- (4.3*1000/60*temps)
      lim <- as.numeric(lim)
      lim <- as.data.frame(lim)
      
     
      #Match node ID with snapped bus stop (same coordinates X and Y)
      
      ########################################################## Dit deeltje kan volgens mij simpeler door de nodeID te halen uit coord$ID die dezelfde X en Y heeft als van het snapped point j (test2)
      #y <- coord[order(coord$ID),]
      #row.names(y) <- y$ID
      #node <- data.frame()
      #node_tot <- data.frame()
      #for (g in 1:dim(test2)[1]) {
        #node_loop <- node_tot
        #node <- subset(y$ID, y$Y %like% test2[g,]$Y)
        #node_tot <- rbind(node_loop, node)
      #}
      #node_isochrones <- node_tot[,1]
      #node_isochrones <- data.frame(node_isochrones)
      #colnames(node_isochrones) <- "nodeID"
      ##########################################################
      
      hey <- filter(coord, Y %like% test2$Y | X %like% test2$X)
      node_isochrones <- hey[1,1]
      
      
      #Compute isochrone
      iso<-get_isochrone(graph,from = node_isochrones,lim = c(lim))       
      
      #Convert nodes to concave polygons with concaveman package
      coord$ID <- as.character(coord$ID)
      poly<-lapply(iso,function(t){
        t<-data.frame(noeuds=t,stringsAsFactors = F)
        t<-left_join(t,coord,by=c("noeuds"="ID"))
        return(concaveman(summarise(st_as_sf(t,coords=c("X","Y"),crs=31370))))
      })
      
      poly <- st_zm(poly)
      poly<-do.call(rbind,poly)
      poly <- cbind(poly, Name_piscine, Start_busstop, End_busstop)

      
      #Delete geometries which are not correct due to errors in the road network 
      #invalid <- st_coordinates(poly$polygons)
      #invalid <- as.data.frame(invalid)
      #invalid$L2 <- as.character(invalid$L2)
      #invalid <- count(invalid, invalid$L2)
      #invalid2 <- invalid[invalid$n<4, ]
      #poly$ID <- row.names(poly)
      #poly <- poly[!(poly$ID %in% invalid2$`invalid$L2`),]
      #poly <- poly %>% select(-ID)
      
      #ay <- st_drop_geometry(row_interest)
      #polyay <- cbind(ay, poly)
     
      
      #######################################################################################################
      
      datalist[[j]] <- poly
    }
      iso_ <- dplyr::bind_rows(datalist)
    
    datalist2[[i]] <- iso_
  }
  
    isochrones_piscine <- dplyr::bind_rows(datalist2)
  
  
  
  datalist3[[m]] <- isochrones_piscine

}
  
Isochrones_ALL <- dplyr::bind_rows(datalist3)
Isochrones_ALL$polygons <- st_geometry(Isochrones_ALL$polygons)

#Write shapefile to location on your computer
destination <- c("J:/SET-SADL_User-DI0222/Projecten/Brussel-10-min/04 Model/Processing/3. Isochrones/Public transport")
st_write(Isochrones_ALL, dsn = destination, layer = "Senior 2", driver = "ESRI Shapefile", append = F, overwrite=T)



