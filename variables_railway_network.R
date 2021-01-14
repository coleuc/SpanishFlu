# this code creates a navigable railway network using the prepared railway stations. 
# in order for this code to work, the dataset has to be in the following form:
# nodes: point data with one point at each station
# edges: line data, that is cut at every node. Each line data has to start at a point and end at a point.

# for the scope of this thesis the data was prepared by hand and using basic python.


local({r <- getOption("repos")
r["CRAN"] <- "http://cran.r-project.org" 
options(repos=r)
})

# function: checks if a package is installed or not 
# if installed --> loads package
# else --> installs package

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# import packages using function
pkgTest("tidyverse")
pkgTest("here")
pkgTest("sf")
pkgTest("tmap")
pkgTest("tidygraph")
pkgTest("igraph")
pkgTest("leaflet")


# set data folder and R folder
dataFolder   <- here::here("data")
RFolder      <- here::here()   
outputFolder <- here::here("output")

#projection strings
WGS84 <- "+init=epsg:4326"
LV03 <- "+init=epsg:21781"


# import the edes and nodes
nodes <- st_read(dsn=paste0(dataFolder, "/nodes_new.shp")) %>% select(-1)
edges <- st_read(dsn=paste0(dataFolder, "/edges_new.shp")) %>% st_set_geometry(NULL) %>% select(9, 10, 11) # drop geometry - will be added again later

node_coords <- do.call(rbind, st_geometry(nodes)) %>% 
  as_tibble() %>% setNames(c("y","x"))

nodes <- bind_cols(nodes, node_coords) %>% st_set_geometry(NULL) # drop geometry, so it doesn't get mixed up later (will be added again)

# We add the short names and coordinates for "from station" and "to station"
edges <- edges %>% 
  # Match "from stations"
  inner_join(select(nodes, c(from_y = "y",
                                from_x = "x",
                                from_id = "stop_id")),
             by = c("From_Node" = "from_id")) %>%
  # Match "to stations""
  inner_join(select(nodes, c(to_y = "y",
                                to_x = "x",
                                to_id = "stop_id")),
             by = c("To_Node" = 'to_id')) 

nodes <- nodes %>%
  st_as_sf(coords = c("y", "x"), crs=21781)

# transform to wgs84 (for leaflet)
nodes <- st_transform(nodes, WGS84)

edges_sf <- edges %>%
  mutate(from = paste(from_y, from_x, sep = " ")) %>%
  mutate(to = paste(to_y, to_x, sep = " ")) %>%
  mutate(coords = paste0("LINESTRING(",from, ", ", to,")")) %>%
  select(-one_of(c("from", "to", 
                   "to_y", "to_x",
                   "from_y", "from_x"))) %>% 
  st_as_sf(wkt="coords", crs=21781)  %>% st_transform(WGS84)

network <- igraph::graph_from_data_frame(as_tibble(edges_sf), vertices = nodes) %>% as_tbl_graph()



# One more thing and we are done: remove loops(edges that start and end at the same vertex)
network <- as_tbl_graph(igraph::simplify(network, 
                                              remove.multiple=F, remove.loops=T))

# calculate node betweeness centrality
network <- network %>%   activate(nodes) %>%
  mutate(btw = centrality_betweenness(weights=edges_sf$Shape_Leng, directed = FALSE))


# Plot the leaflet map
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = network %>% 
    activate (nodes) %>% 
    as_tibble() %>%
    select("btw") %>%
    pull())


leaflet() %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "Terrain")  %>% 
  
  # Add edges
  addPolylines(data = network %>% 
                 activate (edges) %>% 
                 as_tibble() %>%
                 st_as_sf()) %>% 
  # Add point marker            
  addCircleMarkers(data=network %>% 
                  activate(vertices) %>% 
                  as_tibble() %>%
                  st_as_sf(),
                  color =~pal(btw), 
                  stroke = FALSE,
                  radius = 1.5,
                  fillOpacity = 0.8)

# now we can write the nodes back into a shapefile which we can then use in the gis
nodes_btw <- network %>% activate(nodes) %>% as_tibble() %>% st_as_sf(crs=WGS84) %>% st_transform(LV03)

btw_tibble <- network %>% activate(nodes) %>% as_tibble()

st_write(nodes_btw, paste0(RFolder, '/nodes_btw.shp'))

geometry <- st_read(paste0(dataFolder, "/Bern_Punkt_id.shp"), encoding="UTF-8")
railways <- st_read(paste0(dataFolder, "/railway_stations_bern.shp"))

gemeinden <- st_join(geometry, railways) %>% select(Gemeinde, GEM_ID, btw) %>% st_transform(WGS84)
st_write(gemeinden,paste0(RFolder, "/Gemeinden_Erreichbarkeit.shp"))



