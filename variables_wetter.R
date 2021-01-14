# this script rearranges the weather data and joins the actual data to the staions. Using the output of this script,
# a surface with the precipitation for each wave and the overall data can be interpolated using ArcGIS.

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


# set data folder and R folder
dataFolder   <- here::here("data")
RFolder      <- here::here()   
outputFolder <- here::here("output")

#projection strings
WGS84 <- "+init=epsg:4326"
LV03 <- "+init=epsg:21781"

# read datasets
stationen <- read.csv(file = paste0(dataFolder, '/stationen.csv'), sep= ';', encoding = "UTF-8")
stationen <- na.omit(stationen)

wetter <- read.csv(file = paste0(dataFolder, '/wetter_neu.csv'), sep= ';', encoding = "UTF-8") %>% as_tibble()
wetter$month <- sprintf("%02d", as.numeric(wetter$month)) # convert 1 to 01 in month --> easier sorting

# entire dataset----
# summarise the amount of rain for every station --> later used for interpolation, filter out the timespan where we don't have
# any influenza data
wetter_grpd <- wetter %>% unite(month, c(year, month), sep = "_") %>% group_by(month,stn) %>% 
  mutate(rain=sum(as.numeric(rre150d0))) %>% select(stn, month, rain) %>% filter(month != "1918_01") %>% 
  filter(month != "1918_02") %>% filter(month != "1918_03")%>% filter(month != "1918_04")%>% filter(month != "1918_05")%>% 
  filter(month != "1918_06") %>% group_by(stn) %>% mutate(rain = sum(as.numeric(rain))) %>% select(stn, rain) %>% unique()

first_wave <- wetter %>% unite(month, c(year, month), sep = "_") %>% group_by(stn) %>%
  filter(month== "1918_08"| month == "1918_07") %>%
  mutate(rain_first = sum(rre150d0)) %>% select(stn, rain_first) %>% unique() %>% ungroup()

second_wave <- wetter %>% unite(month, c(year, month), sep = "_") %>% group_by(stn) %>%
  filter(month== "1918_10"| month == "1918_11"| month == "1918_12"| month=="1919_01") %>%
  mutate(rain_second = sum(rre150d0)) %>% select(stn, rain_second) %>% unique() %>% ungroup()

# now the dataset is in the form that we want it, we can join it to the stations
station_weather <- left_join(stationen, wetter_grpd, by="stn")
station_weather <- left_join(station_weather, first_wave, by="stn")
station_weather <- left_join(station_weather, second_wave, by="stn")

# convert to sf
stations_sf <- st_as_sf(station_weather, coords = c("E", "N"), crs=LV03)

st_write(stations_sf, paste0(outputFolder,"/stations.shp"), driver="ESRI Shapefile")
write_csv(station_weather, paste0(outputFolder, "/stations_weather.csv"))
write_csv(first_wave, paste0(outputFolder, "/stations_weather_first_wave.csv"))
write_csv(second_wave, paste0(outputFolder, "/stations_weather_second_wave.csv"))

