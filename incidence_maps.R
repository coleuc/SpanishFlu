# this script works as a baseline for various maps showing the incidence per month. The maps themselves were later created in 
# QGIS and illustrator using the shapefiles generated in this code.

# set up part 
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
pkgTest("rgdal")


# set data folder and R folder
dataFolder   <- here::here("data")
RFolder      <- here::here()   
outputFolder <- here::here("output")

#projection strings
WGS84 <- "+init=epsg:4326"
LV03 <- "+init=epsg:21781"

# prepare the data 
dat <- read.csv(file = paste0(dataFolder, '/SpanischeGrippe_Faelle.csv'), sep= ';', encoding = "UTF-8")
gemeinden <- read.csv(file = paste0(dataFolder, '/SpanischeGrippe_Gemeinden.csv'), sep= ';', encoding  = "UTF-8") %>% rename(GEM_ID= X.U.FEFF.GEM_ID) # have to rename the id number to match it

# clean up dataset ----
# join together disease data and district data in order to get the number of inhabitants. Furthermore: some rows have no Gemeinde associated with it (things like
# "Im Amt", "im ganzen Bezirk"). These can not be used for this analysis and are removed
dat <- left_join(dat, gemeinden, by = 'GEM_ID') %>% drop_na(GEM_ID)
dat$Month <- sprintf("%02d", as.numeric(dat$Month)) # convert 1 to 01 in month --> easier sorting


# group by month, calculate incidence per district and month
dat_grouped <- dat %>% filter(CatDisease == 1) %>% select(Gemeinde_Name, NumbCasesAdjust2, Wohnb, GEM_ID, Year, Month, BfS_GEM_ID, E, N) %>% group_by(GEM_ID, Gemeinde_Name, Year, Month, Wohnb, BfS_GEM_ID, E, N) %>%
  summarise(montlyCases = sum(NumbCasesAdjust2)) %>% unite(Month, c(Year, Month), sep = "_") %>% mutate(Inz = montlyCases/Wohnb*100000) %>% ungroup()

write_csv(dat_grouped, paste0(RFolder, "Monthly_Cases.csv"))

# check for N/A
na <- dat_grouped %>% filter(is.na(GEM_ID)) %>% as_tibble()

geometry <- st_read(dsn = paste0(dataFolder, "/Bern_Punkt_id.shp")) %>% `st_crs<-`(LV03)

dat_grouped_sf <- left_join(geometry, dat_grouped, "GEM_ID") %>% na.omit(dat_grouped_sf$Month)
# st_write(dat_grouped_sf, paste0(outputFolder,'/Monthly_Incidences.shp'), driver = 'ESRI Shapefile') # issue here: doesn't write date; seems to be a known problem
months <- read.csv(file=paste0(dataFolder, "/months.csv"), sep = ";", encoding="UTF-8") %>% rename(Month= X.U.FEFF.Month)

dat_grouped_sf <- left_join(dat_grouped_sf, months, by="Month") # could be used to make title pretty, but doesn't work for now

month_list <- dat_grouped %>% distinct(Month) %>% pull()

# this function creates a shapefile with the incidence per municipality for each month
createMonthlyData <- function(month, data=dat_grouped_sf){
  monthly <- data %>% filter(Month==month)
  st_write(monthly,paste0(outputFolder, "/07_monthly_incidence", month, ".shp"))
}

lapply(month_list, createMonthlyData)
# use same breaks as in gif

lapply(month_list, createStaticMaps, field="Inz", name="01_Monthly_Incidence_")


#### if all the maps are prepared and exported to pngs, this code creates a nice looking gif
files <- list.files(path = enFOlder, pattern = "*.png", full.names=TRUE)
all_im <- image_read(files)
scaled <- image_scale(all_im,"1000!")

animation <- image_animate(all_im, fps = 1, dispose = "previous")
image_write(animation, paste0(outputFolder, "/01_Cases_Month_EN.gif"))


# the code beow creates a shapefile for each wave which can then be turned into a map using QGIS/Ilu

# select data of first wave and bring to form we want
first_wave <- dat %>% 
  filter(CatDisease == 1) %>% 
  select(Gemeinde_Name, NumbCasesAdjust2, Year, Month, Wohnb,GEM_ID) %>%
  group_by(GEM_ID, Gemeinde_Name, Wohnb, Year, Month) %>%
  unite(Month, c(Year, Month), sep = "_") %>% 
  filter(Month == "1918_07"| Month == "1918_08") %>% ungroup() %>%
  group_by(GEM_ID, Wohnb, Gemeinde_Name) %>%
  summarise(overallCases = sum(NumbCasesAdjust2)) %>%
  mutate(Inz = overallCases/Wohnb*100000) %>% ungroup()

first_wave_sf <- left_join(geometry, first_wave, by="GEM_ID")
st_write(first_wave_sf, paste0(outputFolder,'/Incidences_1st_wave.shp'), driver = 'ESRI Shapefile') # issue here: doesn't write date; seems to be a known problem

# select data for second wave and bring to form we want
second_wave <- dat %>% 
  filter(CatDisease == 1) %>% 
  select(Gemeinde_Name, NumbCasesAdjust2, Year, Month, Wohnb,GEM_ID) %>%
  group_by(GEM_ID, Gemeinde_Name, Wohnb, Year, Month) %>%
  unite(Month, c(Year, Month), sep = "_") %>% 
  filter(Month == "1918_10"| Month == "1918_11"| Month == "1918_12"| Month == "1919_01") %>% ungroup() %>%
  group_by(GEM_ID, Wohnb, Gemeinde_Name) %>%
  summarise(overallCases = sum(NumbCasesAdjust2)) %>%
  mutate(Inz = overallCases/Wohnb*100000) %>% ungroup()

second_wave_sf <- left_join(geometry, second_wave, by="GEM_ID")
st_write(second_wave_sf, paste0(outputFolder,'/Incidences_2nd_wave.shp'), driver = 'ESRI Shapefile')
