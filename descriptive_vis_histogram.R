# make histogram: this code prepares the data and afterwards creates a nice looking histogram
# set up part ----
# define standard repo for rpackages
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
pkgTest("lubridate")


# set data folder and R folder
dataFolder   <- here::here("data")
RFolder      <- here::here()   
outputFolder <- here::here("output")

# set up coordinate system projection strings
WGS84 <- "+init=epsg:4326"
LV03 <- "+init=epsg:21781"

# read the data -> this part is hard-coded. Change if code needs to be executed
gemeinden <- read.csv(file = paste0(dataFolder, '/SpanischeGrippe_Gemeinden.csv'), sep= ';', encoding  = "UTF-8") %>% rename(GEM_ID= X.U.FEFF.GEM_ID) # have to rename the id number to match it
grippe <- read.csv(file = paste0(dataFolder, '/SpanischeGrippe_Faelle.csv'), sep= ';', encoding = "UTF-8")

# get only the influenza cases and sum them up by district/year
grippe <- grippe %>% filter(CatDisease == 1) %>% select(District, NumbCasesAdjust2, GEM_ID, Year, Month, Day) # %>% group_by(GEM_ID) %>% summarise(totalCases = sum(NumbCasesAdjust2)) %>% ungroup()

# joining the gemeinden data to the disease data
grippe_gemeinden <- left_join(grippe, gemeinden, by = 'GEM_ID')

# check for N/As 
na <- grippe_gemeinden %>% filter(is.na(N)) %>% as_tibble() # data contains some entries that could not be matched with a district. These data are omitted for now
# nas seem to be things that are aggregated by districts --> good that they are na, so that they aren't accidentally included in the analysis

# grippe_gemeinden <- drop_na(grippe_gemeinden, c(N,E)) %>% select(Gemeinde_Name, totalCases, Wohnb, E, N, GEM_ID, BfS_GEM_ID)
# if not summarized by district, use following code
grippe_gemeinden <- drop_na(grippe_gemeinden, c(N,E)) %>% select(Gemeinde_Name, NumbCasesAdjust2, Wohnb, E, N, GEM_ID, BfS_GEM_ID, Year, Month, Day) %>%
  unite(Date, c(Year, Month, Day), sep = "/")

# calculate morbitity
grippe_gemeinden <- grippe_gemeinden %>% group_by(GEM_ID) %>% mutate(inccidence = sum(NumbCasesAdjust2)/Wohnb*100000) %>%
  select(GEM_ID, inccidence, Gemeinde_Name, Wohnb) %>% unique()# calculate cases per 1000 inhabitants 

# make a histogram --> one value for each municipality
ggplot(data = grippe_gemeinden, aes(grippe_gemeinden$inccidence)) + 
  geom_histogram(fill="#7a0177", alpha=0.8, bins = 60) +
  expand_limits(x = 10000) + # make sure axis labels arent being cut off
  ggplot2::labs(title = "Burden of Disease in Bernese Municipalities",
                subtitle = "Histogram of the influenza incidence Jul. 1918 - Dec. 1918",
                y = "Number of municipalities",
                x = "Incidence (cases/100'000 inhabitants)",
                caption = "Source: Cantonal Arcives Berne,
                             Author: Corina Leuch") +  
  theme_minimal() + 
  theme(axis.title.x=element_text(vjust=-0.5, size = 12, face="bold"), 
        axis.title.y = element_text(vjust=2, size=12,face= "bold"), 
        axis.text = element_text(size = 8), 
        plot.title = element_text(size=16, face = "bold"), 
        plot.caption = element_text(size =8),
        plot.subtitle = element_text(size = 12)) +
  theme(legend.position = "none", plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
