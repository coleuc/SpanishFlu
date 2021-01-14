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
pkgTest("zoo")
pkgTest("scales")



# set data folder and R folder
dataFolder   <- here::here("data")
RFolder      <- here::here()   
outputFolder <- here::here("output")

# set up coordinate system projection strings
WGS84 <- "+init=epsg:4326"
LV03 <- "+init=epsg:21781"

grippe <- read.csv(file = paste0(dataFolder, '/SpanischeGrippe_Faelle.csv'), sep= ';', encoding = "UTF-8")

# get only the influenza cases and sum them up by district/year
grippe <- grippe %>% filter(CatDisease == 1) %>% select(District, NumbCasesAdjust2, GEM_ID, Year, Month, Day)
grippe$Month <- sprintf("%02d", as.numeric(grippe$Month))
grippe$Day <- sprintf("%02d", as.numeric(grippe$Day))

# if not summarized by district, use following code
grippe <- drop_na(grippe, NumbCasesAdjust2) %>% select(NumbCasesAdjust2, Year, Month, Day) %>%
  unite(Date, c(Day, Month, Year), sep = ".") 
  
grippe <- grippe %>% mutate(Date = as.Date(grippe$Date, "%d.%m.%Y")) %>% select(NumbCasesAdjust2, Date) %>% group_by(Date) %>%
  mutate(Cases = sum(NumbCasesAdjust2)) %>% select(Date, Cases) %>% unique() %>% ungroup()#  %>%
  # mutate(Date = format(Date, "%d.%m.%Y"))



grippe <- grippe %>% arrange(Date) %>%
  dplyr::mutate(cases07 = zoo::rollmean(Cases, k =7, fill = NA)) %>% 
  dplyr::mutate(totalDay = cumsum(Cases))

grippe_narm <- grippe %>% drop_na()

  
ggplot(data=grippe_narm) + geom_line(aes(x=totalDay, y=cases07), size=1, color="#7a0177") +
  theme_bw() + 
  labs(title = "'Flatten the Curve' During the 1918 Influenza Pandemic",
       subtitle = "New cases (7-day avg.) vs. total cases per Bernese municipality 1918/19",
       y = "New cases",
       x = "Total cases",
       caption = "Source: Cantonal Arcives Berne,
                             Author: Corina Leuch")  +
  scale_x_continuous(labels = comma) + 
  theme_minimal() +
  theme(axis.title.x=element_text(vjust=-0.5, size = 10, face="bold"), 
        axis.title.y = element_text(vjust=2, size=10,face= "bold"), 
        axis.text = element_text(size = 10), 
        plot.title = element_text(size=12, face = "bold"), 
        plot.caption = element_text(size =8),
        plot.subtitle = element_text(size = 10))