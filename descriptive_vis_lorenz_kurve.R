# this script draws a lorenz curve to show the inequality in incidence rates
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
pkgTest("gglorenz")

# set data folder and R folder
dataFolder   <- here::here("data")
RFolder      <- here::here()   
outputFolder <- here::here("output")

# set up coordinate system projection strings
WGS84 <- "+init=epsg:4326"
LV03 <- "+init=epsg:21781"

grippe <- read.csv(file = paste0(dataFolder, '/SpanischeGrippe_Faelle.csv'), sep= ';', encoding = "UTF-8")%>% 
  filter(CatDisease == 1) %>% select(NumbCasesAdjust2, GEM_ID) %>% drop_na() %>% group_by(GEM_ID) %>%
  mutate(Cases = sum(NumbCasesAdjust2)) %>% select(GEM_ID, Cases) %>% unique()

gemeinden <- read.csv(file= paste0(dataFolder, '/SpanischeGrippe_Gemeinden.csv'), sep= ';', encoding = "UTF-8") %>%
  rename(GEM_ID=X.U.FEFF.GEM_ID)

grippe <- left_join(grippe, gemeinden, by= "GEM_ID")
grippe <- grippe %>%select(Wohnb, Cases) %>% rename(Population=Wohnb) %>% ungroup() %>% select(Cases, Population) %>% mutate(Inz = Cases/Population*100000)

ggplot(grippe) +
  stat_lorenz(aes(Inz),color="#7a0177", size=1)+
  coord_fixed()+
  geom_abline(linetype = "dashed") +
  theme_minimal() + 
  theme(axis.title.x=element_text(vjust=-0.5, size = 10, face="bold"), 
        axis.title.y = element_text(vjust=2, size=10,face= "bold"), 
        axis.text = element_text(size = 10), 
        plot.title = element_text(size=12, face = "bold"), 
        plot.subtitle = element_text(size = 10), 
        plot.caption = element_text(size = 8))+
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  labs(x = "Cumulative percentage of population",
       y = "Cumulative percentage of incidence",
       title = "Distribution of Cases Among the Population on a Municipality Level",
       subtitle = "Canton of Berne, July 1918 - December 1919",
       caption = "Source: Cantonal Arcives Berne,
                             Author: Corina Leuch") +
  annotate_ineq(grippe$Cases) +
  theme(legend.position = "none", plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
