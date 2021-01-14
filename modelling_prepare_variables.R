# this script gathers all the variables in one dataset and creates the correlation plots

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


# load packages
pkgTest("MASS")
pkgTest("tidyverse")
pkgTest("classInt")  # needed for Jenks 
pkgTest("graphics")
pkgTest("sf")
pkgTest("lattice")
pkgTest("RColorBrewer")

# set data folder and R folder
dataFolder   <- here::here("data")
RFolder      <- here::here()   
outputFolder <- here::here("output")


# load oliver's helper function
source("helper.r")

# read data  and calculate overall incidence ----
grippe <- read_csv(paste0(dataFolder, '/Monthly_Cases.csv')) %>% group_by(GEM_ID) %>%
  mutate(cases=sum(montlyCases)) %>% dplyr::select(GEM_ID, Wohnb, cases) %>%
  unique()  %>% mutate(Inz = cases/Wohnb*100000) %>% na.omit(grippe)

gemeinden <- read_delim(paste0(dataFolder, "/gemeinden.csv"), delim =";") %>% 
  select(GEM_ID, Gemeinde_Name, LWS, TB, Haush, Hoehe, Wohnb, E, N) %>% mutate(HauGr =Wohnb/Haush) %>%
  mutate(AntLWS = LWS/Wohnb*100) %>% mutate(TBratio = TB/TB/Wohnb*100) %>% select(-Wohnb, -TB, -Haush, -LWS) %>%
  na.omit()

# calculate first wave incidence ----
first <- read_csv(paste0(dataFolder, '/First_Wave.csv')) %>% group_by(GEM_ID) %>%
  mutate(cases=sum(overallCases)) %>% dplyr::select(GEM_ID, Wohnb, cases) %>%
  unique() %>% mutate(Inz_first = cases/Wohnb*100000) %>% na.omit()

second <- read_csv(paste0(dataFolder, '/Second_Wave.csv')) %>% group_by(GEM_ID) %>%
  mutate(cases=sum(overallCases)) %>% dplyr::select(GEM_ID, Wohnb, cases) %>%
  unique() %>% mutate(Inz_second = cases/Wohnb*100000) %>% na.omit()


# join all the datasets together
first <- inner_join(first, gemeinden, "GEM_ID")
second <- inner_join(second, gemeinden, "GEM_ID")
grippe <- inner_join(grippe, gemeinden, 'GEM_ID')

# now I need to calculate quintiles for every variable I want. This is TB and inccidence for now

inz_total_quint <- grippe %>%
  pull(Inz) %>%
  quantile(probs = seq(0, 1, length.out = 6))

TB_total_quint <- grippe %>%
  pull(TBratio) %>%
  quantile(probs=seq(0,1, length.out = 6))

grippe <- grippe %>%
  mutate(
    Inz_Quintiles = cut(
      Inz,
      breaks = inz_total_quint,
      include.lowest = TRUE
    ),
    TB_Quintiles = cut(
      TBratio,
      breaks = TB_total_quint,
      include.lowest = TRUE
    )) %>% 
  select(GEM_ID, Gemeinde_Name, Inz, Inz_Quintiles, TBratio, TB_Quintiles, AntLWS, HauGr, Wohnb)

# now I have to do the same thing for the first and second wave respectively
inz_first_quint <- first %>%
  pull(Inz_first) %>%
  quantile(probs = seq(0, 1, length.out = 6))

TB_first_quint <- first %>%
  pull(TBratio) %>%
  quantile(probs=seq(0,1, length.out = 6))

first <- first %>%
  mutate(
    Inz_Quintiles = cut(
      Inz_first,
      breaks = inz_first_quint,
      include.lowest = TRUE
    ),
    TB_Quintiles = cut(
      TBratio,
      breaks = TB_first_quint,
      include.lowest = TRUE
    )) %>% 
  select(GEM_ID, Gemeinde_Name, Inz_first, Inz_Quintiles, TBratio, TB_Quintiles, AntLWS, HauGr, Wohnb)

# second wave
inz_second_quint <- second %>%
  pull(Inz_second) %>%
  quantile(probs = seq(0, 1, length.out = 6))

TB_second_quint <- second %>%
  pull(TBratio) %>%
  quantile(probs=seq(0,1, length.out = 6))

second <- second %>%
  mutate(
    Inz_Quintiles = cut(
      Inz_second,
      breaks = inz_second_quint,
      include.lowest = TRUE
    ),
    TB_Quintiles = cut(
      TBratio,
      breaks = TB_second_quint,
      include.lowest = TRUE
    )) %>% 
  select(GEM_ID, Gemeinde_Name, Inz_second, Inz_Quintiles, TBratio, TB_Quintiles, AntLWS, HauGr, Wohnb)

# some variables are still missing: urbanity, weather and railways
# urbaniity: everything that's smaller than 15'000 in one class

grippe <- grippe %>% mutate(
  urbanity = ifelse(Wohnb < 10000, 0,1)
)

first <- first%>% mutate(
  urbanity = ifelse(Wohnb < 10000, 0,1)
)

second <- second %>% mutate(
  urbanity = ifelse(Wohnb < 10000, 0,1)
)

# weather data ---- 
wetter <- read_delim(paste0(dataFolder, "/rain_gemeinden.csv"), delim =";") %>% 
  mutate(first_wave = prec_1,second_wav = perc_2,Total_rain = prec_all) %>%
  select(first_wave, second_wav, Total_rain, GEM_ID)

geometry <- st_read(dsn = paste0(dataFolder, '/Bern_Punkt_id.shp'))

grippe <- left_join(grippe, wetter, "GEM_ID") %>% select(-first_wave, -second_wav)
first <- left_join(first, wetter, "GEM_ID") %>% select( -Total_rain, -second_wav)
second <- left_join(second, wetter, "GEM_ID") %>% select(-Total_rain, -first_wave)


wetter_total_quint <- grippe %>%
  pull(Total_rain) %>%
  quantile(probs = seq(0, 1, length.out = 6))

grippe <- grippe %>%
  mutate(
    Rain_Quintile = cut(
      Total_rain,
      breaks = wetter_total_quint,
      include.lowest = TRUE
    ))

wetter_first_quint <- first %>%
  pull(first_wave) %>%
  quantile(probs = seq(0, 1, length.out = 6))

first <- first %>%
  mutate(
    Rain_Quintile = cut(
      first_wave,
      breaks = wetter_first_quint,
      include.lowest = TRUE
    ))

wetter_second_quint <- second %>%
  pull(second_wav) %>%
  quantile(probs = seq(0, 1, length.out = 6))

second <- second %>%
  mutate(
    Rain_Quintile = cut(
      second_wav,
      breaks = wetter_second_quint,
      include.lowest = TRUE
    ))

# finally: the railway stations
railway_sf <- st_read(paste0(dataFolder, "/Gemeinden_Erreichbarkeit_final.shp"))
railway <- st_set_geometry(railway_sf, NULL) %>% mutate(btw = replace_na(btw, 0))

freq_table <- railway_sf %>%
  dplyr::count(GEM_ID) %>%
  group_by(GEM_ID) %>%          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n))

# some municipalities have two train stations and this needs to be adressed by looking at the geometry (i.e selecting the relevant one)
# furthermore the ones where GEM_ID is zero are adressed
# read new dataset (don't have GEM_ID yet) -> this is done by hand
railway_new_sf <- st_read(paste0(dataFolder, "/Gemeinden_Erreichbarkeit_final.shp")) %>% 
  select("Gemeinde", "btw", "GEM_ID")

freq_table <- railway_new_sf %>%
  dplyr::count(GEM_ID) %>%
  group_by(GEM_ID) %>%          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n))

# select only the GEM ID and btw, delete Geometry (not required)
railway_gemeinden <- railway_new_sf %>% st_set_geometry(NULL) %>%
  select(btw, GEM_ID)

grippe_final <- left_join(grippe, railway_gemeinden, by="GEM_ID")
first_final <- left_join(first, railway_gemeinden, by='GEM_ID')
second_final <- left_join(second, railway_gemeinden, by='GEM_ID')

# classify btw
btw_not_zero <- filter(grippe_final, btw>0)
first_not_zero <- filter(first_final, btw>0)
second_not_zero <- filter(second_final, btw>0)


classIntervals(btw_not_zero$btw, n=4, style = "jenks")
classIntervals(first_not_zero$btw, n=4, style = "jenks")
classIntervals(second_not_zero$btw, n=4, style = "jenks")

grippe_final <- grippe_final %>% mutate(btw_classes = 
                                        ifelse(btw == 0, 1, 
                                        ifelse(btw <= 13896, 2, 
                                        ifelse(btw <= 30624, 3,
                                        ifelse(btw <= 87852,4,5)))))

first_final <- first_final %>% mutate(btw_classes = 
                                        ifelse(btw == 0, 1, 
                                               ifelse(btw <= 15167, 2, 
                                                      ifelse(btw <= 42005, 3,
                                                             ifelse(btw <= 87852,4,5)))))
second_final <- second_final %>% mutate(btw_classes = 
                                          ifelse(btw == 0, 1, 
                                                 ifelse(btw <= 13896, 2, 
                                                        ifelse(btw <= 30624, 3,
                                                               ifelse(btw <= 69866,4,5)))))


# pairs plot first wave, absolute
# rename the names to make pairs plot more clear
first_pairs <- first_final %>%
  rename(
    TB = TBratio,
    rain = first_wave,
    agriculture = AntLWS,
    railway = btw
    
  )

variables <- c("TB", "rain", "agriculture", 'railway', "urbanity")
pairs(first_pairs[variables], lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist, cex.labels=2,
      main = "Correlation between Explanatory Factors (First Wave)")


# pairs Plot second wave, absolute
second_pairs <- second_final %>%
  rename(
    TB = TBratio,
    rain = second_wav,
    agriculture = AntLWS,
    railway = btw
  )

pairs(second_pairs[variables], lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist, cex.labels=2,
      main = "Correlation between Explanatory Factors (Second Wave)", sub='test')
# write csv
write_csv(grippe_levels, path = paste0(outputFolder, "/Grippe_Final.csv"))
write_csv(first_final, path = paste0(outputFolder, "/First_Wave_Final.csv"))
write_csv(second_final, path = paste0(outputFolder, "/Second_Wave_Final.csv"))
