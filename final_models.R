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
pkgTest("tidyverse")
pkgTest("glmulti")
pkgTest("stargazer")
pkgTest("xtable")
pkgTest("arm")


# set data folder and R folder
dataFolder   <- here::here("data")
RFolder      <- here::here()   
outputFolder <- here::here("output/")

# first wave ----
# first have to read data set and add factors

dat_first  <- read_csv(paste0(outputFolder, "/First_Wave_Final.csv"))

dat_first <- within(dat_first, {
  urbanity <- factor(urbanity, levels = 0:1, labels=c("Dorf", "City"))
  GEM_ID <- factor(GEM_ID)
  Rain_Quintile <- factor(Rain_Quintile, levels = c("[90.8,104]", "(104,110]" , "(110,125]" , "(125,157]", "(157,250]" ), labels = c(1,2,3,4,5))
  Inz_Quintiles <- factor(Inz_Quintiles, levels = c("[71,934]", "(934,2.02e+03]", "(2.02e+03,3.23e+03]", "(3.23e+03,5.62e+03]", "(5.62e+03,2.95e+04]")  , labels = c(1,2,3,4,5))
  TB_Quintiles <- factor(TB_Quintiles, levels = c("[0.000956,0.0501]", "(0.0501,0.0969]", "(0.0969,0.159]", "(0.159,0.286]", "(0.286,1.75]") , labels = c(1,2,3,4,5))
  btw_classes <- factor(btw_classes)
})

dat_first <- dat_first %>%
  mutate(Top_Inz = ifelse(Inz_Quintiles == 5, 1, 0)) %>%
  mutate(Top_TB = ifelse(TB_Quintiles == 5, 1, 0)) %>%
  mutate(Bottom_TB = ifelse(TB_Quintiles == 1, 1, 0))

# need to calculate terzile for TB
tb_terziles <- dat_first %>%
  pull(TBratio) %>%
  quantile(probs = seq(0, 1, length.out = 4))

dat_first <- dat_first %>%
  mutate(
    TB_Terziles = cut(
      TBratio,
      breaks = tb_terziles,
      include.lowest = TRUE
    )) %>% 
  mutate(
    TB_Terziles = factor(TB_Terziles, labels = c(1,2,3))
  )

dat_first <-  dat_first %>%
  mutate(Top_Inz = factor(Top_Inz, levels=0:1)) %>%
  mutate(Bottom_TB = factor(Bottom_TB, levels = 0:1)) %>%
  mutate(Top_TB = factor(Top_TB, levels = 0:1))

# model selection
g1 <- glmulti(Top_Inz~urbanity+AntLWS+Top_TB+btw_classes+Rain_Quintile, data = dat_first,
              level = 1,
              crit = "aic",
              plotty =TRUE, report = TRUE,
              family = "binomial",
              method = "h",
              confsetsize = 250)

top <- weightable(g1)
top <- top[top$aic <= min(top$aic) + 2,]
xtable(top)

# model
summary(m1 <- glm(Top_Inz ~ 1 + urbanity + Top_TB + btw_classes + Rain_Quintile, data = dat_first, family = 'binomial'))


# breusch-pagan test
lmtest::bptest(m1)

# calculate odds ratio and CI
odds1 <- exp(cbind(OR = coef(m1), confint(m1)))

# prepare results for reporting in table
m1.OR <- m1 # small workarounds, exponantiate coefficients of model to generate a second fake model which has the OR
m1.OR$coefficients <- exp(m1$coefficients)
stargazer(m1, m1.OR, ci=c(F,T), column.labels = c('coefficients', 'odds ratio'),
          single.row = TRUE, star.cutoffs = c(0.05,0.01,0.001),
          digits = 2, column.sep.width = "0.5pt", no.space = TRUE, 
          covariate.labels = c("urbanityCity", "Top TB Quintile", "railway2", "railway3", "railway4", 'railway5', 'rain2', 'rain3', 'rain4', 'rain4'))

dat_first$residuals <- residuals(m1)
dat_first$predicted <- predict(m1)   # Save the predicted values

# write model data (used for Moran's I in GeoDA)
write.csv(dat_first, file = paste0(outputFolder, '/FirstWaveModel.csv'))

# do this exact thing for the second wave ----
dat_second  <- read_csv(paste0(outputFolder, "/Second_Wave_Final.csv")) 

# prepare the data (assign factors)
dat_second <- within(dat_second, {
  urbanity <- factor(urbanity, levels = 0:1, labels=c("Dorf", "City"))
  GEM_ID <- factor(GEM_ID)
  Rain_Quintile <- factor(Rain_Quintile, levels = c("[228,304]","(304,334]","(334,374]", "(374,410]", "(410,661]"), labels = c(1,2,3,4,5))
  Inz_Quintiles <- factor(Inz_Quintiles, levels = c( "[284,3.63e+03]", "(3.63e+03,8.09e+03]", "(8.09e+03,1.28e+04]", "(1.28e+04,1.9e+04]",  "(1.9e+04,6.47e+04]")  , labels = c(1,2,3,4,5))
  TB_Quintiles <- factor(TB_Quintiles, levels = c("[0.000956,0.0624]", "(0.0624,0.117]", "(0.117,0.189]", "(0.189,0.337]", "(0.337,2.27]") , labels = c(1,2,3,4,5))
  btw_classes <- factor(btw_classes)
})

dat_second <- dat_second %>%
  mutate(Top_Inz = ifelse(Inz_Quintiles == 5, 1, 0)) %>%
  mutate(Top_TB = ifelse(TB_Quintiles == 5, 1, 0)) %>%
  mutate(Bottom_TB = ifelse(TB_Quintiles == 1, 1, 0))


dat_second <-  dat_second %>%
  mutate(Top_Inz = factor(Top_Inz, levels=0:1)) %>%
  mutate(Bottom_TB = factor(Bottom_TB, levels = 0:1)) %>%
  mutate(Top_TB = factor(Top_TB, levels = 0:1))

# second model selection
g2 <- glmulti(Top_Inz~urbanity+AntLWS+Top_TB+btw_classes+Rain_Quintile, data = dat_second,
              level = 1,
              crit = "aic",
              plotty =TRUE, report = TRUE,
              family = "binomial",
              method = "h",
              confsetsize = 250)

# create table with candidate models (R)
top2 <- weightable(g2)
top2 <- top2[top2$aic <= min(top2$aic) + 2,]

# create table of candidate models for latex
xtable(top2)

# actual model
summary(m2 <- glm(Top_Inz ~ 1 + urbanity + Top_TB + btw_classes + Rain_Quintile, data = dat_second, family = 'binomial'))

# breusch pagan test
lmtest::bptest(m2)

# latex table
m2.OR <- m2 # small workarounds, exponantiate coefficients of model to generate a second fake model which has the OR
m2.OR$coefficients <- exp(m2$coefficients)
stargazer(m2, m2.OR, ci=c(F,T), column.labels = c('coefficients', 'odds ratio'),
          single.row = TRUE, star.cutoffs = c(0.05,0.01,0.001),
          digits = 2, column.sep.width = "0.5pt", no.space = TRUE, 
          covariate.labels = c("urbanityCity", "Top TB inccidence", "railway2", "railway3", "railway4", 'railway5', 'rain2', "rain3", "rain4", "rain5"))

# calculate KI and odds ratio
# calculate odds ratio and 95% KI
odds2 <- exp(cbind(OR = coef(m2), confint(m2)))

# save residuals and predicted
dat_second$residuals <- residuals(m2)
dat_second$predicted <- predict(m2)

write.csv(dat_second, file = paste0(outputFolder, '/SecondWaveModel.csv'))

