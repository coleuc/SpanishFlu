# boxplots ----
# write csv

grippe_levels <- read.csv(grippe_levels, path = paste0(outputFolder, "/Grippe_Final.csv"))
first_levels <- read.csv(first_final, path = paste0(outputFolder, "/First_Wave_Final.csv"))
second_levels <- read.csv(second_final, path = paste0(outputFolder, "/Second_Wave_Final.csv"))

# the incidence data seems to have some outliers. Create boxplot 
colnames(first_levels)[3] <- "Inz"
first_levels$wave <- "First wave"


colnames(second_levels)[3] <- "Inz"
second_levels$wave <- "Second wave"

grippe_levels$wave <- "Entire period"

dat <- bind_rows(grippe_levels, first_levels,second_levels)
dat$wave <- factor(dat$wave, levels = c("Entire period", 'First wave', 'Second wave'))

ggplot(dat, aes(x=wave, y=Inz), group_by(wave)) + 
  geom_boxplot(fill="#7a0177", alpha=0.8) + 
  theme_minimal() + 
  scale_x_discrete(name = "Wave") +
  ggplot2::labs(title = "Distribution of incidences",
                subtitle = "Incidence for the entire study period and the two waves",
                y = "Incidence (cases/100'000 inhabitants)",
                x = "Wave",
                caption = "Source: Cantonal Archives Berne,
                             Author: Corina Leuch") +
  theme_minimal() + 
  theme(axis.title.x=element_text(vjust=-0.5, size = 10, face="bold"), 
        axis.title.y = element_text(vjust=2, size=10,face= "bold"), 
        axis.text = element_text(size = 10), 
        plot.title = element_text(size=12, face = "bold"), 
        plot.caption = element_text(size =8), 
        plot.subtitle = element_text(size = 10))
