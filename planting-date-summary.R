library(tidyverse)
library(lubridate)

list.files("data/")

dates <- read_csv("data/Trials_Dates.csv")
plots <- read_csv("data/PlotVarietyFile.csv")

planting_date_varieties <- data.frame(
  Variety = c("Yuma-2", "Puma-3", "Bama", "BerryBlossom", "CherryBlossomxTI",
              "Eletta", "Tygra", "CarmagnolaSelezionata"),
  Rate_plot = c(900, 1500, 900, 60, 60, 1500, 900, 900),
  Rate_zone = c(167, 278, 167, 60, 60, 278, 167, 167),
  Region = c("China", "China", "China", "USA", "USA",
             "Europe", "Europe", "Europe")
)

### Emergence Data
emergence_data <- list()
for (experiment in dates$Experiment){
  plot_varieties <- plots %>%
    filter(Experiment == experiment)
  emergence_file <- str_subset(list.files("data/"), paste0(experiment, ".*Emergence.*"))
  emergence_data[[experiment]] <- read_csv(paste0("data/", emergence_file)) %>%
    mutate(Date = ymd(paste(Year, Month, Day, sep = "-"))) %>%
    left_join(plot_varieties)
}

avg_emergence <- emergence_data[["PilotPlot1"]] %>%
  group_by(Date, Variety) %>% 
  summarize(avg_emergence = mean(Emergence, na.rm=TRUE)) %>% 
  left_join(select(planting_date_varieties, Variety, Rate_zone)) %>%
  mutate(percent_emergence = avg_emergence/Rate_zone*100)