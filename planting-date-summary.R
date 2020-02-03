library(tidyverse)
library(lubridate)
library(agricolae)

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

### Emergence Data  -  NOT COMPLETE
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

### Stand Count
for (experiment in dates$Experiment){
  plot_varieties <- plots %>%
    filter(Experiment == experiment)
  stand_count_file <- str_subset(list.files("data/"), paste0(experiment, ".*StandCount.*"))
  stand_count_trial <- read_csv(paste0("data/", stand_count_file)) %>%
    left_join(plot_varieties, by = c("Block", "Row", "Column"))
  if (exists("stand_count_data")){
    stand_count_data <- bind_rows(stand_count_data, stand_count_trial)
  } else {
    stand_count_data <- stand_count_trial
  }
}

stand_count_pilot <- stand_count_data %>%
  filter(Variety %in% planting_date_varieties$Variety) %>% 
  filter(Day %in% c(NA, 7, 17)) %>% 
  left_join(planting_date_varieties, by = "Variety") %>% 
  mutate(stand_percent = StandCount/Rate_plot*100)

stand_count_test <- aov(stand_percent~Experiment + Variety + Experiment*Variety,
                        data = stand_count_pilot)
summary(stand_count_test)
HSD.test(stand_count_test, "Variety")$groups
HSD.test(stand_count_test, "Experiment")$groups

stand_count_summary_pilot <- stand_count_pilot %>%
  filter(Variety %in% planting_date_varieties$Variety) %>% 
  filter(Day %in% c(NA, 7, 17)) %>% 
  left_join(planting_date_varieties, by = "Variety") %>%
  ungroup() %>% 
  mutate(Variety = factor(Variety, levels = c("Yuma-2", "Puma-3", "Bama", "Eletta", "Tygra", "CarmagnolaSelezionata",
                                              "BerryBlossom", "CherryBlossomxTI")),
         PlantingDate = factor(Experiment, 
                               levels = c("PilotPlot1", "VarietyTrial", "PilotPlot2", "PilotPlotPlus8"),
                               labels = c("May-1", "May-21", "June-21", "July-18"))) %>%
  group_by(PlantingDate, Variety) %>% 
  summarize(         
    avg_stand_percent= mean(stand_percent, na.rm=TRUE),
    sd_stand_percent = sd(stand_percent, na.rm=TRUE))

ggplot(stand_count_summary_pilot, aes(x=PlantingDate, y=avg_stand_percent)) +
  geom_bar(stat = "identity") +
  labs(x = "Planting Date", y = "Stand Establishment [% of live seed + SD]") +
  geom_errorbar(aes(ymin = avg_stand_percent, ymax = avg_stand_percent+sd_stand_percent)) +
  facet_grid(.~Variety) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

stand_count_summary_variety <- stand_count_data %>%
  filter(Experiment == "VarietyTrial") %>% 
  group_by(Variety, Day) %>% 
  summarize(avg_stand_count = mean(StandCount, na.rm=TRUE),
            sd_stand_count = sd(StandCount, na.rm=TRUE))

### Flowering
for (experiment in dates$Experiment){
  plot_varieties <- plots %>%
    filter(Experiment == experiment)
  flower_file <- str_subset(list.files("data/"), paste0(experiment, ".*Flowering.*"))
  flower_trial <- read_csv(paste0("data/", flower_file)) %>%
    left_join(plot_varieties, by = c("Block", "Row", "Column"))
  if (exists("flower_data")){
    flower_data <- bind_rows(flower_data, flower_trial)
  } else {
    flower_data <- flower_trial
  }
}

flower_pilot <- flower_data %>%
  filter(Variety %in% planting_date_varieties$Variety) %>%
  mutate(Date = ymd(paste(Year, Month, Day, sep="-"))) %>% 
  arrange(desc(FemaleOpen_perc),  desc(MaleOpen_perc), Date, Variety)

flower_induc_pilot <- flower_pilot %>%
  filter(Induc_perc == 100) %>%
  group_by(Experiment, Variety, Block) %>% 
  summarize(induc_date = min(Date)) %>%
  group_by(Experiment, Variety) %>%  
  summarize(induc_date = min(induc_date), avg_date = mean(induc_date))

flower_female_pilot <- flower_pilot %>%
  filter(FemaleOpen_perc == 100) %>%
  group_by(Experiment, Variety) %>% 
  summarize(female_date = min(Date)) 

flower_male_pilot <- flower_pilot %>%
  filter(MaleOpen_perc == 100) %>%
  group_by(Experiment, Variety) %>% 
  summarize(male_date = min(Date)) 

### Grain Harvest
for (experiment in dates$Experiment[1:2]){
  plot_varieties <- plots %>%
    filter(Experiment == experiment)
  grain_file <- str_subset(list.files("data/"), paste0(experiment, ".*GrainHarvest.*"))
  grain_trial <- read_csv(paste0("data/", grain_file)) %>%
    left_join(plot_varieties, by = c("Block", "Row", "Column"))
  if (exists("grain_data")){
    grain_data <- bind_rows(grain_data, grain_trial)
  } else {
    grain_data <- grain_trial
  }
}

grain_pilot <- grain_data %>%
  filter(Variety %in% planting_date_varieties$Variety) %>%
  mutate(grain_harvest_lbsac = GrainDryWeight_g*8.92, # per g/m to lbs/ac
         grain_fresh_lbsac = GrainFreshWeight_g*8.92) 

grain_summary <- grain_data %>%
  group_by(Variety, Experiment) %>% 
  mutate(grain_harvest_lbsac = GrainDryWeight_g*8.92,  
         grain_fresh_lbsac = GrainFreshWeight_g*8.92) %>% 
  summarize(avg_grain_harvest = mean(grain_harvest_lbsac, na.rm=TRUE),
            sd_grain_harvest = sd(grain_harvest_lbsac, na.rm=TRUE),
            avg_grain_fresh = mean(grain_fresh_lbsac, na.rm=TRUE),
            sd_grain_fresh = sd(grain_fresh_lbsac, na.rm=TRUE)) %>% 
  mutate(PlantingDate = factor(Experiment, 
                               levels = c("PilotPlot1", "VarietyTrial"),
                               labels = c("May-1", "May-21")))
        

ggplot(grain_summary, aes(x=PlantingDate, y=avg_grain_fresh)) +
  geom_bar(stat = "identity") +
  labs(x = "Planting Date", y = "Grain Fresh Wgt [lbs/ac + SD]") +
  geom_errorbar(aes(ymin = avg_grain_fresh, ymax = avg_grain_fresh+sd_grain_fresh)) +
  facet_grid(.~Variety) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Fiber Harvest
for (experiment in dates$Experiment[1:2]){
  plot_varieties <- plots %>%
    filter(Experiment == experiment)
  fiber_file <- str_subset(list.files("data/"), paste0(experiment, ".*FiberHarvest_GM.*"))
  fiber_trial <- read_csv(paste0("data/", fiber_file)) %>%
    left_join(plot_varieties, by = c("Block", "Row", "Column"))
  if (exists("fiber_data")){
    fiber_data <- bind_rows(fiber_data, fiber_trial)
  } else {
    fiber_data <- fiber_trial
  }
}

fiber_dry_down <- fiber_data %>%
  filter(!is.na(SampleDryWeight_g)) %>% 
  mutate(dry_down_ratio = SampleDryWeight_g/SampleFreshWeight_g) %>% 
  summarize(dry_down = mean(dry_down_ratio))

fiber_pilot <- fiber_data %>%
  filter(Variety %in% planting_date_varieties$Variety) %>%
  mutate(fiber_fresh_lbsac = FreshWeight_kg*8921) # kg/m to lbs/ac

fiber_summary <- fiber_data %>%
  group_by(Variety, Experiment) %>% 
  mutate(fiber_fresh_lbsac = FreshWeight_kg*8921) %>% 
  summarize(avg_fiber_fresh = mean(fiber_fresh_lbsac, na.rm=TRUE),
            sd_fiber_fresh = sd(fiber_fresh_lbsac, na.rm=TRUE),
            avg_fiber_dry = avg_fiber_fresh*fiber_dry_down$dry_down,
            sd_fiber_dry = sd_fiber_fresh*fiber_dry_down$dry_down) %>% 
  mutate(PlantingDate = factor(Experiment, 
                               levels = c("PilotPlot1", "VarietyTrial"),
                               labels = c("May-1", "May-21")))


ggplot(fiber_summary, aes(x=PlantingDate, y=avg_fiber_dry)) +
  geom_bar(stat = "identity") +
  labs(x = "Planting Date", y = "Fiber Dry Wgt [lbs/ac + SD]") +
  geom_errorbar(aes(ymin = avg_fiber_dry, ymax = avg_fiber_dry+sd_fiber_dry)) +
  facet_grid(.~Variety) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
