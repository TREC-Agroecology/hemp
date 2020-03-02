library(tidyverse)
library(lubridate)
library(agricolae)

collect_data <- function(data_name, experiment_names = dates$Experiment, plot_info = plots){
  data_name_tag <- paste0(".*", data_name, ".*") 
  for (experiment in experiment_names){
    plot_varieties <- plot_info %>%
      filter(Experiment == experiment)
    data_file <- str_subset(list.files("data/"), paste0(experiment, data_name_tag))
    trial_data <- read_csv(paste0("data/", data_file)) %>%
      mutate(Date = ymd(paste(Year, Month, Day, sep = "-"))) %>%
      left_join(plot_varieties)
    if (exists("data_output")){
      data_output <- bind_rows(data_output, trial_data)
    } else {
      data_output <- trial_data
    }
  }
  return(data_output)
}

dates <- read_csv("data/Trials_Dates.csv") %>% 
  mutate(Experiment = factor(Experiment, levels = c("PilotPlot1", "VarietyTrial",
                                                    "PilotPlot2", "PilotPlotPlus8"))) %>% 
  arrange(Experiment)
plots <- read_csv("data/PlotVarietyFile.csv")

planting_date_varieties <- data.frame(
  Variety = c("Yuma-2", "Puma-3", "Bama", "BerryBlossom", "CherryBlossomxTI",
              "Eletta", "Tygra", "CarmagnolaSelezionata"),
  Rate_plot = c(900, 1500, 900, 60, 60, 1500, 900, 900),
  Rate_zone = c(161, 268, 161, 60, 60, 268, 161, 161),
  Region = c("China", "China", "China", "USA", "USA",
             "Europe", "Europe", "Europe")
)

### Emergence Data
emergence_data <- collect_data("Emergence")

avg_emergence <- emergence_data %>%
  group_by(Experiment, Variety, Date) %>% 
  summarize(avg_emergence = mean(Emergence, na.rm=TRUE)) %>% 
  left_join(select(planting_date_varieties, Variety, Rate_zone)) %>%
  mutate(percent_emergence = avg_emergence/Rate_zone*100)

emergence_pilot <- avg_emergence %>% 
  filter(Variety %in% planting_date_varieties$Variety) %>%
  ungroup() %>% 
  mutate(Variety = factor(Variety, levels = c("Yuma-2", "Puma-3", "Bama", "Eletta", "Tygra", "CarmagnolaSelezionata",
                                              "BerryBlossom", "CherryBlossomxTI")))

ggplot(emergence_pilot, aes(x=Date, y=avg_emergence, color=Variety)) +
  geom_line() +
  facet_grid(.~Experiment, scales = "free_x") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("output/emergence.png")


### Density Data
density_data <- collect_data("PlantDensity", dates$Experiment[1:2])

density_pilot <- density_data %>%
  filter(Variety %in% planting_date_varieties$Variety) %>% 
  mutate(Variety = factor(Variety, levels = c("Yuma-2", "Puma-3", "Bama", "Eletta", "Tygra", "CarmagnolaSelezionata",
                                              "BerryBlossom", "CherryBlossomxTI")))

ggplot(density_pilot, aes(x=Date, y=Quantity)) +
  geom_point() +
  facet_grid(Variety~Experiment, scales = "free_x") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

density_variety <- density_data %>%
  filter(Experiment == "VarietyTrial") %>% 
  mutate(Date = ymd(paste(Year, Month, Day, sep="-")))

ggplot(density_variety, aes(x=Date, y=Quantity)) +
  geom_point() +
  facet_wrap(~ Variety) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### Stand Count
stand_count_data <- collect_data("StandCount")

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
ggsave("output/stand_count.png")

stand_count_summary_variety <- stand_count_data %>%
  filter(Experiment == "VarietyTrial") %>% 
  group_by(Variety, Day) %>% 
  summarize(avg_stand_count = mean(StandCount, na.rm=TRUE),
            sd_stand_count = sd(StandCount, na.rm=TRUE))

### Flowering
flower_data <- collect_data("Flowering")

flower_pilot <- flower_data %>%
  filter(Variety %in% planting_date_varieties$Variety) %>%
  mutate(Date = ymd(paste(Year, Month, Day, sep="-")),
         PlantingDate = factor(Experiment, 
                               levels = c("PilotPlot1", "VarietyTrial", "PilotPlot2", "PilotPlotPlus8"),
                               labels = c("May-1", "May-21", "June-21", "July-18")),
         Variety = factor(Variety, levels = c("Yuma-2", "Puma-3", "Bama", "Eletta", "Tygra", "CarmagnolaSelezionata",
                                              "BerryBlossom", "CherryBlossomxTI")))

ggplot(flower_pilot, aes(x=Date, y=Induc_perc)) +
  geom_point() +
  facet_grid(Variety~PlantingDate, scales = "free_x") +
  labs(x = "Date", y = "Flower Induction [%]") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("output/flower_pilot.png")

flower_induc_pilot <- flower_pilot %>%
  filter(Induc_perc >= 50) %>%
  group_by(Experiment, Variety, Block) %>% 
  summarize(induc_date = min(Date)) %>%
  group_by(Experiment, Variety) %>%  
  summarize(induc_date = min(induc_date)) %>% 
  arrange(induc_date)
write_csv(flower_induc_pilot, "output/flower_induc_pilot.csv")

flower_variety <- flower_data %>%
  filter(Experiment == "VarietyTrial") %>% 
  mutate(Date = ymd(paste(Year, Month, Day, sep="-")))

ggplot(flower_variety, aes(x=Date, y=Induc_perc)) +
  geom_point() +
  facet_wrap(~ Variety) +
  labs(x = "Date", y = "Flower Induction [%]") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("output/flower_variety.png")

flower_induc_variety <- flower_variety %>% 
  filter(Induc_perc >=50) %>%
  group_by(Variety, Block) %>% 
  summarize(induc_date = min(Date)) %>%
  group_by(Variety) %>%  
  summarize(induc_date = min(induc_date)) %>% 
  arrange(induc_date)
write_csv(flower_induc_variety, "output/flower_induc_variety.csv")

flower_female_pilot <- flower_pilot %>%
  filter(FemaleOpen_perc == 100) %>%
  group_by(Experiment, Variety) %>% 
  summarize(female_date = min(Date)) 

flower_male_pilot <- flower_pilot %>%
  filter(MaleOpen_perc == 100) %>%
  group_by(Experiment, Variety) %>% 
  summarize(male_date = min(Date)) 

### Grain Harvest
grain_data <- collect_data("GrainHarvest", dates$Experiment[1:2])

grain_dry_down <- grain_data %>%
  filter(!is.na(GrainFreshWeight_g), !is.na(GrainDryWeight_g)) %>% 
  mutate(dry_down_ratio = GrainDryWeight_g/GrainFreshWeight_g) %>% 
  summarize(dry_down = mean(dry_down_ratio))

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
            avg_grain_dry = mean(grain_fresh_lbsac, na.rm=TRUE)*grain_dry_down$dry_down,
            sd_grain_dry = sd(grain_fresh_lbsac, na.rm=TRUE)*grain_dry_down$dry_down) %>% 
  mutate(PlantingDate = factor(Experiment, 
                               levels = c("PilotPlot1", "VarietyTrial"),
                               labels = c("May-1", "May-21")))
        
ggplot(grain_summary, aes(x=PlantingDate, y=avg_grain_dry)) +
  geom_bar(stat = "identity") +
  labs(x = "Planting Date", y = "Grain Dry Wgt [lbs/ac + SD]") +
  geom_errorbar(aes(ymin = avg_grain_dry, ymax = avg_grain_dry+sd_grain_dry)) +
  facet_grid(.~Variety) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("output/grain_summary.png")

### Fiber Harvest
fiber_data <- collect_data("FiberHarvest_GM", dates$Experiment[1:2])

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
ggsave("output/fiber_summary.png")
