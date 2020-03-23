### REMINDER. Set working directory.

library(tidyverse)
library(lubridate)
library(agricolae)

collect_data <- function(data_name, experiment_names = dates$Experiment, plot_info = plots){
  data_name_tag <- paste0(".*", data_name, ".csv") 
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

variety_levels <- c("Yuma-2", "Puma-3", "Bama", "Eletta", "Tygra", "CarmagnolaSelezionata",
                    "BerryBlossom", "CherryBlossomxTI")
variety_labels <- c("Yuma-2", "Puma-3", "Bama", "Eletta", "Tygra", "Carmag. Sel.",
                    "BerryBlossom", "CherryBlos. xTI")

experiment_levels <- c("PilotPlot1", "VarietyTrial", "PilotPlot2", "PilotPlotPlus8")
experiment_labels <- c("May-01", "May-22", "June-21", "July-18")
experiment_labels_full <- c("May-01-2019", "May-22-2019", "June-21-2019", "July-18-2019")

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
  mutate(Variety = factor(Variety, variety_levels, variety_labels))

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
  mutate(Variety = factor(Variety, variety_levels, variety_labels))

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
  mutate(Variety = factor(Variety, variety_levels, variety_labels),
         PlantingDate = factor(Experiment, experiment_levels, experiment_labels)) %>%
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

#### Pilot flowering
flower_pilot <- flower_data %>%
  filter(Variety %in% planting_date_varieties$Variety) %>%
  mutate(Date = ymd(paste(Year, Month, Day, sep="-")),
         PlantingDate = factor(Experiment, experiment_levels, experiment_labels_full),
         Variety = factor(Variety, variety_levels, variety_labels))

flower_induc_pilot_blocks_10 <- flower_pilot %>%
  filter(Induc_perc >= 10) %>%
  mutate(day_interval = Date - mdy(PlantingDate)) %>% 
  group_by(PlantingDate, Variety, Block) %>% 
  summarize(induc_interval = min(day_interval))

flower_induc_pilot_10 <- flower_induc_pilot_blocks_10 %>% 
  group_by(PlantingDate, Variety) %>%  
  summarize(avg_induc_interval_10 = mean(induc_interval),
            induc_date_10 = min(mdy(PlantingDate)) + days(round(avg_induc_interval_10, 0)),
            sd_induc_interval_10 = sd(as.numeric(induc_interval))) %>% 
  arrange(induc_date_10)

flower_induc_pilot_blocks_50 <- flower_pilot %>%
  filter(Induc_perc >= 50) %>%
  mutate(day_interval = Date - mdy(PlantingDate)) %>% 
  group_by(PlantingDate, Variety, Block) %>% 
  summarize(induc_interval = min(day_interval))

flower_induc_pilot_50 <- flower_induc_pilot_blocks_50 %>% 
  group_by(PlantingDate, Variety) %>%  
  summarize(avg_induc_interval_50 = mean(induc_interval),
            induc_date_50 = min(mdy(PlantingDate)) + days(round(avg_induc_interval_50, 0)),
            sd_induc_interval_50 = sd(as.numeric(induc_interval))) %>% 
  arrange(induc_date_50)

flower_induc_pilot <- left_join(flower_induc_pilot_10, flower_induc_pilot_50)
write_csv(flower_induc_pilot, "output/flower_induc_pilot.csv")

ggplot(flower_pilot, aes(x=Date, y=Induc_perc)) +
  geom_point() +
  geom_vline(data=flower_induc_pilot, aes(xintercept = induc_date), linetype="dashed") +
  facet_grid(Variety~PlantingDate) +  # scales = "free_x"
  labs(x = "Date", y = "Flower Induction [%]") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("output/flower_pilot.png")

#### Variety flowering
flower_variety <- flower_data %>%
  filter(Experiment == "VarietyTrial") %>% 
  mutate(Date = ymd(paste(Year, Month, Day, sep="-")))

ggplot(flower_variety, aes(x=Date, y=Induc_perc)) +
  geom_point() +
  facet_wrap(~Variety) +
  labs(x = "Date", y = "Flower Induction [%]") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("output/flower_variety.png")

flower_induc_variety_blocks_10 <- flower_variety %>% 
  filter(Induc_perc >=10) %>%
  mutate(day_interval = Date - ymd("2019-May-22")) %>% 
  group_by(Latitude, Variety, Block, Row) %>% 
  summarize(induc_interval = min(day_interval)) 

flower_induc_variety_10 <- flower_induc_variety_blocks_10 %>%
  group_by(Latitude, Variety) %>%  
  summarize(avg_induc_interval_10 = mean(induc_interval),
            induc_date_10 = ymd("2019-May-22") + days(round(avg_induc_interval_10, 0)),
            sd_induc_interval_10 = sd(as.numeric(induc_interval))) %>% 
  arrange(induc_date_10)

flower_induc_variety_blocks_50 <- flower_variety %>% 
  filter(Induc_perc >=50) %>%
  mutate(day_interval = Date - ymd("2019-May-22")) %>% 
  group_by(Latitude, Variety, Block, Row) %>% 
  summarize(induc_interval = min(day_interval)) 

flower_induc_variety_50 <- flower_induc_variety_blocks_50 %>%
  group_by(Latitude, Variety) %>%  
  summarize(avg_induc_interval_50 = mean(induc_interval),
            induc_date_50 = ymd("2019-May-22") + days(round(avg_induc_interval_50, 0)),
            sd_induc_interval_50 = sd(as.numeric(induc_interval))) %>% 
  arrange(induc_date_50)

flower_induc_variety <- left_join(flower_induc_variety_10, flower_induc_variety_50)
write_csv(flower_induc_variety, "output/flower_induc_variety.csv")

ggplot(flower_variety, aes(x=Date, y=Induc_perc)) +
  geom_point() +
  geom_vline(data=flower_induc_variety, aes(xintercept = induc_date_50), 
             linetype="dashed", color="darkgrey") +
  facet_wrap(~Latitude+Variety, dir="v") +
  labs(x = "Date", y = "Flower Induction [%]") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("output/flower_variety_class.png")

flower_induc_class <- flower_induc_variety %>%
  group_by(Latitude) %>%  
  summarize(Avg_induc_interval = mean(avg_induc_interval_50),
            Induc_date = ymd("2019-May-22") + days(round(Avg_induc_interval, 0)),
            Sd_induc_interval = sd(as.numeric(avg_induc_interval_50))) %>% 
  arrange(Induc_date)
write_csv(flower_induc_variety, "output/flower_induc_class.csv")

ggplot(flower_variety, aes(x=Date, y=Induc_perc, shape = as.ordered(Latitude))) +
  geom_point() +
  geom_vline(data=flower_induc_class, aes(xintercept = Induc_date), linetype="dotted") +
  labs(x = "Date", y = "Flower Induction [%]", shape = "Latitutde") +
  scale_shape_manual(values = c(9,7,1,5,0,2)) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("output/flower_class.png")

flower_female_pilot <- flower_pilot %>%
  filter(FemaleOpen_perc == 100) %>%
  group_by(Experiment, Variety) %>% 
  summarize(female_date = min(Date)) 

flower_male_pilot <- flower_pilot %>%
  filter(MaleOpen_perc == 100) %>%
  group_by(Experiment, Variety) %>% 
  summarize(male_date = min(Date)) 

### Height
height_data <- collect_data("PlantHeight-SexRatio")
canopy_data <- collect_data("PlantHeight", "VarietyTrial")

height_pilot <- height_data %>%
  filter(Variety %in% planting_date_varieties$Variety) %>%
  mutate(PlantingDate = factor(Experiment, experiment_levels, experiment_labels_full),
         Variety = factor(Variety, variety_levels, variety_labels)) %>% 
  group_by(PlantingDate, Variety, Date) %>% 
  summarize(mean_height = mean(Height_cm, na.rm=TRUE))

max_height_pilot <- height_pilot %>% 
  group_by(PlantingDate, Variety) %>% 
  summarize(max_height = round(max(mean_height, na.rm=TRUE), 3))

height_variety <- canopy_data %>%
  group_by(Variety, Date) %>% 
  summarize(mean_height = mean(Height_cm, na.rm=TRUE))

max_height_variety <- height_variety %>% 
  group_by(Variety) %>% 
  summarize(max_height = round(max(mean_height, na.rm=TRUE), 3))

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
  ungroup() %>% 
  mutate(PlantingDate = factor(Experiment, experiment_levels, experiment_labels),
         Variety = str_replace_all(Variety, "CarmagnolaSelezionata", "Carmag. Sel."))
        
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
  mutate(PlantingDate = factor(Experiment, experiment_levels, experiment_labels))

ggplot(fiber_summary, aes(x=PlantingDate, y=avg_fiber_dry)) +
  geom_bar(stat = "identity") +
  labs(x = "Planting Date", y = "Fiber Dry Wgt [lbs/ac + SD]") +
  geom_errorbar(aes(ymin = avg_fiber_dry, ymax = avg_fiber_dry+sd_fiber_dry)) +
  facet_grid(.~Variety) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("output/fiber_summary.png")

### Summary Table

plots_summary <- plots %>%
  filter(Variety %in% planting_date_varieties$Variety) %>% 
  mutate(Variety = factor(Variety, variety_levels, variety_labels)) %>% 
  distinct(Variety, Latitude)
    
pilot_summary <- left_join(flower_induc_pilot_50, max_height_pilot) %>% 
  select(PlantingDate, Variety, FloweringTime = induc_date_50, MaxHeight = max_height) %>% 
  left_join(plots_summary)
write_csv(pilot_summary, "output/flowering_height_summary_pilot.csv")

variety_summary <- left_join(flower_induc_variety_50, max_height_variety) %>% 
  select(Variety, Latitude, FloweringTime = induc_date_50, MaxHeight = max_height)
write_csv(variety_summary, "output/flowering_height_summary_variety.csv")
