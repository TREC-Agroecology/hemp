### REMINDER. Set working directory.

library(brms)
library(tidyverse)
library(lubridate)
library(segmented)
            
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


### Data
flower_data <- collect_data("Flowering")

#### Pilot
flower_pilot <- flower_data %>%
  filter(Variety %in% planting_date_varieties$Variety) %>%
  mutate(Date = ymd(paste(Year, Month, Day, sep="-")),
         PlantingDate = factor(Experiment, experiment_levels, experiment_labels_full),
         Variety = factor(Variety, variety_levels, variety_labels),
         days_after = Date - mdy(PlantingDate))

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
#write_csv(flower_induc_pilot, "output/flower_induc_pilot.csv")

ggplot(flower_pilot, aes(x=Date, y=Induc_perc)) +
  geom_point() +
  geom_vline(data=flower_induc_pilot, aes(xintercept = induc_date_50), linetype="dashed") +
  facet_grid(Variety~PlantingDate) +  # scales = "free_x"
  labs(x = "Date", y = "Flower Induction [%]") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#ggsave("output/flower_pilot.png")


#### Variety
latitude_variety <- plots %>% 
  filter(Experiment == "VarietyTrial") %>%
  select(latitude = Latitude, variety = Variety) %>% 
  distinct(latitude, variety) %>% 
  arrange(latitude)

flower_variety <- flower_data %>%
  filter(Experiment == "VarietyTrial") %>% 
  mutate(Date = ymd(paste(Year, Month, Day, sep="-")),
         days_after = as.numeric(Date - mdy("May-21-2019")))

ggplot(flower_variety, aes(x=Date, y=Induc_perc)) +
  geom_point() +
  facet_wrap(~Variety) +
  labs(x = "Date", y = "Flower Induction [%]") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#ggsave("output/flower_variety.png")

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
#write_csv(flower_induc_variety, "output/flower_induc_variety.csv")

ggplot(flower_variety, aes(x=Date, y=Induc_perc)) +
  geom_point() +
  geom_vline(data=flower_induc_variety, aes(xintercept = induc_date_50), 
             linetype="dashed", color="darkgrey") +
  facet_wrap(~Latitude+Variety, dir="v") +
  labs(x = "Date", y = "Flower Induction [%]") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#ggsave("output/flower_variety_class.png")

flower_induc_class <- flower_induc_variety %>%
  group_by(Latitude) %>%  
  summarize(Avg_induc_interval = mean(avg_induc_interval_50),
            Induc_date = ymd("2019-May-22") + days(round(Avg_induc_interval, 0)),
            Sd_induc_interval = sd(as.numeric(avg_induc_interval_50))) %>% 
  arrange(Induc_date)
#write_csv(flower_induc_variety, "output/flower_induc_class.csv")

ggplot(flower_variety, aes(x=Date, y=Induc_perc, shape = as.ordered(Latitude))) +
  geom_point() +
  geom_vline(data=flower_induc_class, aes(xintercept = Induc_date), linetype="dotted") +
  labs(x = "Date", y = "Flower Induction [%]", shape = "Latitutde") +
  scale_shape_manual(values = c(9,7,1,5,0,2)) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#ggsave("output/flower_class.png")

flower_female_pilot <- flower_pilot %>%
  filter(FemaleOpen_perc == 100) %>%
  group_by(Experiment, Variety) %>% 
  summarize(female_date = min(Date)) 

flower_male_pilot <- flower_pilot %>%
  filter(MaleOpen_perc == 100) %>%
  group_by(Experiment, Variety) %>% 
  summarize(male_date = min(Date)) 

### Models

lat_50 <- flower_variety %>% 
  filter(Latitude == 50)

lat_50_varieties <- distinct(lat_50, Variety)

not_50 <- flower_variety %>% 
  filter(Latitude < 50)

not_50_varieties <- distinct(not_50, Variety)

segment_1_r <- flower_variety %>%  
  split(.$Variety) %>% 
  map(~ segmented(lm(Induc_perc ~ days_after, data = .))) %>% 
  map(summary) %>% 
  map_dbl("r.squared")

segment_2_r <- not_50 %>% 
  split(.$Variety) %>% 
  map(~ segmented(lm(Induc_perc ~ days_after, data = .), npsi=2)) %>% 
  map(summary) %>% 
  map_dbl("r.squared")

seg_results <- bind_rows(enframe(segment_1_r), enframe(segment_2_r))

segment_1_psi <- flower_variety  %>%
  split(.$Variety) %>% 
  map(~ segmented(lm(Induc_perc ~ days_after, data = .))) %>% 
  map(summary) %>% 
  transpose() %>% 
  pluck("psi")

segment_2_psi <- not_50 %>% 
  split(.$Variety) %>% 
  map(~ segmented(lm(Induc_perc ~ days_after, data = .), npsi=2)) %>% 
  map(summary) %>% 
  transpose() %>% 
  pluck("psi")

segment_1_m <- flower_variety %>%
  split(.$Variety) %>% 
  map(~slope(segmented(lm(Induc_perc ~ days_after, data = .))))

segment_1_b <- flower_variety %>%
  split(.$Variety) %>% 
  map(~intercept(segmented(lm(Induc_perc ~ days_after, data = .))))

segment_2_m <- not_50 %>%
  split(.$Variety) %>% 
  map(~slope(segmented(lm(Induc_perc ~ days_after, data = .), npsi=2)))

segment_2_b <- not_50 %>%
  split(.$Variety) %>% 
  map(~intercept(segmented(lm(Induc_perc ~ days_after, data = .), npsi=2)))

seg_output <- data.frame(variety = c(), r_squared = c(),
                           breakpoint = c(), breakpoint_se = c(), 
                           slope = c(), slope_se =  c(), intercept = c())

all_output <- data.frame()
for ( variety in unique(flower_variety$Variety) ) {
  r_squared <- round(segment_1_r[[variety]], 3)
  breakpoint_1 <- round(segment_1_psi[[variety]][[2]],3)
  breakpoint_1_se <- round(segment_1_psi[[variety]][[3]],3)
  slope_1 <- round(segment_1_m[[variety]][[1]][1, 1],3)
  slope_1_se <- round(segment_1_m[[variety]][[1]][1, 2],3)
  slope_2 <- round(segment_1_m[[variety]][[1]][2, 1],3)
  slope_2_se <- round(segment_1_m[[variety]][[1]][2, 2],3)
  intercept_1 <- round(segment_1_b[[variety]][[1]][1],3)
  intercept_2 <- round(segment_1_b[[variety]][[1]][2],3)
  variety_output <- data.frame(variety, r_squared, breakpoint_1,  
                      slope_1,  slope_2, intercept_1, intercept_2,
                      breakpoint_1_se, slope_1_se, slope_2_se)
  all_output <- bind_rows(all_output, variety_output)
}

not_50_output <- data.frame()
for ( variety in not_50_varieties$Variety ) {
  r_squared <- round(segment_2_r[[variety]], 3)
  breakpoint_1 <- round(segment_2_psi[[variety]][1, 2],3)
  breakpoint_1_se <- round(segment_2_psi[[variety]][1, 3],3)
  breakpoint_2 <- round(segment_2_psi[[variety]][2, 2],3)
  breakpoint_2_se <- round(segment_2_psi[[variety]][2, 3],3)
  slope_1 <- round(segment_2_m[[variety]][[1]][1, 1],3)
  slope_1_se <- round(segment_2_m[[variety]][[1]][1, 2],3)
  slope_2 <- round(segment_2_m[[variety]][[1]][2, 1],3)
  slope_2_se <- round(segment_2_m[[variety]][[1]][2, 2],3)
  slope_3 <- round(segment_2_m[[variety]][[1]][3, 1],3)
  slope_3_se <- round(segment_2_m[[variety]][[1]][3, 2],3)
  intercept_1 <- round(segment_2_b[[variety]][[1]][1],3)
  intercept_2 <- round(segment_2_b[[variety]][[1]][2],3)
  intercept_3 <- round(segment_2_b[[variety]][[1]][3],3)
  variety_output <- data.frame(variety, r_squared, breakpoint_1, breakpoint_2, 
                               slope_1, slope_2,  slope_3,  
                               intercept_1, intercept_2, intercept_3,
                               breakpoint_1_se, breakpoint_2_se, slope_1_se,
                               slope_2_se, slope_3_se)
  not_50_output <- bind_rows(not_50_output, variety_output)
}

seg_output <- bind_rows(not_50_output, all_output)
seg_output <- left_join(latitude_variety, seg_output) %>% 
  arrange(latitude, variety)
write_csv(seg_output, "output/flowering_segment_analysis.csv")

seg_sweep <- read_csv("output/flowering_segment_sweep.csv") %>% 
  mutate(y_start = slope*x_start + intercept,
         y_end = slope*(x_end-x_start) + y_start,
         Variety = as.factor(variety),
         Latitude = as.factor(latitude))

ggplot(flower_variety, aes(x=days_after, y=Induc_perc)) +
  geom_point() +
  geom_segment(data = seg_sweep, 
               aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               linetype="dashed", color="darkgrey") +
  facet_wrap(~Latitude+Variety, dir="v") +
  labs(x = "Days After Planting", y = "Flower Induction [%]") +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### OTHER

bb_sub <- flower_variety %>% 
  filter(Variety == "Fibranova", # PlantingDate == "May-22-2019",
         !is.na(Induc_perc))

### nls
sigmoid_fit <- nls(Induc_perc ~ b/(1 + exp(-c * (days_after - d))),
                   data = bb_sub,
                   start = list(b = 101, c = 1, d = 50))
exponent_fit <- nls(Induc_perc ~ a * (exp(k * days_after)), 
                    data = bb_sub,
                    start = list(a = 0.1, k = 0.1))
michaelis_fit <- nls(Induc_perc ~ (a * days_after) / (b + days_after), 
                     data = bb_sub,
                     start = list(a = 10, b = 0.1))


### brms

#### Tutorial
b <- c(2, 0.75)
x <- rnorm(100)
y <- rnorm(100, mean = b[1] * exp(b[2] * x))
dat1 <- data.frame(x, y)

prior1 <- prior(normal(1, 2), nlpar = "b1") +
  prior(normal(0, 2), nlpar = "b2")
fit1 <- brm(bf(y ~ b1 * exp(b2 * x), b1 + b2 ~ 1, nl = TRUE),
            data = dat1, prior = prior1)
summary(fit1)
plot(fit1)
plot(conditional_effects(fit1), points = TRUE)

#### Data
prior_sig <- prior(normal(101, 2), nlpar = "b") +
  prior(normal(1, 1), nlpar = "c") +
  prior(normal(50, 2), nlpar = "d")
fit_sig <- brm(bf(Induc_perc ~ b/(1 + exp(-c * (days_after - d))),
                  b ~ 1, c ~ 1, d ~ 1, nl = TRUE),
               data = bb_sub, prior = prior_sig)
plot(conditional_effects(fit_sig), points = TRUE)

prior_exp <- prior(normal(0, 0.1), nlpar = "a") +
  prior(normal(1, 1), nlpar = "k")
fit_exp <- brm(bf(Induc_perc ~ a * exp(k * (days_after)),
                  a ~ 1, k ~ 1, nl = TRUE),
               data = bb_sub, prior = prior_exp)
plot(conditional_effects(fit_exp), points = TRUE)

prior_mm <- prior(normal(101, 0.1), nlpar = "a") +
  prior(normal(1, 0.1), nlpar = "b")
fit_mm <- brm(bf(Induc_perc ~ (a * days_after) / (b + days_after),
                 a ~ 1, b ~ 1, nl = TRUE),
              data = bb_sub, prior = prior_mm)
plot(conditional_effects(fit_mm), points = TRUE)

prior_asym <- prior(normal(101, 1), nlpar = "a") +
  prior(normal(10, 1), nlpar = "b") + prior(normal(0.1, 0.1), nlpar = "c")
fit_asym <- brm(bf(Induc_perc ~ a - (a - b) * exp(-c * days_after),
                   a ~ 1, b ~ 1, c ~ 1, nl = TRUE),
                data = bb_sub, prior = prior_asym)
plot(conditional_effects(fit_asym), points = TRUE)