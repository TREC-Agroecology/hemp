### Compares yield per fertilizer treatment and remote
### imaging values canopy area and NDVI

library(tidyverse)
library(agricolae)

data <- read_csv("data/RemoteCanopyYield-BK10.csv") %>% 
  select(YieldMass, PlotNum, PntProduct, AppRate, CanopyArea, NDVI)

wife <- filter(data, PntProduct == "Maverick")

wife_plot <- wife %>% 
  group_by(PlotNum, PntProduct, AppRate) %>% 
  summarize(avg_yield_p = mean(YieldMass),
            ci_yield_p = 2*sd(YieldMass)/sqrt(n()),
            avg_canopy_p = mean(CanopyArea),
            ci_canopy_p = 2*sd(CanopyArea)/sqrt(n()),
            avg_ndvi_p = mean(NDVI),
            ci_ndvi_p = 2*sd(NDVI)/sqrt(n()))

wife_rate <- wife_plot %>%
  group_by(AppRate) %>% 
  summarize(avg_yield = mean(avg_yield_p),
            ci_yield = 2*sd(avg_yield_p)/sqrt(n()),
            avg_canopy = mean(avg_canopy_p),
            ci_canopy = 2*sd(avg_canopy_p)/sqrt(n()),
            avg_ndvi = mean(avg_ndvi_p),
            ci_ndvi = 2*sd(avg_ndvi_p)/sqrt(n()))
  
# Yield

ggplot(wife, aes(x=AppRate, y=YieldMass)) +
  geom_point()

ggplot(wife_plot, aes(x=AppRate, y=avg_yield_p)) +
  geom_point()

# Canopy

ggplot(wife, aes(x=AppRate, y=CanopyArea)) +
  geom_point()

ggplot(wife_plot, aes(x=AppRate, y=avg_canopy_p)) +
  geom_point()

# NDVI

ggplot(wife, aes(x=AppRate, y=NDVI)) +
  geom_point()

ggplot(wife_plot, aes(x=AppRate, y=avg_ndvi_p)) +
  geom_point()

# Yield ~ NDVI

ggplot(wife_plot, aes(x=avg_ndvi_p, y=avg_yield_p, color=AppRate)) +
  geom_point()

# Yield ~ Canopy

ggplot(wife_plot, aes(x=avg_canopy_p, y=avg_yield_p, color=AppRate)) +
  geom_point()
