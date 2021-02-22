### Compares yield per fertilizer treatment and remote
### imaging values canopy area and NDVI

library(tidyverse)
library(agricolae)

data <- read_csv("data/RemoteCanopyYield-BK10.csv") %>% 
  select(YieldMass, PlotNum, PntProduct, AppRate, CanopyArea, NDVI, GNDVI) %>% 
  mutate(AppRate_kgha = round(AppRate*1.12, 0),
         YieldMass_kg = YieldMass/1000,
         CanopyArea_m2 = CanopyArea*0.0929)

wife <- filter(data, PntProduct == "Wife" & YieldMass > 0)
wife_lost <- filter(data, PntProduct == "Wife" & YieldMass == 0)

wife_plot <- wife %>% 
  group_by(PlotNum, PntProduct, AppRate_kgha) %>% 
  summarize(avg_yield_p = mean(YieldMass_kg),
            ci_yield_p = 2*sd(YieldMass_kg)/sqrt(n()),
            avg_canopy_p = mean(CanopyArea_m2),
            ci_canopy_p = 2*sd(CanopyArea_m2)/sqrt(n()),
            avg_ndvi_p = mean(NDVI),
            ci_ndvi_p = 2*sd(NDVI)/sqrt(n()),
            avg_gndvi_p = mean(GNDVI),
            ci_gndvi_p = 2*sd(GNDVI)/sqrt(n()))

wife_rate <- wife_plot %>%
  group_by(AppRate_kgha) %>% 
  summarize(avg_yield = mean(avg_yield_p),
            ci_yield = 2*sd(avg_yield_p)/sqrt(n()),
            avg_canopy = mean(avg_canopy_p),
            ci_canopy = 2*sd(avg_canopy_p)/sqrt(n()),
            avg_ndvi = mean(avg_ndvi_p),
            ci_ndvi = 2*sd(avg_ndvi_p)/sqrt(n()),
            avg_gndvi = mean(avg_gndvi_p),
            ci_gndvi = 2*sd(avg_gndvi_p)/sqrt(n()))
  
# Yield

ggplot(wife_plot, aes(x=as.factor(AppRate_kgha), y=avg_yield_p)) +
  geom_point() +
  labs(x="N Application [kg/ha]", y="Plant Mass [kg]") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/yield-point.png")

test <- aov(avg_yield_p ~ AppRate_kgha, data=wife_plot)
print(summary(test))
print(duncan.test(test, "AppRate")$groups)

groupings <- data.frame(AppRate_kgha = c(0, 56, 112, 168, 224, 280),
                        letter = c("c", "c", "bc", "a", "a", "ab"))
wife_rate_yield <- left_join(wife_rate, groupings)

ggplot(wife_rate_yield, aes(x=as.factor(AppRate_kgha), y=avg_yield)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = avg_yield-ci_yield, 
                    ymax = avg_yield+ci_yield), width=0.2) +
  geom_text(aes(label = letter, y = avg_yield+ci_yield+0.05),
    position = position_dodge(0.9), vjust = 0) +
  labs(x="N Application [kg/ha]", y="Plant Mass [kg]") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/yield-bar.png")


# Canopy

test <- aov(avg_canopy_p ~ AppRate_kgha, data=wife_plot)
print(summary(test))
print(duncan.test(test, "AppRate")$groups)

groupings <- data.frame(AppRate_kgha = c(0, 56, 112, 168, 224, 280),
                        letter = c("b", "b", "a", "a", "a", "a"))
wife_rate_canopy <- left_join(wife_rate, groupings)

ggplot(wife_rate_canopy, aes(x=as.factor(AppRate_kgha), y=avg_canopy)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = avg_canopy-ci_canopy, 
                    ymax = avg_canopy+ci_canopy), width=0.2) +
  geom_text(aes(label = letter, y = avg_canopy+ci_canopy+0.025),
            position = position_dodge(0.9), vjust = 0) +
  labs(x="N Application [kg/ha]", y="Canopy Area [m2]") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/canopy-bar.png")


# NDVI

test <- aov(avg_ndvi_p ~ AppRate_kgha, data=wife_plot)
print(summary(test))
print(duncan.test(test, "AppRate_kgha")$groups)

groupings <- data.frame(AppRate_kgha = c(0, 56, 112, 168, 224, 280),
                        letter = c("c", "c", "b", "a", "a", "ab"))

wife_rate_ndvi <- left_join(wife_rate, groupings)

ggplot(wife_rate_ndvi, aes(x=as.factor(AppRate_kgha), y=avg_ndvi)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = avg_ndvi-ci_ndvi, 
                    ymax = avg_ndvi+ci_ndvi), width=0.2) +
  geom_text(aes(label = letter, y = avg_ndvi+ci_ndvi+0.025),
            position = position_dodge(0.9), vjust = 0) +
  labs(x="N Application [kg/ha]", y="NDVI") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/ndvi-bar.png")


# GNDVI

test <- aov(avg_gndvi_p ~ AppRate_kgha, data=wife_plot)
print(summary(test))
print(duncan.test(test, "AppRate")$groups)

groupings <- data.frame(AppRate_kgha = c(0, 56, 112, 168, 224, 280),
                        letter = c("b", "b", "b", "a", "a", "a"))

wife_rate_gndvi <- left_join(wife_rate, groupings)

ggplot(wife_rate_gndvi, aes(x=as.factor(AppRate_kgha), y=avg_gndvi)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = avg_gndvi-ci_gndvi, 
                    ymax = avg_gndvi+ci_gndvi), width=0.2) +
  geom_text(aes(label = letter, y = avg_gndvi+ci_gndvi+0.025),
            position = position_dodge(0.9), vjust = 0) +
  labs(x="N Application [kg/ha]", y="GNDVI") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/gndvi-bar.png")

# Yield ~ NDVI

ggplot(wife, aes(x=NDVI, y=log(YieldMass), color=as.factor(AppRate))) +
  geom_point()

test <- lm(log10(avg_yield_p)~avg_ndvi_p, data=wife_plot)
#test <- lm(avg_yield_p~avg_ndvi_p, data=wife_plot)
#test <- lm(log(YieldMass)~NDVI, data=wife)
summary(test)$r.squared # 0.864

ggplot(wife_plot, aes(x=avg_ndvi_p, y=log10(avg_yield_p))) +
  geom_point() +
  geom_abline(intercept = summary(test)$coeff[1,1],
              slope = summary(test)$coeff[2,1]) +
  labs(x="NDVI", y="Log10( Plant Mass [kg] )") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/yield-ndvi.png")

# Yield ~ GNDVI


test <- lm(log10(avg_yield_p)~avg_gndvi_p, data=wife_plot)
summary(test)$r.squared # 0.790

ggplot(wife_plot, aes(x=avg_gndvi_p, y=log10(avg_yield_p))) +
  geom_point() +
  geom_abline(intercept = summary(test)$coeff[1,1],
              slope = summary(test)$coeff[2,1]) +
  labs(x="GNDVI", y="Log10( Plant Mass [kg] )") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/yield-gndvi.png")


# Yield ~ Canopy

test <- lm(avg_yield_p~avg_canopy_p, data=wife_plot)
summary(test)$r.squared  # 0.780

ggplot(wife_plot, aes(x=avg_canopy_p, y=avg_yield_p)) +
  geom_point() +
  geom_abline(intercept = summary(test)$coeff[1,1],
         slope = summary(test)$coeff[2,1]) +
  labs(x="Canopy Area [m2]", y="Plant Mass [kg]") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/yield-canopy.png")


