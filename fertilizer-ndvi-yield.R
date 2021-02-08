### Compares yield per fertilizer treatment and remote
### imaging values canopy area and NDVI

library(tidyverse)
library(agricolae)

data <- read_csv("data/RemoteCanopyYield-BK10.csv") %>% 
  select(YieldMass, PlotNum, PntProduct, AppRate, CanopyArea, NDVI)

wife <- filter(data, PntProduct == "Wife" & YieldMass > 0)
wife_lost <- filter(data, PntProduct == "Wife" & YieldMass == 0)

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

ggplot(wife_plot, aes(x=AppRate, y=avg_yield_p)) +
  geom_point() +
  labs(x="N Application [lbs/acre]", y="Yield Mass [g]") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/yield-point.png")

ggplot(wife_rate, aes(x=AppRate, y=avg_yield)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = avg_yield-ci_yield, 
                    ymax = avg_yield+ci_yield), width=0.2) +
  labs(x="N Application [lbs/acre]", y="Yield Mass [g]") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/yield-bar.png")

test <- aov(avg_yield_p ~ AppRate, data=wife_plot)
print(summary(test))
print(duncan.test(test, "AppRate")$groups)

# Canopy

ggplot(wife_plot, aes(x=AppRate, y=avg_canopy_p)) +
  geom_point() +
  labs(x="N Application [lbs/acre]", y="Canopy Area [?]") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/canopy-point.png")

ggplot(wife_rate, aes(x=AppRate, y=avg_canopy)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = avg_canopy-ci_canopy, 
                    ymax = avg_canopy+ci_canopy), width=0.2) +
  labs(x="N Application [lbs/acre]", y="Canopy Area [?]") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/canopy-bar.png")

test <- aov(avg_canopy_p ~ AppRate, data=wife_plot)
print(summary(test))
print(duncan.test(test, "AppRate")$groups)

# NDVI

ggplot(wife_plot, aes(x=AppRate, y=avg_ndvi_p)) +
  geom_point() +
  labs(x="N Application [lbs/acre]", y="NDVI") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/ndvi-point.png")

ggplot(wife_rate, aes(x=AppRate, y=avg_ndvi)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = avg_ndvi-ci_ndvi, 
                    ymax = avg_ndvi+ci_ndvi), width=0.2) +
  labs(x="N Application [lbs/acre]", y="NDVI") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/ndvi-bar.png")

test <- aov(avg_ndvi_p ~ AppRate, data=wife_plot)
print(summary(test))
print(duncan.test(test, "AppRate")$groups)

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
  labs(x="NDVI", y="Log10( Yield [g] )") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/yield-ndvi.png")



# Yield ~ Canopy

ggplot(wife, aes(x=CanopyArea, y=YieldMass, color=as.factor(AppRate))) +
  geom_point()

test <- lm(avg_yield_p~avg_canopy_p, data=wife_plot)
#test <- lm(avg_yield_p~avg_ndvi_p, data=wife_plot)
#test <- lm(log(YieldMass)~NDVI, data=wife)
summary(test)$r.squared  # 0.780

ggplot(wife_plot, aes(x=avg_canopy_p, y=avg_yield_p)) +
  geom_point() +
  geom_abline(intercept = summary(test)$coeff[1,1],
         slope = summary(test)$coeff[2,1]) +
  labs(x="Canopy Area [?]", y="Yield [g]") +
  theme_classic(base_size = 12, base_family = "Helvetica")
ggsave("output/yield-canopy.png")


