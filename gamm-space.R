# load the libraries
library(tidyverse)   # data manipulation
library(ggplot2)     # plotting
library(patchwork)   # putting together multi panel plots
library(lubridate)   # for dates/times
library(mgcv)        # model fitting
library(gratia)      # plotting/diagnostics of mgcv objects
library(janitor)     # tidying up column names
library(visreg)      # model visualisation
library(ecostats)    # model validation
library(scales)      # pretty labels
library(raster)      # for calculating ud areas

# custom functions
source("R/plot_defaults.R")

# plotting defaults
plot_defaults()
cond_label <- bquote(paste("Conductivity (mS cm"^-1, ")"))
temp_label <- "Water temperature (°C)"

# load the data
data <- read_csv("output/crab-uds.csv")

data <- data %>% filter(area95 < 50000)

# make a summary table
tagging_summary <- data %>%
  group_by(crab, sex, cl_mm) %>%
  summarise(detections = sum(n),
            tracks = n_distinct(track),
            n_mean = round(mean(n)),
            n_sd = round(sd(n)),
            n_min = min(n),
            n_max = max(n),
            start = min(date(start)),
            end = max(date(end)),
            mean_duration = round(mean(duration), 2),
            sd_duration = round(sd(duration), 2),
            total_duration = sum(duration),
            area95_mean = round(mean(area95)),
            area95_sd = round(sd(area95)),
            area95_min = min(area95),
            area95_max = max(area95),
            area50_mean = round(mean(area50)),
            area50_sd = round(sd(area50)),
            area50_min = min(area50),
            area50_max = max(area50),
            ratio_mean = round(mean(ratio), 3),
            ratio_sd = round(sd(ratio), 2),
            ratio_min = min(ratio),
            ratio_max = max(ratio),
            mean_of_mean_interval = round(mean(mean_interval), 2),
            sd_interval = round(sd(mean_interval), 2),
            mean_temp = round(mean(temp), 2),
            sd_temp = round(sd(temp), 2),
            temp_min = min(temp),
            temp_max = max(temp),
            mean_cond = round(mean(cond), 2),
            sd_cond = round(sd(cond), 2),
            cond_min = min(cond),
            cond_max = max(cond))

write_csv(x = tagging_summary, file = "output/tagging-summary.csv")

# make crab a factor for random
data <- data %>% mutate(crab = factor(crab), track = factor(track))

# ratio plot
ggplot() + 
  geom_point(data = data,
              aes(x = area95,
                  y = area50,
                  colour = crab)) +
  xlab(expression(paste("Area within 95% UD (m"^2,")"))) +
  ylab(expression(paste("Area within 50% UD (m"^2,")"))) +
  scale_x_continuous(labels = scales::label_comma(), trans = "log10") +
  scale_y_continuous(labels = scales::label_comma(), trans = "log10") +
  scale_colour_viridis_d() +
  theme(legend.position = "none")

ggsave(filename = "figures/ratio-plot.png",
       width = 12,
       height = 12,
       units = "cm")
  

# fit the model
m <- 
  gam(
    area95 ~
      s(temp, bs = "tp") +
      s(cond, bs = "tp") +
      s(duration, bs = "tp") +
      s(crab, bs = "re"),
    data = data,
    family = nb(),
    method = "REML",
    select = TRUE
  )


# look at residuals
appraise(m) # good

# save this plot
ggsave(filename = "figures/space95-gamm-resid.png",
       width = 16,
       height = 16,
       units = "cm")

# check k is high enough
gam.check(m)

# look at model summary
summary(m)

var_comp <- variance_comp(m) %>% mutate_at(.vars = c("variance", "lower_ci", "upper_ci"), round, 2)

# look at the effects
draw(m)

# Get out the estimated effects
effects_95 <- m %>% visreg(scale = "response")
temp_95_effects <- effects_95[[1]][[1]]
cond_95_effects <- effects_95[[2]][[1]]
duration_95_effects <- effects_95[[3]][[1]]

# fit the model
m <- 
  gam(
    area50 ~
      s(temp, bs = "tp") +
      s(cond, bs = "tp") +
      s(duration, bs = "tp") +
      s(crab, bs = "re"),
    data = data,
    family = nb(),
    method = "REML",
    select = TRUE
  )

# check
appraise(m)

# save this plot
ggsave(filename = "figures/space50-gamm-resid.png",
       width = 16,
       height = 16,
       units = "cm")

# check k is high enough
gam.check(m)

# look at model summary
summary(m)

var_comp <- variance_comp(m)

# look at the effects
draw(m)

# Get estimated effects
effects_50 <- m %>% visreg(scale = "response")
temp_50_effects <- effects_50[[1]][[1]]
cond_50_effects <- effects_50[[2]][[1]]
duration_50_effects <- effects_50[[3]][[1]]

# Plot the effects for the 50 % UD
# Conductivity
a <- ggplot() +
  geom_ribbon(data = cond_50_effects, 
              aes(x = cond, 
                  ymin = visregLwr, 
                  ymax = visregUpr), 
              fill = "#FDE725FF", 
              alpha = 0.25) +
  geom_rug(data = data, aes(x = cond)) +
  geom_line(data = cond_50_effects, 
            aes(y = visregFit, 
                x = cond), 
            colour = "#FDE725FF") +
  ylab(expression(paste("Area (m"^-2, ")"))) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 500))

# Temperature
b <- ggplot() +
  geom_ribbon(data = temp_50_effects, 
              aes(x = temp, 
                  ymin = visregLwr, 
                  ymax = visregUpr), 
                  fill = "#FDE725FF", 
              alpha = 0.25) +
  geom_rug(data = data, aes(x = temp)) +
  geom_line(data = temp_50_effects, 
            aes(y = visregFit, 
                x = temp), 
                colour = "#FDE725FF") +
  ylab(expression(paste("Area (m"^-2, ")"))) +
  theme(legend.position = "none") +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 500))

# Tracking duration
c <- ggplot() +
  geom_ribbon(data = duration_50_effects, 
              aes(x = duration, 
                  ymin = visregLwr, 
                  ymax = visregUpr), 
              fill = "#FDE725FF", 
              alpha = 0.25) +
  geom_rug(data = data, aes(x = duration)) +
  geom_line(data = duration_50_effects, 
            aes(y = visregFit, 
                x = duration), 
            colour = "#FDE725FF") +
  ylab(expression(paste("Area (m"^-2, ")"))) +
  theme(legend.position = "none") +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 500))

# Plot the effects for the 95 % UD
# Conductivity
d <- ggplot() +
  geom_ribbon(data = cond_95_effects, 
              aes(x = cond, 
                  ymin = visregLwr, 
                  ymax = visregUpr), 
              fill = "#238A8DFF", 
              alpha = 0.25) +
  geom_rug(data = data, aes(x = cond)) +
  geom_line(data = cond_95_effects, 
            aes(y = visregFit, 
                x = cond), 
            colour = "#238A8DFF") +
  ylab(expression(paste("Area (m"^-2, ")"))) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000))

# Temperature
e <- ggplot() +
  geom_ribbon(data = temp_95_effects, 
              aes(x = temp, 
                  ymin = visregLwr, 
                  ymax = visregUpr), 
              fill = "#238A8DFF", 
              alpha = 0.25) +
  geom_rug(data = data, aes(x = temp)) +
  geom_line(data = temp_95_effects, 
            aes(y = visregFit, 
                x = temp), 
            colour = "#238A8DFF") +
  ylab(expression(paste("Area (m"^-2, ")"))) +
  theme(legend.position = "none") +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000))

# Tracking duration
f <- ggplot() +
  geom_ribbon(data = duration_95_effects, 
              aes(x = duration, 
                  ymin = visregLwr, 
                  ymax = visregUpr), 
              fill = "#238A8DFF", 
              alpha = 0.25) +
  geom_rug(data = data, aes(x = duration)) +
  geom_line(data = duration_95_effects, 
            aes(y = visregFit, 
                x = duration), 
            colour = "#238A8DFF") +
  ylab(expression(paste("Area (m"^-2, ")"))) +
  theme(legend.position = "none") +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000))

# fit the ratio model
m <- 
  gam(
    ratio ~
      s(temp, bs = "tp") +
      s(cond, bs = "tp") +
      s(duration, bs = "tp") +
      s(crab, bs = "re"),
    data = data,
    family = Gamma(link = "log"),
    method = "REML",
    select = TRUE
  )

# check
appraise(m)

# save this plot
ggsave(filename = "figures/space-ratio-gamm-resid.png",
       width = 16,
       height = 16,
       units = "cm")

# check k is high enough
gam.check(m)

# look at model summary
summary(m)

var_comp <- variance_comp(m)

# look at the effects
draw(m)

# Get the estimates
effects_ratio <- m %>% visreg(scale = "response")
cond_effects <- effects_ratio[[2]][[1]]
temp_effects <- effects_ratio[[1]][[1]]
duration_effects <- effects_ratio[[3]][[1]]

# Plot them
# Conductivity
g <- ggplot(data = cond_effects, 
            aes(x = cond)) +
  geom_ribbon(aes(ymin = visregLwr, 
                  ymax = visregUpr), 
              fill = "light grey") +
  geom_rug(data = data, 
           aes(x = cond)) +
  geom_line(aes(y =  visregFit)) +
  xlab(cond_label) +
  ylab("50:95 % ratio") +
  scale_y_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05))

# Temperature
h <- ggplot(data = temp_effects, 
            aes(x = temp)) +
  geom_ribbon(aes(ymin = visregLwr, 
                  ymax = visregUpr), 
              fill = "light grey") +
  geom_rug(data = data, 
           aes(x = temp)) +
  geom_line(aes(y =  visregFit)) +
  xlab(temp_label) +
  ylab("50:95 % ratio") + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_y_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05))

# Tracking duration
i <- ggplot(data = duration_effects, 
            aes(x = duration)) +
  geom_ribbon(aes(ymin = visregLwr, 
                  ymax = visregUpr), 
              fill = "light grey") +
  geom_rug(data = data, 
           aes(x = duration)) +
  geom_line(aes(y =  visregFit)) +
  xlab("Tracking duration (h)") +
  ylab("50:95 % ratio") +
  scale_x_continuous(limits = c(2, 24), 
                     breaks = seq(0, 24, 4)) + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_y_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05))

((a | b | c) / (d | e | f) / (g | h | i)) + plot_annotation(tag_levels = "a")

ggsave(filename = "figures/combined-space-use-effects.png",
       width = 22,
       height = 22,
       units = "cm")

### overlap ###
# list all the dates
dates <- unique(date(data$start))

for(i in 1:length(dates)){
  ud95s <- list.files(path = "output/", pattern = paste0(dates[i], "-raster-95.rds"))
  ud50s <- list.files(path = "output/", pattern = paste0(dates[i], "-raster-50.rds"))
}

# list all the 95 ud files
ud95s <- list.files(path = "output/", pattern = "raster-95.rds")

# list all the 50 ud files
ud50s <- list.files(path = "output/", pattern = "raster-50.rds")



# fit a gam to this data of a similar form to the others
