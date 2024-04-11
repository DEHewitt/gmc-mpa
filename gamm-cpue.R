# load the libraries
library(tidyverse)   # data manioulation
library(readxl)      # import data straight from excel
library(ggplot2)     # plotting
library(patchwork)   # putting together multi panel plots
library(lubridate)   # for dates/times
library(mgcv)        # model fitting
library(gratia)      # plotting/diagnostics of mgcv objects
library(janitor)     # tidying up column names
library(visreg)      # model visualisation

# load custom functions
source("R/plot_defaults.R")

# load the data
data <- read_excel("data_raw/GMC_MARINEPARKSURVEYDATA_MASTER_211109.xlsx", sheet = 1)

# tidy the names
data <- data %>% clean_names()

# reduce this df a bit
data <- data %>% select(-est_code, -period_code, -start_wp)

# rename some variables
data <- data %>% rename(soak = depl_dur_decday, n_crabs = scyser)

# adjust date stuff
data <- data %>%  mutate(date = date(start_gps_time_aest), year = year(date), month = month(date))

# make site names readable
data <- data %>% 
  mutate(site_name = case_when(site_name == "TB" ~ "Taylors Beach",
                               site_name == "CB" ~ "Cromartys Bay",
                               site_name == "LSB" ~ "Little Swan Bay",
                               site_name == "LT" ~ "Lower Tilligerry",
                               site_name == "UT" ~ "Upper Tilligerry",
                               site_name == "LK" ~ "Lower Karuah",
                               site_name == "12MC" ~ "Twelve Mile Creek"))

# rearrange columns
data <- data %>% select(zone, site_name, site, sample_code, date, month, year, n_crabs, soak, temp_c, cond_ms_cm)

# add a column identifying which 'season' of sampling it was
data <- data %>% mutate(season = case_when(year == 2019 | year == 2020 ~ "First",
                                           year == 2021 | year == 2022 ~ "Second"))

# stolen traps
stolen <- c("191210.PST.REF.2.6", "191017.PST.REF.2.4", "200210.PST.SZ.1.4", "200210.PST.SZ.1.5", "200210.PST.REF.3.1", "200210.PST.REF.3.2", "211103.PST.SZ.2.4")

# remove them
data <- data %>% filter(!(sample_code %in% stolen))

# create some factors for model fitting
data <- data %>%
  mutate(zone = factor(zone),
         site_name = factor(site_name),
         season = factor(season))

# fit a model
m <- 
  gam(
    n_crabs ~
      # parametric effects
      zone +
      #season +
      # smooth effects of temp and cond
      s(temp_c, bs = "tp") +
      s(cond_ms_cm, bs = "tp") +
      # nested random-effects
      s(site_name, season, bs = "re") +
      offset(log(soak)),
    data = data,
    family = poisson(),
    method = "REML",
    select = TRUE
  )

# set up plotting defaults
plot_defaults()

# extract residuals
m %>% appraise()

# save them
ggsave(filename = "figures/cpue-gamm-residuals.png",
       height = 16,
       width = 16,
       units = "cm")

# construct a rootogram
m %>% rootogram() %>% draw()

# save it
ggsave(filename = "figures/cpue-gamm-rootogram.png",
       height = 16,
       width = 16,
       units = "cm")

# check choice of k
gam.check(m)

# get a model summary
summary(m)

model_table <- tidy(m)

var_comp <- variance_comp(m) %>% mutate_at(.vars = c(2:5), round, 2)

# estimated effects
effects <- m %>% visreg()

# zone effects
zone_effects <- effects[[1]][[1]] 

zone_resid <- effects[[1]][[2]]

zone_effects <- zone_effects %>% mutate(zone = case_when(zone == "REF" ~ "Fished", zone == "SZ" ~ "No-take"))

zone_resid <- zone_resid %>% mutate(zone = case_when(zone == "REF" ~ "Fished", zone == "SZ" ~ "No-take"))

# plot them
a <- ggplot() +
  geom_point(data = zone_resid, aes(x = zone, y = visregRes), position = position_jitter(), alpha = 0.25) +
  geom_point(data = zone_effects, aes(x = zone, y = visregFit)) +
  geom_errorbar(data = zone_effects, aes(x = zone, ymin = visregLwr, ymax = visregUpr), width = 0.75) +
  ylab("Partial effect") +
  xlab("Zone") 

# save them
#ggsave(filename = "figures/cpue-zone-effects.png",
 #      width = 12,
  #     height = 12,
   #    units = "cm")
  
# site effects
site_effects <- effects[[4]][[1]]

site_resid <- effects[[4]][[2]]

# encode zoning for plotting
site_effects <- site_effects %>% 
  mutate(zone = case_when(site_name == "Taylors Beach" ~ "No-take",
                          site_name == "Cromartys Bay" ~ "No-take",
                          site_name == "Twelve Mile Creek" ~ "No-take",
                          site_name == "Little Swan Bay" ~ "No-take",
                          TRUE ~ "Fished")) %>%
  mutate(site_name = factor(site_name, levels = c("Lower Karuah", "Lower Tilligerry", "Upper Tilligerry",
                                        "Cromartys Bay", "Little Swan Bay", "Taylors Beach", "Twelve Mile Creek")))

site_resid <- site_resid %>% 
  mutate(zone = case_when(site_name == "Taylors Beach" ~ "No-take",
                          site_name == "Cromartys Bay" ~ "No-take",
                          site_name == "Twelve Mile Creek" ~ "No-take",
                          site_name == "Little Swan Bay" ~ "No-take",
                          TRUE ~ "Fished")) %>% 
  mutate(site_name = factor(site_name, levels = c("Lower Karuah", "Lower Tilligerry", "Upper Tilligerry",
                                                  "Cromartys Bay", "Little Swan Bay", "Taylors Beach", "Twelve Mile Creek")))

b <- ggplot() +
  geom_point(data = site_resid, aes(x = site_name, y = visregRes, colour = zone), position = position_jitter(), alpha = 0.25) +
  geom_point(data = site_effects, aes(x = site_name, y = visregFit, colour = zone)) +
  geom_errorbar(data = site_effects, aes(x = site_name, ymin = visregLwr, ymax = visregUpr, colour = zone), width = 0.75) +
  ylab("Partial effect") +
  xlab("Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = "Dark2") +
  labs(colour = "Zone")

#ggsave(filename = "figures/cpue-site-effects.png",
 #      width = 12,
  #     height = 12,
   #    units = "cm")

cond_effects <- m %>% smooth_estimates(smooth = "s(cond_ms_cm)") %>% add_confint()

c <- ggplot(data = cond_effects, aes(x = cond_ms_cm)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "light grey") +
  geom_rug(data = data, aes(x = cond_ms_cm)) +
  geom_line(aes(y = est)) +
  xlab(bquote(paste("Conductivity (mS cm"^-1, ")"))) +
  ylab("Partial effect")

((a|c)/b) + plot_annotation(tag_levels = "a")

ggsave(filename = "figures/cpue-effects.png",
       width = 20,
       height = 20,
       units = "cm")
