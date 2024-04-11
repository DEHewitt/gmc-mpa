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
library(broom)       # model summary

# load custom functions
source("R/plot_defaults.R")

# load the data
data <- read_excel("data_raw/GMC_MARINEPARKSURVEYDATA_MASTER_211109.xlsx", sheet = 3)

# tidy the names
data <- data %>% clean_names()

# reduce this df a bit
data <- data %>% select(-est_code, -period_code)

# rename some variables
data <- data %>% rename(species = sp_code)

# only keep mud crabs
data <- data %>% filter(species == "SCYSER")

# adjust date stuff
data <- data %>%  mutate(year = (paste0("20", str_sub(date, 1, 2))))

# make site names readable
data <- data %>% 
  mutate(site_name = case_when(site_name == "TB" ~ "Taylors Beach",
                               site_name == "CB" ~ "Cromartys Bay",
                               site_name == "LSB" ~ "Little Swan Bay",
                               site_name == "LT" ~ "Lower Tilligerry",
                               site_name == "UT" ~ "Upper Tilligerry",
                               site_name == "LK" ~ "Lower Karuah",
                               site_name == "12MC" ~ "Twelve Mile Creek"))

# add a column identifying which 'season' of sampling it was
data <- data %>% mutate(season = case_when(year == 2019 | year == 2020 ~ "First",
                                           year == 2021 | year == 2022 ~ "Second"))

# rearrange columns
data <- data %>% select(zone, site_name, site, sample_code, year, season, cl_mm)

# get env data
env_data <- read_excel("data_raw/GMC_MARINEPARKSURVEYDATA_MASTER_211109.xlsx", sheet = 1)

# tidy up column names
env_data <- env_data %>% clean_names()

# get vars of interest
env_data <- env_data %>% select(sample_code, temp_c, cond_ms_cm)

# add to length data
data <- data %>% left_join(env_data)

# create some factors for model fitting
data <- data %>%
  mutate(zone = factor(zone),
         site_name = factor(site_name),
         season = factor(season))

# fit a model
m <- 
  gam(
    cl_mm ~
      # parametric effects
      zone +
      # smooth effects of temp and cond
      s(temp_c, bs = "tp") +
      s(cond_ms_cm, bs = "tp") +
      # nested random-effects
      #s(site_name, bs = "re") +
      #s(site_name, zone, bs = "re") +
      s(site_name, zone, season, bs = "re"),
    data = data,
    family = gaussian(),
    method = "REML",
    select = TRUE
  )

# set up plotting defaults
plot_defaults()

# extract residuals
resid <- appraise(m)

# plot them
resid

# save them
ggsave(filename = "figures/length-gamm-resid.png",
       width = 16,
       height = 16,
       units = "cm")

# check choice of k
gam.check(m)

# get a model summary
summary(m)

# estimated effects
effects <- m %>% visreg()

zone_effects <- effects[[1]][[1]]

zone_effects <- zone_effects %>% mutate(zone = case_when(zone == "REF" ~ "Fished", zone == "SZ" ~ "Unfished"))

zone_resid <- effects[[1]][[2]]

zone_resid <- zone_resid %>% mutate(zone = case_when(zone == "REF" ~ "Fished", zone == "SZ" ~ "Unfished"))

ggplot() +
  geom_point(data = zone_effects, aes(x = zone, y = visregFit)) +
  geom_point(data = zone_resid, aes(x = zone, y = visregRes), position = position_jitter(), alpha = 0.25) +
  geom_errorbar(data =  zone_effects, aes(x = zone, ymin = visregLwr, ymax = visregUpr), width = 0.75) + 
  geom_hline(yintercept = 85, linetype = "dashed", colour = "red") +
  ylab("Partial effect") +
  xlab("Zone") 

ggsave(filename = "figures/length-zone-effects.png",
       width = 12,
       height = 12,
       units = "cm")
                