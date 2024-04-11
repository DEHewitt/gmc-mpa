# load libraries
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(fy)

# load function
source("R/kde.compare.R")

# load length data
data <- read_excel("data_raw/GMC_MARINEPARKSURVEYDATA_MASTER_211109.xlsx", sheet = 3) %>% 
  clean_names() %>% 
  filter(sp_code == "SCYSER") %>% 
  dplyr::mutate(zone = if_else(zone == "SZ", "No-take", "Fished"),
                season = date2fy(ymd(date)),
                zone = factor(zone, levels = c("No-take", "Fished")))

# subset to just first year
data1 <- data %>% filter(season == "2019-20")

# name figure
fig_name1 <- "figures/kde_gmc_mpa.jpeg"

# save figure
jpeg(fig_name1, width = 4500, height = 4500/2, units = "px", res = 300, pointsize = 14) 

par(mfrow=c(1, 2))

# run analysis
kde.compare(length = data1$cl_mm, 
            group = data1$zone, 
            align = "no", 
            nboot = 500, 
            xlab = "Carapace length (mm)", 
            ylab = "Probability density", 
            main = "2019-20")

# subset to just second year
data2 <- data %>% filter(season == "2021-22")

# run analysis
kde.compare(length = data2$cl_mm, 
            group = data2$zone, 
            align = "no", 
            nboot = 500, 
            xlab = "Carapace length (mm)", 
            ylab = "Probability density", 
            main = "2021-22")

dev.off()
