# load in packages
library(tidyverse)
library(rgdal)
library(ggplot2)
library(lubridate)
library(sf)
library(move)
library(ggspatial)
library(viridis)

# load custom functions
source("R/load_data.R") # custom function that loads in relevant data
source("R/lost_tags.R") # function to remove lost tags
source("R/create_tracks.R") # split paths up into tracks based on some time cutoff
source("R/time_diff.R")
source("R/few_detections.R") # remove tracks with too few detections
source("R/vps_error.R") # function to calculate error in the VPS
source("R/save_object.R")
source("R/find_wq.R")

# load in the data
load_data()

if(Sys.info()[6] == "Dan"){
  oz <- st_as_sf(readOGR(dsn = "data_raw/clipped_port_stephens.shp"))
  mpa_zoning <- st_as_sf(readOGR(dsn = "data_raw/NSW_Marine_Protected_Areas.shp"))
} else {
  oz <- st_as_sf(readOGR(dsn = "data/clipped_port_stephens.shp"))
  mpa_zoning <- st_as_sf(readOGR(dsn = "data/NSW_Marine_Protected_Areas.shp"))
}

# select the columns and rename them
crab.data <- raw.data %>% dplyr::select(ID = FullId, time = Time, lon = Longitude, lat = Latitude)

# remove the lost tags
# determined this by going back over scanned datasheets and some plotting of the data (these two didn't move)
crab.data <- crab.data %>% lost_tags(tags = c("A69-9006-7799", "A69-9006-7806"))

# split the paths up into daily tracks
crab.data <- crab.data %>% mutate(date = date(time))

# give it a numeric value
crab.data <- crab.data %>% group_by(ID, date) %>% mutate(group = cur_group_id()) %>% ungroup()

# create a column that identifies each crab
crab.data <- crab.data %>% mutate(crab = str_sub(ID, 1, 13))

# calculate time difference between detections
crab.data <- crab.data %>% time_diff()

# get environmental data
crab.data <- crab.data %>% find_wq(wq.data)

# project utm coordinates
coords <- data.frame(lon = crab.data$lon, lat = crab.data$lat)
llcoord <- SpatialPoints(coords[,1:2], proj4string = CRS("+proj=longlat +datum=WGS84"))
utmcoord <- spTransform(llcoord, CRS("+proj=utm +zone=56 +datum=WGS84"))

# add UTM locations to data frame
crab.data$x <- attr(utmcoord, "coords")[,1]
crab.data$y <- attr(utmcoord, "coords")[,2]

# set window.size and margin
# guess at the time interval behavioural changes will occur
# Hewitt et al. (2023) showed influence of tide so 6 hours
t_change <- 6*60 # in minutes

# use median difference between detections to approximate this
window.size <- t_change/crab.data$time.diff %>% median() %>% round()

# ensure it is an odd number
if(window.size %% 2 == 0){
  window.size <- window.size-1
}

window.size <- 31

# margin 
margin <- (window.size/6) %>% round()

margin <- 11

# must be odd
if(margin %% 2 == 0){
  margin <- margin + 1
}

# remove any tracks with too few detections
crab.data <- crab.data %>% few_detections(n = window.size, group = group)

# how much error is their in the sync/ref tag data
vps.error <- syncref.data %>% vps_error(type = "both", which = "ref", time = crab)

# error of the system
location.error <- vps.error$HPEm %>% median()

# raster resolution for use in dbbmm
raster <- round(location.error)

# make sure raster resolution is greater than the error
if(raster < location.error){
  raster <- raster+1
}


tagging_summary <- crab.data %>%
  group_by(crab) %>%
  summarise(mean_interval = round(mean(time.diff), 2),
            sd_interval = round(sd(time.diff), 2))


# list all crabs
IDs <- crab.data$group %>% unique()

IDs <- c(IDs[1:91], IDs[93:326])
  
crab_uds <- NULL

for(i in 1:length(IDs)){
  # reduce down to one track at a time
  crab <- crab.data %>% filter(group == IDs[i])
  
  # convert to a move object
  crab.move <- move(
    x = crab$x,
    y = crab$y,
    time = as.POSIXct(crab$time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    proj = CRS("+proj=utm +zone=56 +datum=WGS84"),
    data = crab,
    animal = crab$crab
  )
  
  # calculate the dbbmm
  temp.dbbmm <- brownian.bridge.dyn(
    crab.move, 
    raster = raster, # picked based on error during the track times, ensured that it is greater than the error
    location.error = location.error,
    window.size = window.size,
    margin = margin,
    ext = 30
  )
  
  # for labelling
  crab.id <- crab$crab %>% unique()
  date <- crab$date %>% unique()
  
  # extract the ud
  volume.ud <- getVolumeUD(temp.dbbmm)
  
  # 95% ud
  ud95 <- volume.ud<=.95
  
  saveRDS(ud95, file = paste0("output/", crab.id, "-", IDs[i], "-", date, "-raster-95.rds"))
  
  # 50% ud
  ud50 <- volume.ud<=.5
  
  saveRDS(ud95, file = paste0("output/", crab.id, "-", IDs[i], "-", date, "-raster-50.rds"))
  
  # put this all together
  
  if(Sys.Date() == "1991-05-07"){
    track <- crab$group %>% unique()
    area95 <- sum(values(ud95)) * raster
    area50 <- sum(values(ud50)) * raster
    start <- crab$time %>% min()
    end <- crab$time %>% max()
    n <- crab %>% nrow() %>% as.numeric()
    duration <- as.numeric(end - start) # in hours
    temp <- crab$temp %>% mean()
    cond <- crab$cond %>% mean()
    
    bio.data1 <- bio.data %>% filter(FullId == crab.id)
    sex <- bio.data1$Sex
    cl_mm <- bio.data1$CL_mm
    
    median_interval <- median(crab$time.diff)
    iqr_interval <- IQR(crab$time.diff)
    mean_interval <- mean(crab$time.diff)
    sd_interval <- sd(crab$time.diff)
    
    temp <- data.frame(
      crab = crab.id,
      sex = sex,
      cl_mm = cl_mm,
      track = track,
      start = start, 
      end = end,
      duration = duration,
      n = n,
      median_interval = median_interval,
      iqr_interval = iqr_interval,
      mean_interval = mean_interval,
      sd_interval = sd_interval,
      area95 = area95,
      area50 = area50,
      ratio = area50/area95, # https://www.int-res.com/articles/meps2003/262/m262p253.pdf
      location.error = location.error,
      temp = temp,
      cond = cond
    )
    
    crab_uds <- crab_uds %>% bind_rows(temp)
    
    # convert to dataframe for ggplot
    ud95.df <- as.data.frame(ud95, xy = TRUE) %>% filter(layer == TRUE) %>% mutate(layer = "95 %")
    ud50.df <- as.data.frame(ud50, xy = TRUE) %>% filter(layer == TRUE) %>% mutate(layer = "50 %")
    
    ud.df <- bind_rows(ud95.df, ud50.df)#, ud75.df, ud25.df)
    
    oz <- oz %>% mutate(land = if_else(ID_NAME == "Water", "water", "Land")) %>% filter(land != "water")
    
    # set up plotting limits
    # need these to be either the limits of the points (crab df) or the uds
    crab_xmin <- min(crab.data$x)
    crab_xmax <- 411581.3
    crab_ymin <- min(crab.data$y)
    crab_ymax <- max(crab.data$y)
    ud_xmin <- min(ud.df$x)
    ud_xmax <- max(ud.df$x)
    ud_ymin <- min(ud.df$y)
    ud_ymax <- max(ud.df$y)
    
    xmin <- min(crab_xmin, ud_xmin)
    xmax <- max(crab_xmax, ud_xmax)
    ymin <- min(crab_ymin, ud_ymin)
    ymax <- max(crab_ymax, ud_ymax)
    
    ud <- ggplot() +
     geom_sf(data = oz, aes(fill = land)) +
    geom_sf(data = habitat, aes(fill = HABITAT)) +
    scale_fill_manual(values = c("#FDE725FF", "#238A8DFF", "bisque1", "dark green", "orange", "light green")) + #, "#55C667FF", "#404788FF"
    geom_tile(data = ud.df, 
             aes(x = x, y = y,
                fill = layer),
           colour = NA,
          alpha = 0.75) +
    geom_point(data = crab, aes(x = x, y = y), size = 0.5, alpha = 0.5) +
    geom_line(data = crab, aes(x = x, y = y), linewidth = 0.5, alpha = 0.5) +
    coord_sf(xlim = c(xmin, xmax),
            ylim = c(ymin, ymax),
           crs = "+proj=utm +zone=56 +datum=WGS84") +
    theme_bw() +
    theme(legend.title = element_blank(),
         legend.position = "bottom",
        axis.text = element_blank(),
       axis.title = element_blank(),
      axis.ticks = element_blank(),
     panel.grid = element_blank()) +
    annotation_scale()
    
    ggsave(filename = paste0("output/", crab.id, "-", IDs[i], "-ud.png"),
          device = "png",  
         width = 21, 
        height = 18, 
       units = "cm", 
      dpi = 600)
    
    if(i == length(IDs)){
      write_csv(crab_uds, paste0("output/crab-uds.csv"))
    }
  }
}
