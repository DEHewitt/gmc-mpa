# Script to make a site map for GMC MPA Survey

# load libraries
library(tidyverse)
library(ggplot2)
library(sf)
library(rgdal)
library(rnaturalearth)
library(shadowtext)
library(ggspatial)

# For mapping
nsw_coast <- st_as_sf(readOGR(dsn = "data_raw/Coastaline_WGS84_MGA56_NSWOEH.shp")) %>% mutate(water = if_else(code_coast == 1000, "water", "land")) %>% filter(water != "water")
mpa_zoning <- st_as_sf(readOGR(dsn = "data_raw/NSW_Marine_Protected_Areas.shp"))
site_locations <- st_as_sf(readOGR(dsn = "data_raw/XYsites.shp"))
st_crs(site_locations) <- 4326
australia <- ne_states(country = "australia", returnclass = "sf")
habitat <- st_as_sf(readOGR(dsn = "data_raw/NSW_Estuarine_Macrophytes.shp"))

aus_map <- ggplot() +
  geom_sf(data = australia) +
  coord_sf(xlim = c(113, 153), ylim = c(-45, -10)) +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA), 
        panel.background = element_rect(colour = "white")) +
  geom_point(aes(x = 152.191397, y = -32.708031))

# Port Stephens inset
#port_stephens <- ggplot() +
# geom_sf(data = nsw_coast, aes(fill = water)) +
#coord_sf(xlim = c(151.9, 152.2), ylim = c(-32.8, -32.6)) +
#scale_fill_manual(values = c("light grey", "white")) +
#theme_bw() +
#theme(legend.position = "none",
#     axis.text = element_blank(),
#    axis.ticks = element_blank(),
#   axis.title = element_blank()) +
#geom_rect(aes(xmin = 151.9, xmax = 152.075, ymin = -32.78, ymax = -32.625), colour = "black", fill = "white", alpha = 0.1)# +
#geom_text(aes(x = 151.9, y = -32.62, label = "a"), size = 5)

# Western Port Stephens (main figure) 
port_stephens <- ggplot() +
  geom_sf(data = mpa_zoning, aes(fill = ZONE_TYPE), colour = NA) +
  geom_sf(data = nsw_coast, aes(fill = water)) +
  scale_fill_manual(breaks = c("General Use Zone (IUCN VI)", 
                               "Habitat Protection Zone (IUCN IV)", 
                               "Habitat Restriction Zone (Restrictions Apply) (IUCN IV)", 
                               "land",
                               "Mangrove",
                               "Saltmarsh",
                               "Sanctuary Zone (IUCN II)",
                               "Seagrass",
                               "Special Purpose Zone (IUCN VI)"),
                    values = c("white", 
                               "white", 
                               "white", 
                               "bisque1",
                               "dark green",
                               "orange",
                               "light grey",
                               "light green",
                               "light grey")) +
  geom_sf(data = habitat, aes(fill = Habitat), colour = NA, alpha = 0.5) +
  geom_sf(data = site_locations, aes(shape = site_code)) +
  geom_shadowtext(data = site_locations, aes(x = long, y = lat + 0.004, label = site_name), colour = "black", bg.colour = "white") +
  coord_sf(xlim = c(151.9, 152.2), ylim = c(-32.78, -32.625)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(size = 12, colour = "black")) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_custom(grob = ggplotGrob(aus_map), xmin = 152.15, xmax = 152.2, ymin = -32.675, ymax = -32.625)

ggsave("figures/site_map.jpeg", plot = port_stephens, device = "jpeg", width = 20, height = 15, units = "cm", dpi = 300)
