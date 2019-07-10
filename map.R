###################################
####  Map of AMP-AD locations  ####
###################################

## Based on Kenny's code:
## https://gist.github.com/kdaily/77993a19a90cc0d34c4b5799230f854f

library("maps")
library("mapdata")
library("ggplot2")
library("ggrepel")
library("readr")
library("dplyr")

coors <- read_csv("map_locations.csv")
global <- map_data("state")

coors <- coors %>%
  mutate(
    location = case_when(
      location == "Rush Alzheimer's Disease Center" ~ "Rush Alzheimer's\nDisease Center",
      TRUE ~ location
    )
  )

global <- global %>%
  mutate(highlight = ifelse(region %in% coors$state, "yes", "no"))

gg1 <- ggplot(global) +
  geom_polygon(
    aes(x = long, y = lat, group = group, fill = highlight, alpha = highlight),
    color = "white"
  ) +
  coord_map() +
  scale_fill_manual(values = c("grey90", "#5a498f")) +
  scale_alpha_manual(values = c(1, .3)) +
  geom_point(data = coors, aes(long, lat), colour = "#e567a1", size = 1) +
  geom_text_repel(
    data = coors,
    aes(long, lat, label = location),
    size = 3.5,
    force = 1.2
  ) +
  theme_void() +
  theme(legend.position = "none")

## gg1

ggsave(gg1, filename = "amp-ad_updated_map_20190710.svg", width = 10, height = 8)
