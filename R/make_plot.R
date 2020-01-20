

library(png)
library(glue)
library(grid)
library(ggmap)
library(magick)
library(trackeR)
library(magrittr)
library(tidyverse)

source("R/load_key.R")
source("R/get_center.R")

race_yr   <- '2019' 
race_cty  <- 'Chattanooga'
race_st   <- 'TN'
race_type <- 'half'

race_file <- glue("{race_yr}_{race_cty}_{race_st}_{race_type}") 
race_df   <- as_tibble(readTCX(file = glue("data/{race_file}.tcx")))

center_lat <- get_center(race_df$latitude)
center_lon <- get_center(race_df$longitude)

# each map is different, and it takes some fiddling to find
# the write set of graphic parameters to make the city map
# look the way we want it. We keep each city's set of graphic
# parameters saved here so we can reproduce a figure if needed.

mp_pars <- switch(
  race_cty, 
  'Chattanooga' = list(size = c(640, 640), zoom = 14, scale = 2),
  'Orlando' = list(size = c(500, 500), zoom = 13, scale = 2),
  list(size = c(500, 500), zoom = 13, scale = 2)
)

mp <- get_googlemap(
  center  = c(lon = center_lon, lat = center_lat),
  size    = mp_pars$size,
  zoom    = mp_pars$zoom,
  scale   = mp_pars$scale,
  maptype = 'terrain'
)

print_numeric <- function(x){
  x %>% 
    round(digits = 2) %>% 
    format(nsmall = 2)
}

total_time <- difftime(max(race_df$time), min(race_df$time), units = 'hours')
total_dist <- max(race_df$distance) * 0.000621371
avg_pace <- total_dist / as.numeric(total_time)

title <- glue(
  "Amanda's run in {race_cty}, {race_st}"
)

# "Distance: {print_numeric(total_dist)} miles \n",
# "Time: {print_numeric(total_time)} \n",
# "Pace: {print_numeric(avg_pace)} miles per hour"

p <- ggmap(mp) +
  geom_path(
    aes(x = longitude, y = latitude), 
    data = race_df, 
    size = 1,
    col = 'red'
  ) + 
  theme_void()

p

race_df[1,]

ggsave(glue("plots/{race_file}.png"), plot = p, device = 'png',
  width = 8, height = 8, units = 'in', dpi = 600)

img <- image_read(glue("img/{race_file}.png")) %>% 
  image_scale(geometry = c('x300'))

plt <- image_read(glue("plots/{race_file}.png")) %>% 
  image_scale(geometry = c('x300'))

final_plot <- image_append(c(img, plt))

image_write(final_plot, path = glue('output/{race_file}.png'))



