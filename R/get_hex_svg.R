# Based on a concept by \url{http://www.masalmon.eu/2018/02/22/hexcombine/}
# and \url{http://www.masalmon.eu/2018/01/07/rainbowing/}
library(jsonlite)
library(magrittr)
library(dplyr)
library(tidyr)
library(wrapr)
library(magick)
library(purrr)

hex_names <- qc(rstudio, dplyr, tidyr, ggplot2, rmarkdown, pipe)

hex_uri <- paste0("http://hexb.in/meta/", hex_names, ".json")

pages <- list()
for(i in 1:length(hex_uri)) {
  hex_data <- fromJSON(txt = hex_uri[i])
  pages[[i]] <- c(hex_data$vector, hex_data$raster)
}

hex_df <- pages %>% 
  c(., stringsAsFactors = FALSE) %>% 
  do.call(what = rbind.data.frame, args = .) %>% 
  as_tibble() %>% 
  setNames(c("SVG", "PNG"))

read_append <- . %>% 
  magick::image_read() %>% 
  magick::image_append()

svg_images <- purrr::map(hex_df$SVG, read_append)

svg_info <- purrr::map(.x = svg_images, .f = image_info) %>% 
  c(., stringsAsFactors = FALSE) %>% 
  do.call(what = rbind.data.frame, args = .) %>% 
  as_tibble()

svg_info

# png_images <- purrr::map(hex_df$PNG, read_append)
# 
# png_info <- purrr::map(.x = png_images, .f = image_info) %>%
#   c(., stringsAsFactors = FALSE) %>%
#   do.call(what = rbind.data.frame, args = .) %>%
#   as_tibble()

# magick::image_resize(svg_images[[1]], geometry = geometry_size_pixels(width = 735, height = 852)) %>% 
#   image_trim(., fuzz = 5) %>% 
#   image_background(color = "transparent") %>% 
#   image_convert(image = ., format = "png")

# svg_images[[2]] %>% 
#   image_fill(image = ., color = "pink", fuzz = 10, geometry_point(1, 1)) %>% 
#   image_background(image = ., color = "pink") %>% 
#   magick::image_write(image = ., path = "img/dplyr.svg")

hex_images <- list()
for(i in 1:length(svg_images)) {
  hex_images[[i]] <-  svg_images[[i]] %>% 
    image_trim(image = ., fuzz = 1) %>% 
    #image_resize(image = ., geometry = geometry_size_pixels(width = 735, height = 852, preserve_aspect = TRUE)) %>% 
    #image_convert(image = ., format = "png") %>% 
    image_fill(image = ., color = "pink", fuzz = 10, geometry_point(1, 1)) %>% 
    #image_colorize(image = ., opacity = 0, color = "pink") %>% 
    image_background(image = ., color = "pink") %>% 
    image_resize(image = ., geometry = geometry_size_pixels(width = 735, height = 852, preserve_aspect = TRUE))# %>% 
    #image_convert(image = ., format = "png", depth = 8)
}

hex_info <- hex_images %>% 
  purrr::map(.x = ., .f = image_info) %>%
  c(., stringsAsFactors = FALSE) %>%
  do.call(what = rbind.data.frame, args = .) %>%
  as_tibble()

hex_info

no_row <- 6
no_col <- 2

cell_width <- max(hex_info$width)
cell_height <- max(hex_info$height)

my_laptop <- magick::image_blank(width = cell_width * no_col,
                                 height = cell_height * no_row * 0.9,
                                 col = "#CCCCCC")

for(i in 1:no_row) {
  if(i/2 == floor(i/2)) {
    offset1 <- 0
  } else {
    offset1 <- cell_width/2
  }
  
  offset2 <- (i-1)*(cell_height*0.75)
  
  my_laptop <- image_composite(my_laptop, hex_images[[i]],
                               offset = paste0("+", offset1,
                                               "+", offset2))
}

my_laptop
