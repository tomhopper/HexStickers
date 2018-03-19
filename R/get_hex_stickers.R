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

local_files <- paste0("files/", basename(hex_df$SVG))

download.file(hex_df$SVG, destfile = local_files)

## Now Switch to Terminal ####
## Terminal commands can be run in RStudio from this script by selecting and pressing 
## Command-Option-Enter (Mac) or ctrl+alt+Enter (Windows or Mac).
## Check if ImageMagick is installed and set up to use the rsvg library with:
## convert -list format | grep SVG
## results should look like:
##      MSVG  SVG       rw+   ImageMagick's own SVG internal renderer
##       SVG  SVG       rw+   Scalable Vector Graphics (RSVG 2.42.2)
##      SVGZ  SVG       rw+   Compressed Scalable Vector Graphics (RSVG 2.42.2)
## Note the "RSVG" at the end of the two lines.
## If not, reconfigure ImageMagick.
## With Mac Ports:
## sudo port install imagemagick +rsvg
## With Brewer, ImageMagick already installed:
## brew reinstall imagemagick --with-libsrvg librsvg --use-rsvg
## With Brewer, ImageMagick *not* installed:
## brew install imagemagick --use-rsvg
## Navigate to the /files/ directory in this project
## cd files
## Run the following to convert all SVG files to PNG with transparent background:
## mogrify -background none -resize 735x852 -format png *.svg
## The resulting png files will have about 490 pixels per inch, which should be
## high enough for printing to stickers.

read_append <- . %>% 
  magick::image_read() %>% 
  magick::image_trim() %>% 
  magick::image_background(image = ., color = "transparent") %>% 
  magick::image_append()

png_images <- purrr::map(local_files, read_append)

png_info <- purrr::map(.x = png_images, .f = image_info) %>%
  c(., stringsAsFactors = FALSE) %>%
  do.call(what = rbind.data.frame, args = .) %>%
  as_tibble()

png_info

no_row <- 6
no_col <- 2

cell_width <- max(png_info$width)
cell_height <- max(png_info$height)

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
  
  my_laptop <- image_composite(my_laptop, png_images[[i]],
                               offset = paste0("+", offset1,
                                               "+", offset2))
}

my_laptop
