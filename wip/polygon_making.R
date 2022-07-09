# This script is a stream of consciousness working on this project

library(dplyr)
library(ggplot2)
library(retistruct) # to get intersection

set.seed(45)

# method 1 - geom_segment ----

# for each cube, it is either 7 or 9 segments
# need a x for the center and left, right sides
# then calc all the segments from there based on vp

source("functions/make_new_cube.R")

xes <- c(1, 2, 3) #left, center, right
yes <- c(4, 6) #bottom, top of center

# can create any n of cubes
cube <- list()
for (n in 1:3) {
  cube[[n]] <- make_new_cube(xes = sort(sample(0:9, 3)),
                             yes = sort(sample(-10:10, 2)),
                             vp_scale = 30)
}
cube <- dplyr::bind_rows(cube)

# use ggplot to plot the cubes ----
cube %>%
  ggplot() +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  xlim(c(0,10)) +
  ylim(c(-10,10)) +
  theme_void()

# method 2 - geom_polygon ----
# this is now using a "field" from (0,0) to (100,100)
source("functions/make_new_poly.R")
poly <- make_new_poly(xes = c(20,40,80),
                      yes = c(60,80),
                      horizon_y = 20) # mess with different horizons

# add a background poly
background_poly <- data.frame(cube_id = "sky",
                  x = c(0, 0, 100, 100),
                  y = c(0, 100, 100, 0),
                  id = "sky",
                  value = "sky")
# plot the poly
poly %>%
  ggplot() +
  geom_polygon(alpha = 1, aes(x = x, y = y, group = id),
               fill = "gray90",
               data = background_poly, inherit.aes = FALSE) +
  geom_polygon(aes(x = x, y = y, group = id, fill = value),
               alpha = 1) +
  theme_minimal() +
  theme(legend.position = "none")

# RUN IT -----
source("functions/save_poly_pdf.R")
source("functions/make_many_polys.R")

d <- make_many_polys("tests/simple_cubes3.png",
                   horizon_y = 80,
                   height_range = 10:40,
                   width_range = 20:80,
                   n_cubes = 3, n_second_color = 1)

save_poly_pdf(d = d, filename = "tests/simple_cubes3.pdf")

saveRDS(d, "tests/test_10.Rds")

# you can print with tiling using Adobe reader.
# open the PDF there.

# Or create blocks by splitting the area into n x n areas
# Then make those each their own page

# Below is the code - have not wrapped into a function.
# Because I would like to also incorporate smarter code to
# auto-do some FPP

missing_paths <- d %>% arrange(desc(cube_id))
scale_factor <- 40 / 100 # change if you change vp above
width_blocks <- 40 / 5   # how many blocks across (I wanted 8")

horizontal <- data.frame(cube_id = c("",""),
                         x = c(0, 100),
                         y = c(50, 50)) %>%
  mutate(x = x*scale_factor, y = y*scale_factor)

# design goes from x 0 - 40, y 0, 40
grid <- tidyr::crossing(x = seq(0, 32, width_blocks),
                        y = seq(0, 32, width_blocks)) %>%
  mutate(x_end = x + width_blocks,
         y_end = y + width_blocks) %>%
  arrange(x, y)

for (i in seq_len(nrow(grid))) {
  block <- d %>%
    bind_rows(missing_paths) %>%
    arrange(cube_id) %>%
    mutate(x = x*scale_factor, y = y*scale_factor) %>%
    group_by(id) %>%
    mutate(ave_x = mean(x), ave_y = mean(y)) %>%
    ungroup() %>%
    ggplot() +
    # horizon line
    geom_path(aes(x = x, y = y, group = cube_id),
              data = horizontal,
              alpha = 1,
              color = "black",
              size = 1) +
    geom_path(aes(x = x, y = y, group = id),
              alpha = 1,
              color = "black",
              size = 2) +
    geom_polygon(aes(x = x, y = y, group = id),
                 fill = "white", color = "blue",
                 size = 0.5,
                 alpha = 1) +
    geom_text(aes(label = cube_id, x = ave_x, y = ave_y)) +
    # xlim(c(0, quilt_size)) + ylim(c(-(quilt_size/2), (quilt_size/2))) +
    coord_equal(clip = "off",
                xlim = c(grid$x[i], grid$x_end[i]),
                ylim = c(grid$y[i], grid$y_end[i]),
                expand = FALSE) +
    theme_void() +
    # theme_minimal() +
    theme(legend.position = "none",
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  ggsave(plot = block,
         filename = paste0("to_print/",i,".pdf"),
         width = width_blocks, height = width_blocks)
}

# How much fabric? ------

# Added a function to estimate the amount of fabric
# see functions/calc_needed_fabric.R
source("functions/calc_needed_fabric.R")

calc_needed_fabric(design = d, scale_factor = 40/30, scale_extra = 1.2)
# for our first design, a quarter yard (usually you buy a fat quarter) is plenty!

# Can we pick fabric colors? -----
# grabbed hex codes for two collections of solid quilting fabric that I like.
# Let's pick from these to make the quilt and replot?
source("functions/fabric_color_guide.R")

