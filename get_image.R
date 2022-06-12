# I would like to be able to take an image (png)
# And convert to a quilt pattern.
# Ideally, you could have different construction methods such as
# squares or FPP blocks.
# It could be a single block or a grid of n by m blocks

# load packages
library(dplyr)
library(ggplot2)
library(imager)

# Load an image:
# this loads as cimg object
my_image <- load.image("tests/R_logo.png")
plot(my_image)

# For what I a doing, I don't need a full size image
# Let's make it smaller and easier to work with
my_image <- imresize(my_image, 0.25)

# convert to a data.frame
# note that here transparent was black, but I made it white
image_to_df <- function(image) {
  as.data.frame(my_image, wide = "c") %>%
    dplyr::mutate(
      c.1 = ifelse(c.4 == 0, 1, c.1),
      c.2 = ifelse(c.4 == 0, 1, c.2),
      c.3 = ifelse(c.4 == 0, 1, c.3),
      rgb = rgb(c.1,c.2,c.3))
}

image_df <- image_to_df(my_image)

# this data.frame can be plotted with ggplot2
ggplot(image_df, aes(x, y)) + geom_raster(aes(fill = rgb)) +
  coord_equal() +
  scale_fill_identity() +
  scale_y_continuous(expand=c(0, 0), trans=scales::reverse_trans()) +
  theme(legend.position = "none")

# let's basically do a terrible compression.
# split the image into a grid of squares and average the colors

n_squares_x <- 9  # higher -> more pieces -> more like the image
n_squares_y <- 8

split_x_len <- max(image_df$x) / n_squares_x
split_x <- data.frame(x = seq_len(max(image_df$x)),
                      x_group = ceiling(seq_len(max(image_df$x)) / split_x_len))
split_y_len <- max(image_df$y) / n_squares_y
split_y <- data.frame(y = seq_len(max(image_df$y)),
                      y_group = ceiling(seq_len(max(image_df$y)) / split_y_len))
image_df <- image_df %>%
  left_join(split_x, by = "x") %>%
  left_join(split_y, by = "y")


image_reduced <- image_df %>%
  group_by(x_group, y_group) %>%
  summarise(across(c(c.1, c.2, c.3), ~sqrt(mean(.x^2))),
            .groups = "drop") %>%
  mutate(rgb = rgb(c.1, c.2, c.3))


ggplot(image_reduced, aes(x_group, y_group)) + geom_raster(aes(fill = rgb)) +
  coord_equal() +
  scale_fill_identity() +
  scale_y_continuous(expand=c(0,0), trans=scales::reverse_trans()) +
  theme(legend.position = "none")

# Now what about for an irregular shape to average over? Need to get all the pixels
# in a weird shape (please dont make me use shapefiles!)

library(secr) # lots of dependencies

# test a single polygon - get the points in that shape and average the color
new_poly <- data.frame(x = c(25,50,100,25), y = c(25,250,25,25)) #triangle
in_poly <- pointsInPolygon(image_df, new_poly) # slow

# confirm we selected the shape we created - good
ggplot(image_df[in_poly,], aes(x, y)) +
  geom_raster(aes(fill = rgb)) +
  coord_equal() +
  scale_fill_identity() +
  scale_y_continuous(expand=c(0,0), trans=scales::reverse_trans()) +
  theme(legend.position = "none")

sel_average <- image_df[in_poly,] %>%
  summarise(across(c(c.1,c.2,c.3), ~sqrt(mean(.x^2))),
            .groups = "drop") %>%
  mutate(rgb = rgb(c.1,c.2,c.3))

image_reduced_selection <- image_df
image_reduced_selection[in_poly, c("c.1","c.2","c.3","rgb")] <- sel_average

# confirm our reduction worked
ggplot(image_reduced_selection, aes(x, y)) +
  geom_raster(aes(fill = rgb)) +
  coord_equal() +
  scale_fill_identity() +
  scale_y_continuous(expand=c(0,0), trans=scales::reverse_trans()) +
  theme(legend.position = "none")

# Try with a design we make with the FPP generator
sapply(list.files("functions", pattern = "_fpp_", full.names = TRUE), source)
source("functions/get_point_on_line.R")

# get the image dimensions
dimx <- dim(my_image)[1]
dimy <- dim(my_image)[2]
design_init <- tribble(
  ~section, ~line, ~start,  ~stop,
  "A",      1,    c(0,  0), c(0, dimy),
  "A",      2,    c(0, dimy), c(dimx,dimy),
  "A",      3,    c(dimx,dimy), c(dimx, 0),
  "A",      4,    c(dimx, 0), c(0,  0)
)
test_design <- make_fpp_design(n = 5, design_init)
test_design$plot

# convert design to polygon format we can work with
new_design <- test_design$design %>%
  tidyr::unnest(c(start, stop)) %>%
  mutate(pos = rep(c("x", "y"), n()/2)) %>%
  tidyr::pivot_longer(cols = c(start, stop)) %>%
  tidyr::pivot_wider(names_from = pos, values_from = value) %>%
  distinct(section, x, y) %>%
  group_by(section) %>%
  mutate(ave_x = mean(x), ave_y = mean(y),
         angle = atan2(y - ave_y, x - ave_x)) %>%
  arrange(angle) %>%
  select(section, x, y)
new_design <- rbind(new_design,
                    slice_head(new_design, n = 1))

# helper function to do the color averaging
image_ave <- function(data) {
  if (nrow(data) > 0) {
    summarise(data, across(c(c.1,c.2,c.3), ~sqrt(mean(.x^2))),
              .groups = "drop") %>%
      mutate(rgb = rgb(c.1,c.2,c.3))
  } else {
    data
  }
}

# slow
new_inpoly <- new_design %>%
  group_by(section) %>%
  tidyr::nest() %>%
  mutate(in_section = purrr::map(data, ~pointsInPolygon(image_df, .x)))

new_colors <- new_inpoly %>%
  mutate(new_colors = purrr::map(in_section, ~image_ave(image_df[.x,]))) %>%
  tidyr::unnest(new_colors) %>%
  select(section, R = c.1, G = c.2, B = c.3, color = rgb)

# Plot the FPP again with this palette
plot_fpp_block(test_design$design,
               test_design$fpp_order,
               fill_sections = TRUE,
               palette = new_colors)
# the result is a FPP block with the colors inspired by the image


# Now lets make something where we split the image into a grid
# Then we use the FPP generator on each block
# above we added x_group and y_group to the image_df
# lets create a FPP design for each of those!
make_block <- function(image_piece, section_id) {
  # add/subtract 1 for section size to avoid border problem
  dimx0 <- min(image_piece$x) - 1
  dimx <- max(image_piece$x) + 1
  dimy0 <- min(image_piece$y) - 1
  dimy <- max(image_piece$y) + 1
  design_init <- tribble(
    ~section, ~line, ~start,  ~stop,
    section_id, 1, c(dimx0, dimy0), c(dimx0, dimy),
    section_id, 2, c(dimx0, dimy), c(dimx,dimy),
    section_id, 3, c(dimx,dimy), c(dimx, dimy0),
    section_id, 4, c(dimx, dimy0), c(dimx0, dimy0)
  )
  test_design <- make_fpp_design(n = 5, design_init)

  new_design <- test_design$design %>%
    tidyr::unnest(c(start, stop)) %>%
    mutate(pos = rep(c("x", "y"), n()/2)) %>%
    tidyr::pivot_longer(cols = c(start, stop)) %>%
    tidyr::pivot_wider(names_from = pos, values_from = value) %>%
    distinct(section, x, y) %>%
    group_by(section) %>%
    mutate(ave_x = mean(x), ave_y = mean(y),
           angle = atan2(y - ave_y, x - ave_x)) %>%
    arrange(angle) %>%
    select(section, x, y)
  new_design <- rbind(new_design,
                      slice_head(new_design, n = 1))

  return(list(
    design = test_design,
    poly = new_design))
}

# run on all blocks, join back together
section_letters <- apply(combn(c(LETTERS, letters), 2), 2,
                         function(x) paste(x, collapse = ""))
fpp_image <- image_df %>%
  group_by(x_group, y_group) %>%
  tidyr::nest() %>%
  mutate(section_base = section_letters[cur_group_id()]) %>%
  mutate(fpp_design = purrr::map(data, ~make_block(.x, section_id = section_base)))

# get the polygon data, make into one big data.frame
all_poly <- fpp_image %>%
  ungroup() %>%
  mutate(fpp_poly = purrr::map(fpp_design, ~.x[["poly"]])) %>%
  select(fpp_poly) %>%
  tidyr::unnest(fpp_poly) %>%
  select(section, x, y)

# slow - need better approach
all_inpoly <- all_poly %>%
  group_by(section) %>%
  tidyr::nest() %>%
  mutate(in_section = purrr::map(data, ~pointsInPolygon(image_df, .x)))

all_colors <- all_inpoly %>%
  mutate(new_colors = purrr::map(in_section, ~image_ave(image_df[.x,]))) %>%
  tidyr::unnest(new_colors) %>%
  select(section, R = c.1, G = c.2, B = c.3, color = rgb)

# plot the entire design with the new colors
big_design <- fpp_image %>%
  mutate(designs = purrr::map(fpp_design, ~.x[["design"]][["design"]])) %>%
  select(designs) %>%
  tidyr::unnest(designs)

plot_fpp_block(big_design,
               fill_sections = TRUE,
               show_labels = FALSE,
               show_lines = FALSE,
               palette = all_colors) +
  scale_y_continuous( trans=scales::reverse_trans()) +
  labs(title = "")

ggsave("examples/r_logo_fpp.pdf")

# Ok - that took forever
# lets try another image

my_image <- load.image("~/Desktop/AliceW_1.png")

# For what I a doing, I don't need a full size image
# Let's make it smaller and easier to work with
my_image <- imresize(my_image, 0.1)
plot(my_image)

image_df <- image_to_df(my_image)

n_squares_x <- 5  # higher - more pieces, more like the image
n_squares_y <- 8

split_x_len <- max(image_df$x) / n_squares_x
split_x <- data.frame(x = seq_len(max(image_df$x)),
                      x_group = ceiling(seq_len(max(image_df$x)) / split_x_len))
split_y_len <- max(image_df$y) / n_squares_y
split_y <- data.frame(y = seq_len(max(image_df$y)),
                      y_group = ceiling(seq_len(max(image_df$y)) / split_y_len))
image_df <- image_df %>%
  left_join(split_x, by = "x") %>%
  left_join(split_y, by = "y")


# just make squares
image_reduced <- image_df %>%
  group_by(x_group, y_group) %>%
  summarise(across(c(c.1, c.2, c.3), ~sqrt(mean(.x^2))),
            .groups = "drop") %>%
  mutate(rgb = rgb(c.1, c.2, c.3))

ggplot(image_reduced, aes(x_group, y_group)) + geom_raster(aes(fill = rgb)) +
  coord_equal() +
  scale_fill_identity() +
  scale_y_continuous(expand=c(0,0), trans=scales::reverse_trans()) +
  theme(legend.position = "none")

# make these FPP
fpp_image <- image_df %>%
  group_by(x_group, y_group) %>%
  tidyr::nest() %>%
  mutate(section_base = section_letters[cur_group_id()]) %>%
  mutate(fpp_design = purrr::map(data, ~make_block(.x, section_id = section_base)))

# get the polygon data, make into one big data.frame
all_poly <- fpp_image %>%
  ungroup() %>%
  mutate(fpp_poly = purrr::map(fpp_design, ~.x[["poly"]])) %>%
  select(fpp_poly) %>%
  tidyr::unnest(fpp_poly) %>%
  select(section, x, y)

# slow - need better approach
all_inpoly <- all_poly %>%
  group_by(section) %>%
  tidyr::nest() %>%
  mutate(in_section = purrr::map(data, ~pointsInPolygon(image_df, .x)))

all_colors <- all_inpoly %>%
  mutate(new_colors = purrr::map(in_section, ~image_ave(image_df[.x,]))) %>%
  tidyr::unnest(new_colors) %>%
  select(section, R = c.1, G = c.2, B = c.3, color = rgb)

# plot the entire design with the new colors
big_design <- fpp_image %>%
  mutate(designs = purrr::map(fpp_design, ~.x[["design"]][["design"]])) %>%
  select(designs) %>%
  tidyr::unnest(designs)

plot_fpp_block(big_design,
               fill_sections = TRUE,
               show_labels = FALSE,
               show_lines = FALSE,
               palette = all_colors) +
  scale_y_continuous( trans=scales::reverse_trans()) +
  labs(title = "")

ggsave("examples/my_photo-fpp2.pdf")


## final version - using recursive FPP blocks ----
# make these FPP
# get the image dimensions
dimx <- dim(my_image)[1]
dimy <- dim(my_image)[2]
design_init <- tribble(
  ~section, ~line, ~start,  ~stop,
  "A",      1,    c(0,  0), c(0, dimy),
  "A",      2,    c(0, dimy), c(dimx,dimy),
  "A",      3,    c(dimx,dimy), c(dimx, 0),
  "A",      4,    c(dimx, 0), c(0,  0)
)

fpp_image <- repeated_fpp(design_init, rep = 4)
plot_fpp_block(fpp_image)

# convert design to polygon format we can work with
new_design <- fpp_image %>%
  tidyr::unnest(c(start, stop)) %>%
  mutate(pos = rep(c("x", "y"), n()/2)) %>%
  tidyr::pivot_longer(cols = c(start, stop)) %>%
  tidyr::pivot_wider(names_from = pos, values_from = value) %>%
  distinct(section, x, y) %>%
  group_by(section) %>%
  mutate(ave_x = mean(x), ave_y = mean(y),
         angle = atan2(y - ave_y, x - ave_x)) %>%
  arrange(angle) %>%
  select(section, x, y)
new_design <- rbind(new_design,
                    slice_head(new_design, n = 1))

# slow
new_inpoly <- new_design %>%
  group_by(section) %>%
  tidyr::nest() %>%
  mutate(in_section = purrr::map(data, ~pointsInPolygon(image_df, .x)))

new_colors <- new_inpoly %>%
  mutate(new_colors = purrr::map(in_section, ~image_ave(image_df[.x,]))) %>%
  tidyr::unnest(new_colors) %>%
  select(section, R = c.1, G = c.2, B = c.3, color = rgb)

# Plot the FPP again with this palette
plot_fpp_block(fpp_image,
               fill_sections = TRUE,
               palette = new_colors) +
  scale_y_continuous( trans=scales::reverse_trans()) +
  labs(title = "")

ggsave("examples/my_photo-fpp3.pdf")
