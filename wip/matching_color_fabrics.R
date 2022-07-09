# Can I find fabrics that match my design colors?

# https://stackoverflow.com/questions/9018016/how-to-compare-two-colors-for-similarity-difference

# how to find the closest color in our set? calc distances and sort? use KNN?

library(dplyr)

# Load in our fabric data - this has fabric name and lots of color data
# fabric name, hex, RGB, lum, HSV
fabrics <- readRDS("colors/annotated_fabric_colors.Rds")

# design_from_image.R can return RGB values from the image.
# Use image_reduced from design_from_image.R
# Try with RGB (dont think this is the best way)
all_colors <- rbind(image_reduced %>%
                      select(r, g, b, color = hex) %>%
                      mutate(type = "desired") %>%
                      mutate(name = 1:n()) ,
                    fabrics %>%
                      select(name = fabric_name,
                             r = R, g = G, b = B, color) %>%
                      mutate(type = "fabric"))

# calculate distances - euclidean
dist_rgb <- dist(all_colors[,c("r","g","b")], upper = TRUE)
dist_rgb <- as.matrix(dist_rgb)
dist_rgb_long <- data.frame(X1 = all_colors$name[col(dist_rgb)],
                            X2 = all_colors$name[row(dist_rgb)],
                            dist = c(dist_rgb))

# get the top two for the desired colors
new_palette <- dist_rgb_long %>%
  left_join(all_colors[,c("name","type")], by = c("X1" = "name")) %>%
  filter(type == "desired") %>%
  left_join(all_colors[,c("name","type","color")], by = c("X2" = "name")) %>%
  filter(type.y == "fabric") %>%
  arrange(dist) %>%
  group_by(X1) %>%
  slice_head(n = 2) %>%
  select(section = X1, fabric_name = X2, color) %>%
  mutate(name = rep(c("color","backup"), n()/2)) %>%
  tidyr::pivot_wider(names_from = name, values_from = c(color, fabric_name)) %>%
  rename(color = color_color)

# Plot with the new colors!
image_reduced <- image_reduced %>%
  mutate(section = as.character(1:n())) %>%
  left_join(new_palette[, c("section", "color")], by = "section")
ggplot(image_reduced, aes(x_group, y_group)) + geom_raster(aes(fill = color)) +
  coord_equal() +
  scale_fill_identity() +
  scale_y_continuous(expand=c(0,0), trans=scales::reverse_trans()) +
  theme(legend.position = "none") +
  theme_void() +
  labs(title = "Matched to fabric colors")
# note: pretty cool! just not the same colors I started with!

# Try with HSV
# add hsv to the data
image_reduced <- cbind(image_reduced,
                       as.data.frame(t(rgb2hsv(
                         image_reduced$r, image_reduced$g, image_reduced$b))))

all_colors <- rbind(image_reduced %>%
                      select(name = section,
                             h, s, v, color = hex) %>%
                      mutate(type = "desired")  ,
                    fabrics %>%
                      select(name = fabric_name,
                             h, s, v, color) %>%
                      mutate(type = "fabric"))

# as before, calculate the distance - play with method
dist_hsv <- dist(all_colors[,c("h","s","v")], upper = TRUE, method = "manhattan")
dist_hsv <- as.matrix(dist_hsv)
dist_hsv_long <- data.frame(X1 = all_colors$name[col(dist_hsv)],
                            X2 = all_colors$name[row(dist_hsv)],
                            dist = c(dist_hsv))

# get the top two for the desired colors
new_palette <- dist_hsv_long %>%
  left_join(all_colors[,c("name","type")], by = c("X1" = "name")) %>%
  filter(type == "desired") %>%
  left_join(all_colors[,c("name","type","color")], by = c("X2" = "name")) %>%
  filter(type.y == "fabric") %>%
  arrange(dist) %>%
  group_by(X1) %>%
  slice_head(n = 2) %>%
  select(section = X1, fabric_name = X2, color) %>%
  mutate(name = rep(c("color","backup"), n()/2)) %>%
  tidyr::pivot_wider(names_from = name, values_from = c(color, fabric_name)) %>%
  rename(color = color_color)

# Plot this attempt
image_reduced <- image_reduced %>%
  left_join(new_palette %>% select(section, color2 = color, color_backup),
            by = "section")
ggplot(image_reduced, aes(x_group, y_group)) + geom_raster(aes(fill = color2)) +
  coord_equal() +
  scale_fill_identity() +
  scale_y_continuous(expand=c(0,0), trans=scales::reverse_trans()) +
  theme(legend.position = "none") +
  theme_void() +
  labs(title = "Matched to fabric colors by hsv")
# note: pretty cool! just not the same colors I started with!
