# what are the colors I recently bought?

# kona bundle in driftless coordinates

colors1 <-
  c(
    "Spice 159",
    "Terracotta 482",
    "Salmon 1483",
    "Peach 1281",
    "Pearl Pink 1283",
    "Natural 1242",
    "Sable 275",
    "Mushroom 1239",
    "Smoke 1713",
    "Shitake 858",
    "Shale 456",
    "Shadow 457",
    "Haze 863",
    "Sky 1513",
    "Dusty Blue 362",
    "Fog 444",
    "Teal Blue 1373",
    "Cadet 1058",
    "Windsor 1389",
    "Gotham Grey 862"
  )
# AGF pure solids bundle in crystallizing
colors2 <-
  c(
    "Sugar Plum",
    "Mauvelous",
    "Potter's Clay",
    "Sweet Fig",
    "Bewitched",
    "Field of Lavender",
    "Lavender Water",
    "Wisteria",
    "Purple Pansy",
    "Amethyst",
    "Royal Cobalt",
    "Hydrangea",
    "Blueberry Zest",
    "Atmospheric",
    "Periwinkle",
    "Tranquil Waters",
    "Aero Blue",
    "Parisian Blue",
    "Denim Blue",
    "Heart of the Ocean",
    "Night Sea",
    "Nocturnal"
  )
# confirm these 20 & 22 overlap with our colors
fabrics <- readRDS("colors/annotated_fabric_colors.Rds")

length(intersect(colors1, fabrics$fabric_name))
length(intersect(colors2, fabrics$fabric_name))
setdiff(colors2, fabrics$fabric_name)

# only 7/22 of the art gallery fabrics are in our dataset.
# So I will go try to get them myself in illustrator?

# Can I get the nearest color to my color in the fabrics?
# https://stackoverflow.com/questions/9018016/how-to-compare-two-colors-for-similarity-difference
# get_image.R can return RGB values from the image.


# how to find the closest color in our set? calc distances and sort? use KNN?

# Try with RGB (dont think this is the best way)
all_colors <- rbind(test3 %>%
  mutate(type = "desired") %>%
  rename(name = section) ,
  fabrics %>%
    select(name = fabric_name,
           R, G, B, color) %>%
    mutate(type = "fabric"))

dist_rgb <- dist(all_colors[,c("R","G","B")], upper = TRUE)
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

plot_fpp_block(test_design$design,
               test_design$fpp_order,
               fill_sections = TRUE,
               palette = new_palette)


# Try with HSV
test3 <- cbind(test3, as.data.frame(t(rgb2hsv(test3$R, test3$G, test3$B))))

all_colors <- rbind(test3 %>%
                      select(name = section,
                             h, s, v, color) %>%
                      mutate(type = "desired")  ,
                    fabrics %>%
                      select(name = fabric_name,
                             h,s,v, color) %>%
                      mutate(type = "fabric"))

dist_hsv <- dist(all_colors[,c("h","s","v")], upper = TRUE)
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

plot_fpp_block(test_design$design,
               test_design$fpp_order,
               fill_sections = TRUE,
               palette = new_palette)
