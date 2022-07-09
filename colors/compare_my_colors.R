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

