# I want to design the quilt using available colors
# Need the hex codes for kona solids, and art gallery fabric solids

# Then I can also know what to order!

# play crafts palette builder: http://www.play-crafts.com/blog/palettebuilder2/
# They have .ase files - luckily I have an Adobe Illustrator license and can open it
# Hm. This site: https://carl.camera/sandbox/aseconvert/ can take .ase and convert to hex.
# Huge thanks to the authors who made the .ase files available!!


# Saved as csvs in "colors" directory

fabrics <- rbind(
  read.csv("colors/kona_365.csv"),
  read.csv("colors/art_gallery.csv")
)
# Useful post on this
# https://stackoverflow.com/questions/61193516/how-to-sort-colours-in-r

# Can convert to RGB, HSV, and add luminosity
fabrics <- cbind(fabrics, as.data.frame(colorspace::hex2RGB(fabrics$color)@coords))
fabrics$lum <- sqrt(0.241 * fabrics$R + 0.691 * fabrics$G + 0.068 * fabrics$B)
fabrics <- cbind(fabrics, as.data.frame(t(rgb2hsv(fabrics$R, fabrics$G, fabrics$B))))

# Ideally, want to select colors we have and then sort blues, reds, etc.
fabrics <- fabrics[order(fabrics$lum, fabrics$R),]

library(TSP)
rgb <- t(as.matrix(fabrics[,c("R","G","B")]))
tsp <- as.TSP(dist(t(rgb)))
sol <- solve_TSP(tsp, control = list(repetitions = 1e3))
ordered_cols <- fabrics[sol,]

# Save out this data.frame with all the information
saveRDS(ordered_cols, "annotated_fabric_colors.Rds")

# ggplot2::qplot(x = 1:426, y = 1, fill = fabrics$color, geom = 'col', width = 1) +
#   ggplot2::theme_void() + theme(legend.position = "none")

# to plot all the colors, lets do that...
# https://r-graph-gallery.com/42-colors-names.html

# No margin around chart
par(mar = c(0,0,0,0))
# Empty chart
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")

# Settings
line <- 43
col <- 10

# Add color background
rect(
  rep((0:(col - 1) / col), line),
  sort(rep((0:(line - 1) / line), col), decreasing = T),
  rep((1:col / col), line) ,
  sort(rep((1:line / line), col), decreasing = T),
  border = "white" ,
  col = ordered_cols$color[seq(1, line * col)]
)

# Color names
text(rep((0:(col - 1) / col), line),
     sort(rep((0:(line - 1) / line), col), decreasing = T) + 0.01,
     ordered_cols$fabric_name[seq(1, line * col)],
     adj = 0,
     cex = 0.8)

