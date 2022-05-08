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
  col = fabrics$color[seq(1, line * col)]
)

# Color names
text(rep((0:(col - 1) / col), line),
     sort(rep((0:(line - 1) / line), col), decreasing = T) + 0.01,
     fabrics$fabric_name[seq(1, line * col)],
     adj = 0,
     cex = 0.8)

