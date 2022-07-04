# make 4 panels that are 36 in wide by 39 high
library(dplyr)
library(ggplot2)

# current issues: need better control over point size
# so we should have these be actual poly? not points?

final_size <- c(36, 39)

xlim <- c(0, 10*final_size[1]/final_size[2])
ylim <- c(0, 10)
size_dist <- c(2, 2.5, 4, 5)

for (b in 1:5) {
  circles <- data.frame(
    x = sample(c(0.5:3,6:(xlim[2]-0.5)), 4),
    y = sample(c(0.5:3,7:9.5), 4),
    size = size_dist,
    color = c("a", "b", "c", "d")
  )

  circles %>%
    ggplot(aes(x = x, y = y, color = color, size = size)) +
    geom_point(shape = 19, alpha = 0.2) +
    geom_point(shape = 1, color = "gray50") +
    scale_size(range = c(50*10, 90*10)) +
    scale_color_manual(values = c("a" = "red",
                                  "b" = "lightblue",
                                  "c" = "pink",
                                  "d" = "navy")) +
    coord_equal(xlim = xlim, ylim = ylim, expand = F) +
    theme_void() +
    theme(legend.position = "none", panel.border = element_rect(fill = NA))

  # when you print - make a pdf these dimensions
  ggsave(paste0("~/Downloads/quilt_circles/test_circles_",b,".pdf"),
         width = final_size[1], height = final_size[2])
}

