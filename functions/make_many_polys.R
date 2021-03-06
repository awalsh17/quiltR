#' Create some art
#'
#' Create an object and plot with ggplot2. Allows for
#' customization for the number of shapes, the colors,
#' and the relative widths and heights of the shapes.
#'
#' @param out_path
#' @param n_cubes
#' @param n_second_color
#' @param height_range
#' @param width_range
#' @param horizon_y
#' @param color_values
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun {
#' d <- make_many_polys("tests/simple_cubes10.png",
#' height_range = 20:60,
#' width_range = 10:30,
#' n_cubes = 3, n_second_color = 1)
#' }
make_many_polys <- function(out_path = NULL,
                          n_cubes = 20,
                          n_second_color = 3,
                          height_range = 30:60,
                          width_range = 30:60,
                          horizon_y = 50,
                          color_values = c("#E8958A",
                                           "#FDBFA7",
                                           "#CB563B",
                                           "#A63E1C",
                                           "#5D758B",
                                           "#C1CBC9",
                                           "#4B5D79",
                                           "#353747",
                                           "white",
                                           "gray95"),
                          ...
                          ) {

  # need a named vector to make the ids we provide
  names(color_values) <- c("alt-l-side",
                           "alt-t-side",
                           "alt-r-side",
                           "alt-b-side",
                           "l-side",
                           "t-side",
                           "r-side",
                           "b-side",
                           "sky",
                           "ground")

  # n_tall <- sample(height_range, 1)
  # n_wide <- sample(width_range, 1)
  poly <- list()
  for (n in 1:n_cubes) {
    n_tall <- sample(height_range, 1, replace = T)
    n_wide <- sample(width_range, 1, replace = T)
    xes <- sort(c(0, sample(1:n_wide, 2))) + sample(1:(99-n_wide), 1)
    yes <- sort(sample(0:n_tall, 2)) +
      sample(1:(99-n_tall), 1)
    poly[[n]] <- make_new_poly(xes = xes,
                               yes = yes,
                               horizon_y = horizon_y,
                               ...)
  }
  poly <- dplyr::bind_rows(poly, .id = "cube_id")
  poly$id <- paste0(poly$cube_id, poly$id)
  # add a background poly
  sky <- data.frame(cube_id = "sky",
                    x = c(0, 0, 100, 100),
                    y = c(horizon_y, 100, 100, horizon_y),
                    id = "sky",
                    value = "sky")
  ground <- data.frame(cube_id = "ground",
                       x = c(0, 0, 100, 100),
                       y = c(horizon_y, 0, 0, horizon_y),
                       id = "ground",
                       value = "ground")
  background_poly <- rbind(sky, ground)

  # can also make some of the cubes a different color
  poly <- poly %>%
    dplyr::mutate(value = ifelse(
      cube_id %in% 1:n_second_color, paste("alt", value, sep = "-"),
      value))

  # use ggplot to plot the cubes ----
  p <- plot_polys(poly, background_poly, color_values)
  # display
  print(p)
  # save
  if (!is.null(out_path)) {
    ggsave(plot = p, out_path,
           width= 10, height = 10, units = "in", dpi = 400)
  }
  # return the poly data.frame
  return(poly)
}


plot_polys <- function(design, background_poly, color_values) {
  design %>%
    ggplot() +
    geom_polygon(alpha = 1, aes(x = x, y = y, group = id, fill = value),
                 data = background_poly, inherit.aes = FALSE) +
    geom_polygon(aes(x = x, y = y, group = id, fill = value),
                 alpha = 1) +
    scale_fill_manual(values = color_values) +
    theme_void() +
    theme(legend.position = "none")
}
