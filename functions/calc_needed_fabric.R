# Function to loosely estimate the amount of each fabric needed
# Note: there are 2D shape packing algos that could be used,
# But for assembling quilting, need to leave a lot of extra
# So this just adds all the areas and adds 20 %

#' Calculate the yardage needed of each fabric given a design
#'
#' @param design a data.frame with our cube design, very specific and needs to be
#' generalized; value are the colors, cube_id and id identify each polygon
#' @param scale_factor a factor to scale the units from the design to inches (1 = no change)
#' @param scale_extra a factor to scale to add extra (default is 1.2, which is adding 20%)
#'
#' @return a tibble with the fabrics, total area (square inches), and estimated yardage to buy
#' assuming a standard 42 inch bolt
#' @export
#'
#' @examples
calc_needed_fabric <- function(design, scale_factor, scale_extra = 1.2) {
  # not sure about this formula honestly
  fabric_areas <- design %>%
    group_by(cube_id, id) %>%
    mutate(area_square = abs(max(y) - min(y)) * abs(max(x) - min(x)),
           area_missing_y = ((sort(y)[2] - sort(y)[1]) + (sort(y)[4] - sort(y)[3]))*abs(max(x) - min(x))*0.5,
           area_missing_x = ((sort(x)[2] - sort(x)[1]) + (sort(x)[4] - sort(x)[3]))*abs(max(y) - min(y))*0.5,
           area_final = area_square - area_missing_x - area_missing_y
    ) %>%
    ungroup() %>%
    distinct(cube_id, id, value, area_final) %>%
    group_by(value) %>%
    summarize(total_area = sum(area_final))

  # Now add extra and scale by the pattern scale factor (squared) to be in square inches
  fabric_areas$total_area <- scale_extra * scale_factor^2 * fabric_areas$total_area

  # Fabric bolt is 42 inches tall, so you can calculate the yardage needed as area / 42 / 36
  fabric_areas$yards <- fabric_areas$total_area / 42 / 36

  return(fabric_areas)
}
