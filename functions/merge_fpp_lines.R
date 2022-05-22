merge_fpp_lines <- function(design,
                            edge_lines) {

  # calculate the lines

  result_design <- design %>%
    distinct(section, line, .keep_all = TRUE) %>%
    tidyr::unnest(c(start, stop)) %>%
    mutate(pos = rep(c("x", "y"), n()/2)) %>%
    tidyr::pivot_wider(names_from = pos, values_from = c(start, stop)) %>%
    mutate(slope = (stop_y - start_y) / (stop_x - start_x),
           intercept = stop_y - slope * stop_x,
           # for the verticals, make intercept the x intercept
           intercept = ifelse(is.infinite(slope),
                              start_x, intercept))

  # remove the edge lines, these are a special case
  edge_lines <- edge_lines %>%
    distinct(section, line, .keep_all = TRUE) %>%
    tidyr::unnest(c(start, stop)) %>%
    mutate(pos = rep(c("x", "y"), n()/2)) %>%
    tidyr::pivot_wider(names_from = pos, values_from = c(start, stop)) %>%
    mutate(slope = (stop_y - start_y) / (stop_x - start_x),
           intercept = stop_y - slope * stop_x,
           # for the verticals, make intercept the x intercept
           intercept = ifelse(is.infinite(slope),
                              start_x, intercept))

  result_design <- result_design %>%
    anti_join(edge_lines, by = c("slope", "intercept"))

  result_design <- result_design %>%
    # need to round the numbers
    mutate(slope = round(slope, 5), intercept = round(intercept, 5)) %>%
    group_by(slope, intercept) %>%
    mutate(line = cur_group_id()) %>%
    ungroup()

  return(result_design)
}
