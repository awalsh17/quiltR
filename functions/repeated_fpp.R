#' Make a recursive FPP design
#'
#' @param design the initial design
#' @param rep how many levels
#' @param n how many splits in each block
#'
#' @return a design object
#' @export
#'
repeated_fpp <- function(design, rep = 3, n = 3) {
  # get section names
  section_names <- c(LETTERS, paste0(LETTERS,2), paste0(LETTERS,3))
  # start
  prev_design <- design %>% mutate(fpp_order = 1)
  section_areas <- design
  for (i in seq_len(rep)) {

    new_design <- lapply(
      unique(section_areas$section),
      function(j) {
        temp_design <- make_fpp_design(
          n = n,
          initial_design = prev_design[prev_design$section == j,] %>%
            select(-fpp_order),
          return_plot = FALSE)
        return(temp_design$design %>%
                 left_join(
                   tibble::enframe(temp_design$fpp_order,
                                   name = "section",
                                   value = "fpp_order"),
                   by = c("section")))
      }
    ) %>% bind_rows() %>%
      rbind(prev_design %>%
              filter(!section %in% unique(section_areas$section)))

    # rename the sections so we can tell them apart
    # calc section sizes - only split the biggest
    prev_design <- new_design %>%
      group_by(section) %>%
      mutate(section = section_names[cur_group_id()]) %>%
      ungroup()

    section_areas <- prev_design %>%
      distinct(section, line, .keep_all = TRUE) %>%
      tidyr::unnest(c(start, stop)) %>%
      mutate(pos = rep(c("x", "y"), n()/2)) %>%
      tidyr::pivot_longer(cols = c(start, stop)) %>%
      tidyr::pivot_wider(names_from = pos, values_from = value) %>%
      distinct(section, x, y) %>%
      group_by(section) %>%
      mutate(ave_x = mean(x), ave_y = mean(y),
             angle = atan2(y - ave_y, x - ave_x)) %>%
      arrange(angle) %>%
      rbind(slice(., 1)) %>%
      summarise(area = 0.5 * abs(sum(x*lead(y) - lead(x)*y,
                                     na.rm = TRUE))) %>%
      ungroup() %>%
      arrange(desc(area)) %>%
      slice_head(n = 1)

    ## debug
    print(paste("sections to split:", distinct(section_areas,
                                               section, area)))
  }
  return(new_design %>%
           mutate(section = paste(stringr::str_extract(section, "[A-Za-z]"),
                                  fpp_order, sep = "-") )
  )
}

