#' Create new lines and new sections
#' Note that if the new line does not create any new area
#' then we dont make the changes
#' @param design a tibble of my weird format: section,
#' start, stop, line
#' @param split_section a character with the section in
#' design to split
#' @param split_lines a vector with the two lines in
#' split_section to connect for the split
#' @param split_coord a list with the x, y coordinates
#' on split_lines to connect. Use get_point_on_line()
#' to get these
split_fpp_section <- function(design,
                          split_section,
                          split_lines,
                          split_coord) {

  design_initial <- design
  design_old <- design[design$section != split_section, ]
  design <- design[design$section == split_section, ]

  # split the split_lines at split_coord
  # do not add a line if the split_coord == start or stop
  for (x in c(1, 2)) {
    if (!(all(split_coord[x][[1]] == design$start[design$line == split_lines[x]][[1]]) |
          all(split_coord[x][[1]] == design$stop[design$line == split_lines[x]][[1]]))) {
      new_line <- rbind(design[design$line == split_lines[x],],
                        design[design$line == split_lines[x],])
      new_line[1, "stop"]  <- list(split_coord[x])
      new_line[2, "start"] <- list(split_coord[x])
      design <- design[design$line != split_lines[x],]
      design <- rbind(design, new_line)
    }
  }

  # rename the sections
  # using a fun trick to tell if the points are on one side or other of line
  # https://stackoverflow.com/questions/51088748
  design <- design %>%
    mutate(
      pos_start = purrr::map_dbl(
        start, ~sign((split_coord[[2]][1] - split_coord[[1]][1]) *
                       (.x[[2]] - split_coord[[1]][2]) -
                       (split_coord[[2]][2] - split_coord[[1]][2]) *
                       (.x[[1]] - split_coord[[1]][1]))),
      pos_stop = purrr::map_dbl(
        stop, ~sign((split_coord[[2]][1] - split_coord[[1]][1]) *
                      (.x[[2]] - split_coord[[1]][2]) -
                      (split_coord[[2]][2] - split_coord[[1]][2]) *
                      (.x[[1]] - split_coord[[1]][1]))),
      pos_overall = (pos_stop + pos_start),
      section = ifelse(pos_overall > 0,
                       paste0(section, "1"),
                       paste0(section, "2"))
    ) %>%
    select(-pos_start, -pos_stop, -pos_overall)

  # we add the line created by the split twice
  new_line <- tribble(
    ~section, ~line, ~start,  ~stop,
    paste0(split_section, "1"),      NA,    split_coord[[1]], split_coord[[2]],
    paste0(split_section, "2"),      NA,    split_coord[[1]], split_coord[[2]]
  )
  design <- rbind(design, new_line)

  # add the new part to the old part
  design <- rbind(design_old,
                  design)

  # renumber the lines (the number is the same for identical lines)
  new_line_numbers <- distinct(design, start, stop) %>%
    mutate(line = 1:n())
  design <- design %>%
    select(-line) %>%
    left_join(new_line_numbers, by = c("start","stop"))

  # check if new design is valid
  if (length(unique(design$line)) - length(unique(design_initial$line)) <= 1) {
    design <- design_initial
  }
  return(design)
}
