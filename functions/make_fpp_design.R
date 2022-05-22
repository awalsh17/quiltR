#' Create a FPP block from an initial design or blank design
#'
#' @param initial_design (optional) this defaults to a
#' square from (0,0) to (10,10)
#' @param n the number of lines to add (default is 3)
#' @param only_valid (default TRUE) only allow piecable designs
#' @param required_lines (optional) require lines through these points
#' @param verbose (default FALSE) for debugging
#'
#' @return a list with "design", "plot", "fpp_order", and "all_designs"
#' @export
make_fpp_design <- function(initial_design = NULL,
                            n = 3,
                            only_valid = TRUE,
                            required_lines = NULL,
                            verbose = FALSE) {
  all_designs <- list()
  if (is.null(initial_design)) {
    new_design <- tribble(
      ~section, ~line, ~start,  ~stop,
      "A",      1,    c(0,  0), c(0, 10),
      "A",      2,    c(0, 10), c(10,10),
      "A",      3,    c(10,10), c(10, 0),
      "A",      4,    c(10, 0), c(0,  0)
    )
  } else {
    new_design <- initial_design
  }

  i <- 1
  all_designs[[i]] <- new_design
  while (i < n + 1) {
    # if there are required lines, make sure we do these first

    # choose a section to split
    section <- sample(new_design$section, 1)
    # choose the lines to split
    split_lines <- sample(new_design$line[new_design$section == section], 2)

    # pick a location on each line to split
    is_adjacent <-
      all(new_design$start[new_design$line == split_lines[1]][[1]] ==
                         new_design$stop[new_design$line == split_lines[2]][[1]]) |
      all(new_design$stop[new_design$line == split_lines[1]][[1]] ==
            new_design$start[new_design$line == split_lines[2]][[1]]) |
      all(new_design$start[new_design$line == split_lines[1]][[1]] ==
            new_design$start[new_design$line == split_lines[2]][[1]]) |
      all(new_design$stop[new_design$line == split_lines[1]][[1]] ==
            new_design$stop[new_design$line == split_lines[2]][[1]])

    # if these are adjacent lines, do not use a corner
    split_coord <- list(
      get_point_on_line(new_design[new_design$line==split_lines[1],],
                        corners = !is_adjacent),
      get_point_on_line(new_design[new_design$line==split_lines[2],],
                        corners = !is_adjacent))

    # DEBUG verbose
    if (verbose) {
      print(paste("split_section:", section,
                  "; split_lines:", paste(split_lines, collapse = ","),
                  "; is_adjacent:", is_adjacent,
                  "; split_coord:", paste(unlist(split_coord), collapse = ",")))
    }

    # make sure these are not equal to each other
    if (!all(split_coord[[1]] == split_coord[[2]])) {
      # create the split
      new_design_possible <- split_fpp_section(new_design,
                                               split_section = section,
                                               split_lines,
                                               split_coord)
      if (!identical(new_design_possible, new_design)) {
        # hack - add filter for sections with < 3 lines
        # not sure why this is still needed
        new_design_possible <- new_design_possible %>%
          distinct(section, line, .keep_all = TRUE) %>%
          add_count(section) %>%
          filter(n > 2) %>%
          select(-n)

        # check if valid
        inc_matrix <- get_fpp_inc_matrix(new_design_possible,
                                         initial_design = initial_design)
        test_order <- get_fpp_order(inc_matrix)
        if (test_order$valid) {
          new_design <- new_design_possible
          i <- i + 1
          all_designs[[i]] <- new_design
        } else if (!only_valid) {
          new_design <- new_design_possible
          i <- i + 1
          all_designs[[i]] <- new_design
        }
      } else {
        if (verbose) print("split unsuccessful")
      }
    }
  }

  # get the order
  inc_matrix <- get_fpp_inc_matrix(new_design)

  return(list(design = new_design,
              all_designs = all_designs,
              fpp_order = get_fpp_order(inc_matrix)$order,
              plot = plot_fpp_block(new_design, get_fpp_order(inc_matrix)$order)))
}
