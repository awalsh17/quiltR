#' Pick a location on a line to split
#'
#' @param line a tibble of our weird format. See examples.
#' @param x (optional) if you know the x to split at
#' if missing, then the function picks a point on the line
#' @param corners logical, whether or not you can pick a
#' point on the edge (a corner). Default is TRUE
#'
#' @return a vector with an x and y value
#' @export
#'
#' @examples
#' \dontrun{
#' # This is the expected line input
#' #   A tibble: 1 × 4
#' #   section start     stop       line
#' #   <chr>   <list>    <list>    <int>
#' # 1 A2      <dbl [2]> <dbl [2]>     6
#' }
get_fpp_point <- function(line,
                          x = NULL,
                          corners = TRUE, ...) {

  # test if horizontal
  is_horiz <- line$stop[[1]][2] == line$start[[1]][2]
  is_vert <- line$stop[[1]][1] == line$start[[1]][1]
  ## Debug
  if (length(is_vert) == 0) (print(line))
  ##
  # get a y, given an x
  if (!is_vert) {
    # get line: y = slope * x + intercept
    slope <- (line$stop[[1]][2] - line$start[[1]][2]) /
      (line$stop[[1]][1] - line$start[[1]][1])
    intercept <- line$stop[[1]][2] - slope * line$stop[[1]][1]

    # check if the input is outside the range
    xes <- sort(c(line$start[[1]][1], line$stop[[1]][1]))
    # get an x if NULL
    if (is.null(x)) {
      x <- pick_division_points(xes, corners, ...)

    }
    if (x < xes[1]) {
      warning("input was outside the line range")
      x <- xes[1]
    } else if (x > xes[2]) {
      warning("input was outside the line range")
      x <- xes[2]
    }
    result <- c(x, slope * x + intercept)
  }
  # if the line was vertical, then just pretend the x input is a y
  if (is_vert) {
    slope <- (line$stop[[1]][1] - line$start[[1]][1]) /
      (line$stop[[1]][2] - line$start[[1]][2])
    intercept <- line$stop[[1]][1] - slope * line$stop[[1]][2]

    # check if the input is outside the range
    yes <- sort(c(line$start[[1]][2], line$stop[[1]][2]))
    # get an x if NULL
    if (is.null(x)) {
      x <- pick_division_points(yes, corners, ...)
    }
    if (x < yes[1]) {
      warning("input was outside the line range")
      x <- yes[1]
    } else if (x > yes[2]) {
      warning("input was outside the line range")
      x <- yes[2]
    }
    result <- c(slope * x + intercept, x)
  }
  return(result)
}

#' This helper could be modified to add different characteristics
#' to how the sections are split.
pick_division_points <- function(xes_or_yes,
                                 corners,
                                 method = "golden") {

  # first method - splits into 5 pieces
  # note that sample() is very tricky: sample(10, 1) is same as sample(1:10,1)

  if (method == "even") {
    mychoices <- seq(xes_or_yes[1], xes_or_yes[2], length = 5)
    if (corners) {
      x <- sample(mychoices, 1)
    } else {
      x <- sample(mychoices[c(2:4)], 1)
    }
  }

  # second method - choose midpoint or corners
  if (method == "midpoint") {
    mychoices <- c(xes_or_yes,
                   (xes_or_yes[2] + xes_or_yes[1]) / 2)
    if (corners) {
      x <- sample(mychoices, 1)
    } else {
      x <- mychoices[3]
    }
  }

  # third method - use the golden ratio
  if (method == "golden") {
    mychoices <- c(xes_or_yes,
                   xes_or_yes[1] +
                   (diff(abs(xes_or_yes)) / 1.618),
                   xes_or_yes[2] -
                     (diff(abs(xes_or_yes)) / 1.618))
    if (corners) {
      x <- sample(mychoices, 1)
    } else {
      x <- mychoices[3]
    }
  }

  return(x)
}
