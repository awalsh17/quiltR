# have each face as a set of 4 x and y coordinates, a group id, and fill value

#' Make a box in 2-point perspective, but in a format to go into geom_polygon
#'
#' The "field" is from (0,0) to (100,100) by default, but you can change
#' the aspect ratio with total_width.
#' The horizon is default half-way up the field at y = 50, but you can
#' change that with horizon_y.
#'
#' @param xes x coordinates of the three vertical lines
#' @param yes y min and max height of the center vertical
#' @param total_width (100) make it wider for a landscape
#' @param horizon_y (50) make it smaller to move horizon down, higher to move up
#'
#' @return
#' @export
#'
#' @examples
make_new_poly <- function(xes,
                          yes,
                          total_width = 100,
                          horizon_y = 50) {
  # pick the vanishing points
  vp <- list(c(0, horizon_y), c(total_width, horizon_y))
  # make the center vertical segment
  cube <- data.frame(
    x = xes[2],
    xend = xes[2],
    y = yes[1],
    yend = yes[2]
  )

  # add the left vertical segment
  new_row <- c(
    xes[1],
    xes[1],
    ((yes[1] - horizon_y) / xes[2]) * xes[1] + horizon_y,
    ((yes[2] - horizon_y) / xes[2]) * xes[1] + horizon_y
  )
  cube <- rbind(cube, new_row)

  # this is the first polygon
  poly <- data.frame(
    x = c(cube$x[1], cube$xend[1], cube$x[2], cube$xend[2]),
    y = c(cube$y[1], cube$yend[1], cube$yend[2], cube$y[2]),
    id = "l-side",
    value = "l-side"
  )

  # add the right vertical segment
  cube2 <- data.frame(
    x = xes[2],
    xend = xes[2],
    y = yes[1],
    yend = yes[2]
  )

  new_row <- c(
    xes[3],
    xes[3],
    ((yes[1] - horizon_y) / (xes[2] - total_width)) * (xes[3] - xes[2]) + yes[1],
    ((yes[2] - horizon_y) / (xes[2] - total_width)) * (xes[3] - xes[2]) + yes[2]
  )
  cube2 <- rbind(cube2, new_row)

  # this is the second polygon
  poly2 <- data.frame(
    x = c(cube2$x[1], cube2$xend[1], cube2$x[2], cube2$xend[2]),
    y = c(cube2$y[1], cube2$yend[1], cube2$yend[2], cube2$y[2]),
    id = "r-side",
    value = "r-side"
  )
  poly <- rbind(poly, poly2)

  # if all above or below horizon,  then get bottom or top segments
  add_top <- all(poly$y < horizon_y)
  add_bottom <- all(poly$y > horizon_y)

  if (add_top) {
    # intersect left bottom [2,3] to right vp and right bottom [3,3] to left vp
    P1top <- c(xes[1], max(poly$y[poly$x == xes[1]])) # left top
    P2 <- vp[[2]] # right vp
    P3top <- c(xes[3], max(poly$y[poly$x == xes[3]])) # right top
    P4 <- vp[[1]] # left vp
    poss_top <- line.line.intersection(P1top, P2, P3top, P4,
                                       interior.only = TRUE
    )
    # this is the third polygon
    poly3 <- data.frame(
      x = c(poss_top[1], P1top[1], xes[2], P3top[1]),
      y = c(
        poss_top[2], P1top[2],
        max(poly$y[poly$x == xes[2]]), P3top[2]
      ),
      id = "t-side",
      value = "t-side"
    )
    poly <- rbind(poly, poly3)
  }
  if (add_bottom) {
    # intersect left bottom [2,3] to right vp and right bottom [3,3] to left vp
    P1bottom <- c(xes[1], min(poly$y[poly$x == xes[1]])) # left bottom
    P2 <- vp[[2]] # right vp
    P3bottom <- c(xes[3], min(poly$y[poly$x == xes[3]])) # right bottom
    P4 <- vp[[1]] # left vp
    poss_bot <- line.line.intersection(P1bottom, P2, P3bottom, P4,
                                       interior.only = TRUE
    )
    # this is the third polygon
    poly3 <- data.frame(
      x = c(poss_bot[1], P1bottom[1], xes[2], P3bottom[1]),
      y = c(
        poss_bot[2], P1bottom[2],
        min(poly$y[poly$x == xes[2]]), P3bottom[2]
      ),
      id = "b-side",
      value = "b-side"
    )
    poly <- rbind(poly, poly3)
  }
  return(poly)
}
