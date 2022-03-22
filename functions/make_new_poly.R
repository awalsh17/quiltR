# have each face as a set of 4 x and y coordinates, a group id, and fill value

#' Make a box in 2-point perspective, but in a format to go into geom_polygon
#'
#' @param xes
#' @param yes
#' @param vp_scale
#'
#' @return
#' @export
#'
#' @examples
make_new_poly <- function(xes, yes,
                          vp_scale = 10) {
  # pick the vanishing points (y == 0)
  vp <- list(c(0, 0), c(vp_scale, 0))
  # make the center vertical segment
  cube <- data.frame(
    x = xes[2],
    xend = xes[2],
    y = yes[1],
    yend = yes[2]
  )

  # add the left vertical segment
  new_row <- c(
    xes[1], xes[1],
    (yes[1] / xes[2]) * xes[1],
    (yes[2] / xes[2]) * xes[1]
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
    xes[3], xes[3],
    (yes[1] / (xes[2] - vp[[2]][1])) * (xes[3] - xes[2]) + yes[1],
    (yes[2] / (xes[2] - vp[[2]][1])) * (xes[3] - xes[2]) + yes[2]
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

  # if all above or below y = 0,  then get bottom or top segments
  add_top <- all(poly$y < 0)
  add_bottom <- all(poly$y > 0)

  if (add_top) {
    # intersect left bottom [2,3] to right vp and right bottom [3,3] to left vp
    P1top <- c(xes[1], max(poly$y[poly$x == xes[1]])) # left top
    P2 <- vp[[2]] # right vp
    P3top <- c(xes[3], max(poly$y[poly$x == xes[3]])) # right top
    P4 <- vp[[1]] # left vp
    poss_top <- line.line.intersection(P1top, P2,
      P3top, P4,
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
    poss_bot <- line.line.intersection(P1bottom, P2,
      P3bottom, P4,
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
