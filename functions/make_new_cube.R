#' Create a dataframe with a box draw in 2-point perspective
#'
#' @param xes
#' @param yes
#' @param vp_scale
#'
#' @return
#' @export
#'
#' @examples
#' make_new_cube(xes = sort(sample(0:9, 3)),
#' yes = sort(sample(-10:10, 2)),
#' vp_scale = 30)
make_new_cube <- function(xes, yes,
                          vp_scale = 10) {
  # pick the vanishing points (y == 0)
  vp <- list(c(0, 0), c(vp_scale, 0))
  # make the center vertical segment
  cube <- data.frame(x = xes[2],
                     xend = xes[2],
                     y = yes[1],
                     yend = yes[2])

  # add the left vertical segment
  new_row <- c(xes[1], xes[1],
               (yes[1]/xes[2])*xes[1],
               (yes[2]/xes[2])*xes[1])
  cube <- rbind(cube, new_row)

  # add the right vertical segment
  new_row <- c(xes[3], xes[3],
               (yes[1]/(xes[2] - vp[[2]][1]))*(xes[3] -xes[2]) + yes[1],
               (yes[2]/(xes[2] - vp[[2]][1]))*(xes[3] -xes[2]) + yes[2])
  cube <- rbind(cube, new_row)

  # add the top lines
  new_row <- c(cube[1,1], cube[2,1], cube[1,3], cube[2,3])
  cube <- rbind(cube, new_row)
  new_row <- c(cube[1,1], cube[3,1], cube[1,3], cube[3,3])
  cube <- rbind(cube, new_row)

  # add the bottom lines
  new_row <- c(cube[1,1], cube[2,1], cube[1,4], cube[2,4])
  cube <- rbind(cube, new_row)
  new_row <- c(cube[1,1], cube[3,1], cube[1,4], cube[3,4])
  cube <- rbind(cube, new_row)

  # if all above or below y = 0,  then get bottom or top segments
  add_top <- all(c(cube$y, cube$yend) < 0)
  add_bottom <- all(c(cube$y, cube$yend) > 0)

  if (add_top) {
    # intersect left bottom [2,3] to right vp and right bottom [3,3] to left vp
    P1top <- c(xes[1], max(cube[2,3], cube[2,4]))  #left top
    P2 <- vp[[2]] # right vp
    P3top <- c(xes[3], max(cube[3,3], cube[3,4]))  #right top
    P4 <- vp[[1]] # left vp
    poss_top <- line.line.intersection(P1top, P2,
                                       P3top, P4, interior.only = TRUE)
    new_row <- c(poss_top[1], P1top[1], poss_top[2], P1top[2])
    cube <- rbind(cube, new_row)
    new_row <- c(poss_top[1], P3top[1], poss_top[2], P3top[2])
    cube <- rbind(cube, new_row)
  }
  if (add_bottom) {
    # intersect left bottom [2,3] to right vp and right bottom [3,3] to left vp
    P1bottom <- c(xes[1], min(cube[2,3], cube[2,4]))  #left bottom
    P2 <- vp[[2]] # right vp
    P3bottom <- c(xes[3], min(cube[3,3], cube[3,4]))  #right bottom
    P4 <- vp[[1]] # left vp
    poss_bottom <- line.line.intersection(P1bottom, P2,
                                          P3bottom, P4, interior.only = TRUE)
    new_row <- c(poss_bottom[1], P1bottom[1], poss_bottom[2], P1bottom[2])
    cube <- rbind(cube, new_row)
    new_row <- c(poss_bottom[1], P3bottom[1], poss_bottom[2], P3bottom[2])
    cube <- rbind(cube, new_row)
  }
  return(cube)
}
