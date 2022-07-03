# similar to random fpp but with a constrained design
# basically you want to make a certain shape

library(dplyr)
library(tibble)
library(ggplot2)
library(HyperG)

# the functions to do this work are sourced
fpp_functions <- list.files(path = here::here("functions"),
                            pattern = "fpp", full.names = TRUE)
lapply(fpp_functions, source)

set.seed(1001)

# initialize with outline shape ----
design_init <- tribble(
  ~section, ~line, ~start_x, ~start_y,  ~stop_x, ~stop_y,
  "A",      1,     0,  0,  0, 10,
  "A",      2,     0, 10, 10, 10,
  "A",      3,    10, 10, 10,  0,
  "A",      4,    10,  0,  0,  0
)
# and also add lines you need in the design
design_lines <- tribble(
  ~section, ~line, ~start_x, ~start_y,  ~stop_x, ~stop_y,
  "req",      5,    1, 1, 5, 9,
  "req",      6,    5, 9, 9, 1,
  "req",      7,    9, 1, 1, 1
)

# Plot this design
full_design <- rbind(design_init, design_lines)
plot_fpp_block(full_design)

# write code that will add lines and check if valid design

# pick a line in design
# choose a section to split
# add it by splitting section

split_fpp_section2 <- function(design_init,
                               split_section,
                               line_to_add) {

  # only work on the split section
  design_not_split <- design_init[design_init$section != split_section,]
  design_init <- design_init[design_init$section == split_section,]
  # which design lines intersect the new line and where
  # extend the line_to_add
  # if vertical -
  if (line_to_add$start_x == line_to_add$stop_x) {
    line_to_add$start_y <- -1
    line_to_add$stop_y <- 11
  } else {
    slope_to_add <- with(line_to_add, (stop_y - start_y) / (stop_x - start_x) )
    line_to_add$start_x <- -1
    line_to_add$start_y <- line_to_add$stop_y - slope_to_add*line_to_add$stop_x
    line_to_add$stop_x <- 11
    line_to_add$stop_y <- slope_to_add*10 + line_to_add$start_y
  }

  intersections <- sapply(seq_len(nrow(design_init)), function(x)
    retistruct::line.line.intersection(
      c(line_to_add$start_x, line_to_add$start_y),
      c(line_to_add$stop_x, line_to_add$stop_y),
      c(design_init$start_x[x], design_init$start_y[x]),
      c(design_init$stop_x[x], design_init$stop_y[x]),
      TRUE
    )
  )

  # rownames here are the lines that are split
  split_coord <- as.data.frame(t(intersections), row.names = design_init$line)
  names(split_coord) <- c("x", "y")
  split_coord <- split_coord[complete.cases(split_coord),]
  # also remove Inf (overlapping)
  split_coord <- split_coord[!is.infinite(split_coord$x),]

  # now create the new section at these points
  # remove that line from the design_lines

  # add lines for the split lines
  kept_lines <- design_init %>% filter(!line %in% rownames(split_coord))
  split_lines <- rbind(design_init[design_init$line == rownames(split_coord)[1],],
                       design_init[design_init$line == rownames(split_coord)[1],],
                       design_init[design_init$line == rownames(split_coord)[2],],
                       design_init[design_init$line == rownames(split_coord)[2],])
  split_lines$stop_x[1] <- split_coord$x[1]
  split_lines$start_x[2] <- split_coord$x[1]
  split_lines$stop_y[1] <- split_coord$y[1]
  split_lines$start_y[2] <- split_coord$y[1]
  split_lines$stop_x[3] <- split_coord$x[2]
  split_lines$start_x[4] <- split_coord$x[2]
  split_lines$stop_y[3] <- split_coord$y[2]
  split_lines$start_y[4] <- split_coord$y[2]

  new_design <- rbind(kept_lines, split_lines)

  # rename the sections
  new_design <- new_design %>%
    mutate(
      pos_start = sign((split_coord$x[2] - split_coord$x[1]) *
                         (start_y - split_coord$y[1]) -
                         (split_coord$y[2] - split_coord$y[1]) *
                         (start_x - split_coord$x[1])),
      pos_stop = sign((split_coord$x[2] - split_coord$x[1]) *
                        (stop_y - split_coord$y[1]) -
                        (split_coord$y[2] - split_coord$y[1]) *
                        (stop_x - split_coord$x[1])),
      pos_overall = (pos_stop + pos_start),
      section = ifelse(pos_overall > 0,
                       paste0(section, "1"),
                       paste0(section, "2"))
    ) %>%
    select(-pos_start, -pos_stop, -pos_overall)

  # Add lines for the new line twice
  new_line <- tribble(
    ~section, ~line, ~start_x, ~start_y,  ~stop_x, ~stop_y,
    paste0(split_section, "1"), 0, split_coord[1,1], split_coord[1,2], split_coord[2,1], split_coord[2,2],
    paste0(split_section, "2"), 0, split_coord[1,1], split_coord[1,2], split_coord[2,1], split_coord[2,2]
  )
  new_design <- rbind(new_design, new_line)

  # add back in the sections not split
  new_design <- rbind(new_design, design_not_split)

  # renumber the lines (the number is the same for identical lines)
  new_line_numbers <- distinct(new_design, start_x, start_y,
                               stop_x, stop_y) %>%
    mutate(line = 1:n())
  new_design <- new_design %>%
    select(-line) %>%
    left_join(new_line_numbers, by = c("start_x","start_y","stop_x","stop_y"))

  return(new_design)
}

# test this new function
test_design <- split_fpp_section2(design_init, "A", design_lines[1,])
plot_fpp_block(test_design)

# need to find the section to split - test whether line is inside a section
# will be an issue if crossing two sections.
sections_with_lines <- function(design, line_to_add) {
  xy <- line_to_add %>% distinct(section, line, .keep_all = TRUE) %>%
    tidyr::pivot_longer(cols = start_x:stop_y,
                        names_to = c("name", ".value"),
                        names_pattern = "(.*)_(.)")
  # make a proper poly
  test_poly <- design %>% distinct(section, line, .keep_all = TRUE) %>%
    tidyr::pivot_longer(cols = start_x:stop_y,
                        names_to = c("name", ".value"),
                        names_pattern = "(.*)_(.)") %>%
    distinct(section, x, y) %>%
    group_by(section) %>%
    mutate(ave_x = mean(x), ave_y = mean(y),
           angle = atan2(y - ave_y, x - ave_x)) %>%
    arrange(angle) %>%
    ungroup() %>%
    select(section, x, y)

  # pointsInPolygon does not include point ON the section
  # hack - add mid point on line
  xy <- xy %>%
    add_row(x = mean(xy$x), y = mean(xy$y))
  #
  res <- sapply(setNames(unique(test_poly$section), unique(test_poly$section)),
                function(sec) {secr::pointsInPolygon(
                  xy[,c("x","y")],
                  filter(test_poly, section == sec)[,c("x","y")])}
  ) %>% apply(., 2, any) %>% tibble::enframe(name = "section")
  return(res)
}

# do for all the lines
new_design <- design_init
for (x in 1:3) {
  section_to_split <- sections_with_lines(new_design, design_lines[x,]) %>%
    filter(value) %>% head(1) %>% pull(section)

  new_design <- split_fpp_section2(new_design,
                                   section_to_split,
                                   design_lines[x,])
}
plot_fpp_block(new_design)


# try a new design
design_lines <- tribble(
  ~section, ~line, ~start_x, ~start_y,  ~stop_x, ~stop_y,
  "req",      5,    1, 1, 5, 9,
  "req",      6,    5, 9, 9, 1,
  "req",      7,    9, 1, 1, 1,
  "req",      8,    6, 0, 6, 1,
  "req",      9,    4, 0, 4, 1
)

new_design <- design_init
for (x in seq_len(nrow(design_lines))) {
  section_to_split <- sections_with_lines(new_design, design_lines[x,]) %>%
    filter(value) %>% head(1) %>% pull(section)

  new_design <- split_fpp_section2(new_design,
                                   section_to_split,
                                   design_lines[x,])
}
plot_fpp_block(new_design)


#
rotate_design <- function(design, alpha = 0.5*pi) {
  design <- distinct(design, section, line, .keep_all = TRUE) %>%
    tidyr::pivot_longer(cols = start_x:stop_y,
                        names_to = c("name", ".value"),
                        names_pattern = "(.*)_(.)")
  M <- design %>%
    select(x, y) %>% as.matrix()
  #rotation matrix
  rotm <- matrix(c(cos(alpha),
                   sin(alpha),
                   -sin(alpha),
                   cos(alpha)), ncol=2)
  #shift, rotate, shift back
  # rotated <- t(rotm %*%
                 # (t(M) - c(M[1, 1], M[1, 2])) + c(M[1, 1], M[1, 2]))
  rotated <- t(rotm %*% t(M))
  design$x <- rotated[, 1]
  design$y <- rotated[, 2]
  design <- design %>%
    tidyr::pivot_wider(names_from = name, names_glue = "{name}_{.value}",
                       values_from = c(x,y))
  return(design)
}


# make 25 blocks with random starting order of lines
# can also rotate the "design_lines"

rotate_options <- c(pi, 0.5*pi, 1.5*pi)
blocks <- lapply(1:25,
                 function(x) {
                   new_design <- design_init
                   design_lines <- design_lines[sample(1:5, 5), ]
                   # design_lines <- rotate_design(
                   #   design_lines,
                   #   alpha = sample(rotate_options, 1))
                   for (x in seq_len(nrow(design_lines))) {
                     section_to_split <-
                       sections_with_lines(new_design, design_lines[x,]) %>%
                       filter(value) %>% head(1) %>% pull(section)

                     new_design <- split_fpp_section2(new_design,
                                                      section_to_split,
                                                      design_lines[x,])
                   }
                   return(new_design)
                 }
)
# then put them in 5 x 5 grid
# choose colors from the fabric colors sorted
fabrics <- readRDS("colors/annotated_fabric_colors.Rds")

plot_fpp_block(blocks[[5]],
               fill_sections = TRUE,
               palette = fabrics[1:6,])

plot_fpp_block(blocks[[2]] %>%
                 mutate(start_x = start_x +10,
                        stop_x = stop_x +10),
               fill_sections = TRUE,
               palette = fabrics[6:11,])

plot_fpp_block(blocks[[1]],
               fill_sections = TRUE,
               palette = fabrics[11:16,])

plot_fpp_block(blocks[[3]],
               fill_sections = TRUE,
               palette = fabrics[16:21,])

big_design <- rbind(blocks[[1]],
                    blocks[[2]] %>%
                      mutate(section = sub("A","B",section),
                             start_x = start_x +10,
                             stop_x = stop_x +10),
                    blocks[[3]] %>%
                      mutate(section = sub("A","C",section),
                             start_y = start_y +10,
                             stop_y = stop_y +10),
                    blocks[[4]] %>%
                      mutate(section = sub("A","D",section),
                             start_x = start_x +10,
                             stop_x = stop_x +10,
                             start_y = start_y +10,
                             stop_y = stop_y +10)
)
# issue clear here with geom_poly if sections are not uniquely named
plot_fpp_block(big_design,
               fill_sections = TRUE,
               palette = fabrics %>% filter(lum > 0.5, B > 0.9))
