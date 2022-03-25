# Convert this into something can be printed as templates for quilting
# need to choose the size you want the pattern to be
# notes: I don't think this handles non-square patterns now

create_pattern_pdf <- function(quilt_size = 36,
                               d,
                               filename,
                               vp = 100,
                               horizon_y = 50){
  # quilt_size units are arbitrary, here I am using inches
  scale_factor <- quilt_size / vp # change if you change vp
  # output the dimensions for each piece in each color
  # a single piece of paper with the outline of the piece.
  # 8.5 * 12 paper
  missing_paths <- d %>% arrange(desc(cube_id))
  horizontal <- data.frame(cube_id = c("",""),
                           x = c(0, vp),
                           y = c(horizon_y, horizon_y)) %>%
    mutate(x = x*scale_factor, y = y*scale_factor)
  pattern <- d %>%
    bind_rows(missing_paths) %>%
    arrange(cube_id) %>%
    mutate(x = x*scale_factor, y = y*scale_factor) %>%
    # mutate(x = x - min(x) +1 , y = y - min(y) + 1) %>%
    group_by(id) %>%
    mutate(ave_x = mean(x), ave_y = mean(y)) %>%
    ungroup() %>%
    ggplot() +
    # horizon line
    geom_path(aes(x = x, y = y, group = cube_id),
              data = horizontal,
              alpha = 1,
              color = "black",
              size = 1) +
    geom_path(aes(x = x, y = y, group = id),
              alpha = 1,
              color = "black",
              size = 2) +
    geom_polygon(aes(x = x, y = y, group = id),
                 fill = "white", color = "blue",
                 size = 0.5,
                 alpha = 1) +
    geom_text(aes(label = cube_id, x = ave_x, y = ave_y)) +
    # xlim(c(0,8.5)) + ylim(c(0,11)) +
    xlim(c(0, quilt_size)) + ylim(c(0, quilt_size)) +
    coord_equal(clip = "on", expand = FALSE) +
    theme_void() +
    # theme_minimal() +
    theme(legend.position = "none")
  print(pattern)
  # Now the size is just the size of the quilt
  ggsave(plot = pattern, filename = filename,
         width = quilt_size, height = quilt_size, units = "in")
}
