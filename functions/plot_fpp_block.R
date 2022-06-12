#' Visualize a generated FPP block
#'
#' @param design a tibble with our weird design format
#' @param section_order optional, the order of the sections
#' that can be derived with the get_fpp_order() function
#' @param fill_sections logical (default is FALSE) if
#' TRUE, this will fill the sections with colors
#' @param palette optional color palette to use (has variable named
#' "color" with hex codes)
#' @param show_labels logical (default is TRUE) if TRUE, sections or
#' section order are shown
#' @param show_lines logical (default is TRUE) if TRUE, seam lines are
#' plotted as thick black lines
#'
#' @return a ggplot object
#' @export
#'
plot_fpp_block <- function(design,
                           section_order = NULL,
                           fill_sections = FALSE,
                           palette = NULL,
                           show_labels = TRUE,
                           show_lines = TRUE) {

  # Get the centroids of each section
  centers <- design %>%
    distinct(section, line, .keep_all = TRUE) %>%
    tidyr::unnest(c(start, stop)) %>%
    mutate(pos = rep(c("x", "y"), n()/2)) %>%
    tidyr::pivot_longer(cols = c("start", "stop")) %>%
    tidyr::pivot_wider(names_from = pos, values_from = value) %>%
    group_by(section) %>%
    summarise(ave_x = mean(x),
              ave_y = mean(y),
              .groups = "drop")

  # if section order provided, rename sections with the order
  if (!is.null(section_order)) {
    centers <- centers %>%
      left_join(tibble::enframe(section_order),
                by = c("section" = "name")) %>%
      mutate(section = value)
  }

  # if no fill, make the plot
  if (!fill_sections) {
    # Plot
    final_plot <- design %>%
      distinct(section, line, .keep_all = TRUE) %>%
      tidyr::unnest(c(start, stop)) %>%
      mutate(pos = rep(c("x", "y"), n()/2)) %>%
      tidyr::pivot_wider(names_from = pos, values_from = c(start, stop)) %>%
      ggplot(aes(x = start_x, xend = stop_x, y = start_y, yend = stop_y)) +
      geom_segment(aes(color = section)) +
      labs(x = "x", y = "y", title = "The design") +
      coord_equal() +
      theme_void(base_family = "Avenir") +
      theme(legend.position = "none")
    if (show_labels) {
      final_plot <- final_plot +
        geom_text(aes(label = section, x = ave_x, y = ave_y),
                  inherit.aes = FALSE, data = centers)
    }

  } else {
    # if yes to fill, make the plot
    # need to reorder for geom_path and geom_polygon
    reshape_design <- design %>%
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
      ungroup()

    final_plot <- reshape_design %>%
      ggplot() +
      geom_polygon(aes(group = section, x = x, y = y, fill = section)) +
      labs(x = "x", y = "y", title = "The design") +
      coord_equal() +
      theme_void(base_family = "Avenir") +
      theme(legend.position = "none")
    if (show_labels) {
      final_plot <- final_plot +
        geom_text(aes(label = section, x = ave_x, y = ave_y))
    }
    if (show_lines) {
      final_plot <- final_plot +
        geom_path(aes(group = section, x = x, y = y),
                  color = "black",
                  size = 1)
    }
    # colors
    if (!is.null(palette)) {
      if ("section" %in% colnames(palette)){
        palette_vector <- palette$color
        names(palette_vector) <- palette$section
        final_plot <- final_plot +
          scale_fill_manual(values = palette_vector)
      } else {
        palette <- cbind(centers,
                         palette[sample(1:nrow(palette), nrow(centers)),])
        final_plot <- final_plot +
          scale_fill_manual(values = palette$color) +
          geom_text(aes(label = fabric_name, x = ave_x, y = ave_y),
                    inherit.aes = FALSE,
                    family = "Avenir",
                    size = 2, nudge_y = -0.4,
                    data = palette)
      }

    }

  }
  final_plot
}
