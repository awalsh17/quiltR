#' Get the order to add the pieces
#'
#' @param inc_matrix incidence matrix of a hypergraph
#'
#' @return a list with "valid" logical and "order" named vector
#' @export
#'
get_fpp_order <- function(inc_matrix) {
  is_valid <- TRUE
  section_order <- c()
  current_graph <- inc_matrix
  # Need to error if the inc_matrix has no sections
  # this would happen if no lines were added to the design
  if (is.null(ncol(inc_matrix))) {
    is_valid <- FALSE
    section_order <- colnames(inc_matrix)
  } else if (ncol(inc_matrix) == 0) {
    is_valid <- FALSE
    section_order <- colnames(inc_matrix)
  } else {
    for (section in seq_len(ncol(inc_matrix) - 1)) {
      node_degree <- colSums(current_graph)
      if (min(node_degree) > 1) {
        is_valid <- FALSE
      }
      node_to_remove <- names(which.min(node_degree))
      edge_to_remove <- current_graph[[node_to_remove]] == 1
      section_order <- c(section_order, node_to_remove)
      current_graph <- current_graph[!edge_to_remove,
                                     setdiff(colnames(current_graph),
                                             node_to_remove), drop = FALSE]
    }
    section_order <- c(section_order, names(current_graph))
    section_order <- setNames(rev(seq_len(length(section_order))), section_order)
  }

  return(list(valid = is_valid,
              order = section_order))
}
