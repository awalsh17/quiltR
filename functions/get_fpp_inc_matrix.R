# need to first join the section lines that make one larger
# design line (they have the same formula and overlap)
# do not remove the duplicate lines across > 1 section
# use merge_fpp_lines

get_fpp_inc_matrix <- function(design, initial_design = NULL) {
  if (is.null(initial_design)) {
    initial_design <- tribble(
      ~section, ~line, ~start,  ~stop,
      "A",      1,    c(0,  0), c(0, 10),
      "A",      2,    c(0, 10), c(10,10),
      "A",      3,    c(10,10), c(10, 0),
      "A",      4,    c(10, 0), c(0,  0)
    )
  }
  merged_design <- merge_fpp_lines(design, initial_design)
  # columns are sections/vertices, rows are lines/hyperedges
  inc_matrix <- as.data.frame.array(with(merged_design, table(line, section)))
  # Trim all the self-loops that are just one edge/ one node
  inc_matrix <- inc_matrix[rowSums(inc_matrix) > 1, ]
  # returns a data.frame
  return(inc_matrix)
}
