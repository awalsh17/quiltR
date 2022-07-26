# Idea 1: Generate some random FPP blocks!
# Idea 2: Generate FPP blocks with constraints

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
  ~section, ~line, ~start,  ~stop,
  "A",      1,    c(0,  0), c(0, 10),
  "A",      2,    c(0, 10), c(10,10),
  "A",      3,    c(10,10), c(10, 0),
  "A",      4,    c(10, 0), c(0,  0)
)
# choose a section to split
section <- "A"
# choose the lines to split
split_lines <- c(1, 2)

# split_coord <- list(c(0, 5), c(5, 10))
split_coord <- list(get_fpp_point(design_init[1,], 5),
                    get_fpp_point(design_init[2,], 5))

# example to split the A section
new_design <- split_fpp_section(design_init,
                            split_section = "A",
                            split_lines,
                            split_coord)

# We can plot with ggplot2
plot_fpp_block(design_init)
plot_fpp_block(new_design)

# finally, you probably want to recursively repeat this
# n times to get a fun design. Use make_fpp_design()

# Basic usage
test_design <- make_fpp_design(n = 4, method = "golden")
test_design$plot + labs(title = "A random FPP")

# you can also redo the plot with the sections colored
# using fabrics from kona and art gallery fabrics
fabrics <- readRDS("colors/annotated_fabric_colors.Rds")
plot_fpp_block(test_design$design,
               test_design$fpp_order,
               fill_sections = TRUE,
               palette = fabrics[1:20,])

# You can start with a different shape block
triangle_init <- tribble(
  ~section, ~line, ~start,  ~stop,
  "A",      1,    c(0,  0), c(5, 10),
  "A",      2,    c(5, 10), c(10, 0),
  "A",      3,    c(10, 0), c(0,  0)
)
plot_fpp_block(triangle_init)
# use this as the initial shape
test_design <- make_fpp_design(initial_design = triangle_init,
                               n = 4)
test_design$plot
# replot with random colors
plot_fpp_block(test_design$design,
               test_design$fpp_order,
               fill_sections = TRUE,
               palette = fabrics[fabrics$G > 0.75,])

# plot the hypergraph -----
# tested two CRAN packages: rhype and HyperG
# Decided to work with HyperG

inc_matrix <- get_fpp_inc_matrix(test_design$design,
                                 initial_design = triangle_init)
# use HyperG to make hypergraph and plot
hype2 <- hypergraph_from_incidence_matrix(as.matrix(inc_matrix))
plot(hype2)

# next steps ------
# 1: See the make_constrained_fpp.R script
# 2: I could change around the splitting to avoid tiny areas and do more recurvise
# designs

# print 30 blocks for making a quilt
# repeat 30 x
out_dir <- "~/Downloads/quilt_rand_mid/"
for (b in 1:30) {
  n_b <- sample(c(3:5), 1)
  block_b <- make_fpp_design(n = n_b, method = "midpoint")
  ggsave(block_b$plot + labs(title = NULL),
         filename = paste0(out_dir, "block", b, ".pdf"))
}
