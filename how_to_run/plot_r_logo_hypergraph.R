# Just a demonstration to take a pattern
# specified as sections and seams
# and then plot the hypergraph representation

library(dplyr)
library(tibble)
library(ggplot2)
library(HyperG)
source("functions/get_fpp_inc_matrix.R")

# Make the design incidence matrices
# the design is actually 4 blocks that get combined

# create the incidence matrix
inc_matrix <- list()
inc_matrix[[1]] <- data.frame(
  A1 =  c(0,0,0,1,0,1,0,0,1,1),
  A2 =  c(0,0,0,0,0,0,0,1,1,1),
  A3 =  c(0,0,0,0,0,1,1,1,1,0),
  A4 =  c(0,0,0,1,1,1,0,0,0,0),
  A5 =  c(0,0,0,0,0,0,1,1,0,0),
  A6 =  c(0,0,0,0,1,0,1,0,0,0),
  A7 =  c(0,0,1,0,1,0,0,0,0,0),
  A8 =  c(1,1,1,1,0,0,0,0,0,0),
  A9 =  c(0,1,1,0,0,0,0,0,0,0),
  A10 = c(1,1,0,0,0,0,0,0,0,0),
  A11 = c(1,0,0,0,0,0,0,0,0,0)
)

inc_matrix[[2]] <- data.frame(
  A1 =  c(1,1,1,1,1,0,0,0,0,0),
  A2 =  c(1,0,0,1,1,0,0,0,0,0),
  A3 =  c(0,1,1,0,1,0,0,0,0,0),
  A4 =  c(0,0,1,1,0,0,0,0,0,0),
  A5 =  c(0,0,0,1,0,1,0,0,0,0),
  A6 =  c(0,0,0,0,1,0,0,0,0,0),
  A7 =  c(0,0,0,0,0,1,1,1,0,0),
  A8 =  c(0,0,0,0,0,0,1,1,0,0),
  A9 =  c(0,0,0,0,0,0,0,1,1,1),
  A10 = c(0,0,0,0,0,0,0,0,1,1),
  A11 = c(0,0,0,0,0,0,0,0,0,1)
)

inc_matrix[[3]] <- data.frame(
  A1 =  c(1,1,0,1,1,0),
  A2 =  c(1,0,0,1,1,1),
  A3 =  c(0,1,1,1,1,0),
  A4 =  c(0,0,1,1,1,0),
  A5 =  c(0,0,0,1,0,0),
  A6 =  c(0,0,0,0,1,1),
  A7 =  c(0,0,0,0,0,1)
)

inc_matrix[[4]] <- data.frame(
  A1 =  c(1,1,1,0,0,0,0,0,0,0,0,0,0),
  A2 =  c(1,1,1,0,1,1,0,0,0,0,0,0,0),
  A3 =  c(0,1,0,0,0,1,1,0,0,0,0,0,0),
  A4 =  c(0,0,1,1,1,0,0,0,0,0,0,0,0),
  A5 =  c(0,0,0,1,1,0,0,0,0,0,1,1,0),
  A6 =  c(0,0,0,0,1,1,0,1,0,0,1,0,0),
  A7 =  c(0,0,0,0,0,1,1,1,1,0,0,0,0),
  A8 =  c(0,0,0,0,0,0,1,0,1,1,0,0,0),
  A9 =  c(0,0,0,0,0,0,0,1,1,0,1,0,0),
  A10 = c(0,0,0,0,0,0,0,0,1,1,0,0,0),
  A11 = c(0,0,0,0,0,0,0,0,0,1,0,0,0),
  A12 = c(0,0,0,0,0,0,0,0,0,0,1,1,1),
  A13 = c(0,0,0,0,0,0,0,0,0,0,0,1,1),
  A14 = c(0,0,0,0,0,0,0,0,0,0,0,0,1)
)
# use HyperG to make hypergraph and plot
hypergraphs <- lapply(
  inc_matrix,
  function(x)
    hypergraph_from_incidence_matrix(as.matrix(x)))

lapply(hypergraphs, plot)


