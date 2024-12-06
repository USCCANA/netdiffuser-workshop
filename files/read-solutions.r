library(netdiffuseR)

load("read.rda")

# Part 1: Saving the edgelist as a diffnet object ------------------------------

diffnet1 <- edgelist_to_diffnet(
  # The edgelist
  edgelist = net_edgelist[,1:2],
  t0       = net_edgelist[,"time"],
  # The dataframe
  dat      = X,
  idvar    = "idnum",
  timevar  = "year",
  toavar   = "YearAdopt"
)

#Part 2: Saving the list of adjmats as diffnet object -------------------------

# First, we need to coerce the list of adjacency matrices as an edgelist
# Notice returns the same type of object as before
net <- adjmat_to_edgelist(net_list)

# Then we can call the function edgelist_to_diffnet
diffnet1 <- edgelist_to_diffnet(
  # The edgelist
  edgelist = net[,1:2],
  t0       = net[,"time"],
  # The dataframe
  dat      = X,
  idvar    = "idnum",
  timevar  = "year",
  toavar   = "YearAdopt"
)
