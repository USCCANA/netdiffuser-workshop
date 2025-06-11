# To solve this problem we will use rdiffnet_multiple

# Loading the package
library(netdiffuseR)

# Loading the data
kfamily_21 <- subset(kfamily, village == 21)

# Converting the survey data to a diffnet object
kfamily_diffnet_21 <- survey_to_diffnet(
  dat      = kfamily_21,
  idvar    = "id",
  netvars  = c(
    "net11", "net12", "net13", "net14", "net15", 
    "net21", "net22", "net23", "net24", "net25",
    "net31", "net32", "net33", "net34", "net35"), 
  toavar   = "toa",
  groupvar = "village"
)

# First, some common parameters:
adoption <- function(x) cumulative_adopt_count(x)[2,nslices(x)]
nsim     <- 500
ncores   <- 4

# Korean Family Planning - village 21 network ----------------------------------

# random seeds
sims_rand <- rdiffnet_multiple(
  seed.graph = kfamily_diffnet_21,
  R          = nsim, 
  statistic  = adoption,
  ncpus      = ncores,
  stop.no.diff = FALSE,
  seed.nodes = "random"
)
# central seeds
sims_cent <- rdiffnet_multiple(
  seed.graph = kfamily_diffnet_21,
  R          = nsim, 
  statistic  = adoption,
  ncpus      = ncores,
  stop.no.diff = FALSE,
  seed.nodes = "central"
)
# marginal seeds
sims_marg <- rdiffnet_multiple(
  seed.graph = kfamily_diffnet_21,
  R          = nsim, 
  statistic  = adoption,
  ncpus      = ncores,
  stop.no.diff = FALSE,
  seed.nodes = "marginal"
)
# Plotting the results
boxplot(
  cbind(Random = sims_rand, Central = sims_cent, Marginal = sims_marg),
  main = "Korean Family Planning - village 21 network"
)
