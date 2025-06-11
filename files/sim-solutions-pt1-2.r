# To solve this problem we will use rdiffnet_multiple

# Loading the package
library(netdiffuseR)

# First, some common parameters:
adoption <- function(x) cumulative_adopt_count(x)[2,nslices(x)]
nsim     <- 1e3L
ncores   <- 4

# Small-world networks ---------------------------------------------------------
ans_sw_rand <- rdiffnet_multiple(
  R          = nsim, 
  statistic  = adoption,
  ncpus      = ncores,
  n          = 100,
  t          = 10,
  rewire     = FALSE,
  stop.no.diff = FALSE,
  seed.graph = "small-world",
  seed.nodes = "random"
)

ans_sw_cent <- rdiffnet_multiple(
  R          = nsim, 
  statistic  = adoption,
  ncpus      = ncores,
  n          = 100,
  t          = 10,
  rewire     = FALSE,
  stop.no.diff = FALSE,
  seed.graph = "small-world",
  seed.nodes = "central"
)

ans_sw_marg <- rdiffnet_multiple(
  R          = nsim, 
  statistic  = adoption,
  ncpus      = ncores,
  n          = 100,
  t          = 10,
  rewire     = FALSE,
  stop.no.diff = FALSE,
  seed.graph = "small-world",
  seed.nodes = "marginal"
)

# Scale-free- networks ---------------------------------------------------------
ans_sf_rand <- rdiffnet_multiple(
  R          = nsim, 
  statistic  = adoption,
  ncpus      = ncores,
  n          = 100,
  t          = 10,
  rewire     = FALSE,
  stop.no.diff = FALSE,
  seed.graph = "scale-free",
  seed.nodes = "random"
)

ans_sf_cent <- rdiffnet_multiple(
  R          = nsim, 
  statistic  = adoption,
  ncpus      = ncores,
  n          = 100,
  t          = 10,
  rewire     = FALSE,
  stop.no.diff = FALSE,
  seed.graph = "scale-free",
  seed.nodes = "central"
)

ans_sf_marg <- rdiffnet_multiple(
  R          = nsim, 
  statistic  = adoption,
  ncpus      = ncores,
  n          = 100,
  t          = 10,
  rewire     = FALSE,
  stop.no.diff = FALSE,
  seed.graph = "scale-free",
  seed.nodes = "marginal"
)

# Bernoulli graphs -------------------------------------------------------------

ans_er_rand <- rdiffnet_multiple(
  R          = nsim, 
  statistic  = adoption,
  ncpus      = ncores,
  n          = 100,
  t          = 10,
  rewire     = FALSE,
  stop.no.diff = FALSE,
  seed.graph = "bernoulli",
  seed.nodes = "random"
)

ans_er_cent <- rdiffnet_multiple(
  R          = nsim, 
  statistic  = adoption,
  ncpus      = ncores,
  n          = 100,
  t          = 10,
  rewire     = FALSE,
  stop.no.diff = FALSE,
  seed.graph = "bernoulli",
  seed.nodes = "central"
)

ans_er_marg <- rdiffnet_multiple(
  R          = nsim, 
  statistic  = adoption,
  ncpus      = ncores,
  n          = 100,
  t          = 10,
  rewire     = FALSE,
  stop.no.diff = FALSE,
  seed.graph = "bernoulli",
  seed.nodes = "marginal"
)


op <- par(mfrow = c(2, 2))
boxplot(
  cbind(Central = ans_sw_cent, Marginal = ans_sw_marg, Random = ans_sw_rand),
  main = "Small-world"
)
boxplot(
  cbind(Central = ans_sf_cent, Marginal = ans_sf_marg, Random = ans_sf_rand),
  main = "Scale-free"
)
boxplot(
  cbind(Central = ans_er_cent, Marginal = ans_er_marg, Random = ans_er_rand),
  main = "Bernoulli"
)
par(op)