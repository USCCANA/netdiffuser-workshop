# Problem 1 - Network density: simple vs threshold diffusion.
# Same setup as in the lecture, but on a denser small-world network (k = 5).

library(netdiffuseR)

# Behavioral-like diffusion
diffnet_behavior_k5 <- rdiffnet(
  n              = 500,                        
  t              = 10,                         
  seed.nodes     = "random",                   
  seed.p.adopt   = .05,                        
  seed.graph     = "small-world",              
  rgraph.args    = list(k = 5, p = .2),        
  threshold.dist = function(x) runif(1, .3, .7)
)

# Disease-like diffusion
diffnet_disease_k5 <- rdiffnet(
  n              = 500,
  t              = 10,
  seed.graph     = "small-world",
  rgraph.args    = list(k = 5, p = .2),
  seed.nodes     = "random",
  seed.p.adopt   = .05,
  rewire         = TRUE,
  threshold.dist = function(i) 1L,
  exposure.args  = list(normalized = FALSE),
  name           = "Disease spreading"
)

# Adoption curves: the disease saturates faster, the behavior gets slower with density
plot_adopters(diffnet_behavior_k5, what = "cumadopt", include.legend = FALSE,
              main = "Disease-like vs Behavioral-like diffusions (k = 5)")
plot_adopters(diffnet_disease_k5, bg = "lightblue", add = TRUE, what = "cumadopt")
legend(
  "topleft",
  legend = c("Disease", "Behavior"),
  col = c("lightblue", "tomato"),
  bty = "n", pch = 19
)

# ======= END =========

# Now let's check the density and clustering
# First we need the baseline (k = 2) network
set.seed(1213)
diffnet_behavior <- rdiffnet(
  n              = 500,
  t              = 10,
  seed.nodes     = "random",
  seed.p.adopt   = .05,
  seed.graph     = "small-world",
  rgraph.args    = list(p = .2),
  threshold.dist = function(x) runif(1, .3, .7)
)

# Now we can compute
g <- igraph::graph_from_adjacency_matrix(diffnet_behavior$graph[[1]])
g_dens <- igraph::edge_density(g)                  # share of all possible ties that exist
g_clust <- igraph::transitivity(g, type = "global") # global clustering coefficient

g_k5 <- igraph::graph_from_adjacency_matrix(diffnet_behavior_k5$graph[[1]])
g_k5_dens <- igraph::edge_density(g_k5)                  # share of all possible ties that exist
g_k5_clust <- igraph::transitivity(g_k5, type = "global") # global clustering coefficient

g_k5_dens/g_dens
g_k5_clust/g_clust
