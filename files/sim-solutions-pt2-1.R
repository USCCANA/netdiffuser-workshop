# The following code shows how to build a disadoption function that randomly 
# selects 10% of the adopters at time `t - 1`:
  
random_dis <- function(expo, cumadopt, time) {
  
  # Number of behaviors
  num_of_behaviors <- dim(cumadopt)[3]
  
  # Making room for the disadopted nodes: list(integer(), integer())
  list_disadopt <- replicate(num_of_behaviors, integer())
  
  # We iterate through the behaviors
  for (q in 1:num_of_behaviors) {
    
    # Identifying the adopters at time t-1
    adopters_old <- which(cumadopt[, time - 1, q] == 1)
    
    if (length(adopters_old) != 0) {
      
      # selecting 10% of adopters to disadopt
      list_disadopt[[q]] <- sample(
        adopters_old,
        round(0.10 * length(adopters_old)
        )
      )
    }
    
  }
  return(list_disadopt)
}

# To **simulate** a diffusion process **with disadoption**, we can use the 
# `rdiffnet` function as follows:

library(netdiffuseR)

# Using the Watts-Strogatz model to create a seed graph

n <- 200
k <- 8
t <- 10
graph <- rgraph_ws(n, k, p=.3)  # Watts-Strogatz model

set.seed(123)

diffnet_ws <- rdiffnet(
  seed.graph   = graph,
  t            = t,
  seed.p.adopt = list(0.1, 0.15)
)
diffnet_ws

set.seed(123)

diffnet_ws_random_dis <- rdiffnet(
  seed.graph   = graph,
  t            = t,
  seed.p.adopt = list(0.1, 0.15),
  disadopt     = random_dis
)
diffnet_ws_random_dis

# Using the k-family model to create a seed graph

kfamily_diffnet_21 <- kfamilyDiffNet[kfamilyDiffNet$vertex.static.attrs$village == 21,]

set.seed(123)

diffnet_kfam  <- rdiffnet(
  seed.graph   = kfamily_diffnet_21,
  seed.p.adopt = list(0.1,0.15),
)

diffnet_kfam

set.seed(123)

diffnet_kfam_random_dis  <- rdiffnet(
  seed.graph   = kfamily_diffnet_21,
  seed.p.adopt = list(0.1,0.15),
  disadopt     = random_dis
)

diffnet_kfam_random_dis
