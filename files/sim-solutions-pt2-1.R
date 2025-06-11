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

set.seed(123)

diffnet_5  <- rdiffnet(
  seed.graph   = kfamily_diffnet_21,
  seed.p.adopt = list(0.1,0.15),
)

diffnet_5

set.seed(123)

diffnet_6  <- rdiffnet(
  seed.graph   = kfamily_diffnet_21,
  seed.p.adopt = list(0.1,0.15),
  disadopt     = random_dis
)

diffnet_6
