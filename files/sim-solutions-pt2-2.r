library(netdiffuseR)

# Using k-family data to create a seed graph

data(kfamily)
kfamily_21 <- subset(kfamily, village == 21)
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

kfamily_diffnet_21[['age']]

# Disadoption function: younger nodes are more likely to disadopt

young_dis <- function(expo, cumadopt, time) {
  
  # Number of behaviors
  num_behaviors <- dim(cumadopt)[3]
  
  # Making room for the disadopted nodes:
  list_disadopt <- replicate(num_behaviors, integer())
  
  # Ages of all nodes
  ages <- kfamily_diffnet_21[['age']]
  min_age <- min(ages)
  max_age <- max(ages)
  
  for (q in 1:num_behaviors) {
    # Identifying the adopters at time t-1
    adopters <- which(cumadopt[, time-1, q] == 1)
    
    if (length(adopters) > 0) {
      # linear probability: younger = higher chance
      prob <- (max_age - ages[adopters]) / (max_age - min_age)
      
      list_disadopt[[q]] <- adopters[runif(length(adopters)) < prob]
    }
  }
  return(list_disadopt)
}

# Simulating the diffusion process with disadoption
set.seed(111)
diffnet_young_dis  <- rdiffnet(
  seed.graph   = kfamily_diffnet_21,
  seed.p.adopt = 0.20,
  disadopt     = young_dis
)

# Checking the disadoption
cor(kfamily_diffnet_21[['age']], rowSums(diffnet_young_dis$cumadopt))