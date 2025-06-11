declining_fashion <- function(expo, cumadopt, time) {

  # Number of behaviors
  num_of_behaviors <- dim(cumadopt)[3]

  # Number of nodes
  num_of_nodes <- dim(cumadopt)[1]

  # Making room for the disadopted nodes
  list_disadopt <- list()

  sigmoide <- function(x, midpoint = 4, steepness = 1) {
    1 / (1 + exp(-steepness * (x - midpoint)))
  }

  # We iterate through the behaviors
  for (q in 1:num_of_behaviors) {

    # Cumulative adoption matrix for behavior Q
    cumadopt_q <- cumadopt[, , q]

    # Calculating the total time of adoption
    cumulative_time <- sapply(seq_len(num_of_nodes), function(i) {

      if (any(cumadopt_q[i, ] == 1)) {

        # Length of the vector from the first adoption to the currect
        length(seq(which(cumadopt_q[i, ] == 1)[1], time))

      } else {

        0
      }
    })

    # Looking the adopters
    adopters_q <- which(cumadopt[, time, q] == 1)

    # Calculating probability of disadoption based on cumulative_time
    prob_dis <- rep(0, num_of_nodes)
    prob_dis[adopters_q] <- sigmoide(cumulative_time[adopters_q], midpoint = 4, steepness = 1)

    # We select 2 nodes based on prob_dis
    disadopters_q <- sample(seq_len(num_of_nodes), size = 2, prob = prob_dis)

    # Adding disadopters_q to list_disadopt
    if (length(disadopters_q) != 0){
      set.seed(123)
      list_disadopt[[q]] <- disadopters_q
    } else {
      list_disadopt[[q]] <- integer()
    }
  }

  return(list_disadopt)
}


set.seed(1231)

n <- 200
t <- 10

diffnet_declining <- rdiffnet(
  n, t,
  seed.p.adopt = list(0.1, 0.15),
  disadopt = declining_fashion
)

diffnet_declining
