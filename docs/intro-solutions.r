# What they should see
load("problems_intro.rda")
library(netdiffuseR)

plot_threshold(
  diffnet,
  vertex.col = adjustcolor(diffnet[["ItrustMyFriends"]] + 1, .7),
  edge.col   = adjustcolor("gray", .3))
legend(
  "bottomright",
  legend = c(0, 1),
  fill = c(1, 2),
  title = "ItrustMyFriends"
)