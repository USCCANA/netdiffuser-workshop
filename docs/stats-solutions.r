library(netdiffuseR)
load("stats.rda")

# Part 1: ----------------------------------------------------------------------

# Calculating adoption matrix
toamat <- toa_mat(X[,"toa"])

# Computing geodesic, and generating the inverse
G <- approx_geodesic(W, 100)
Ginv <- G

Ginv@x <- 1/Ginv@x # We do it like this b/c is a sparse matrix

# Now we can compute Moran's I
moran(toamat$cumadopt[,1], Ginv)


# Part 2 -----------------------------------------------------------------------

# Creating the diffnet object
dn <- new_diffnet(W, toa=X[,2], vertex.static.attrs = X[,1,drop=FALSE])

# Computing lagged exposure, and computing adoption
dn[["cohesive_exposure"]] <- cbind(NA, exposure(dn)[,-nslices(dn)])
dn[["adopted"]]           <- dn$cumadopt

# Estimating the model
dat <- as.data.frame(dn)
ans <- glm(adopted ~ cohesive_exposure + Measure + factor(per),
           family = binomial(link="logit"),
           data = dat,
           subset = is.na(toa) | (per <= toa)
)

summary(ans)

# This is equivalent to run a diffnet model
summary(diffreg(dn ~ exposure(lags = 1) + Measure + factor(per)))
