library(SpaDES)

modulePath <- normalizePath("..")

# Define simulation parameters
times <- list(start = 1, end = 1, timeunit = "year")
modules <- list("fireSense_EscapeFit")
paths <- list(
  modulePath = modulePath
)


# Examples of model formula
formula <- cbind(escaped, n_fires - escaped) ~ MDC_07 + cn + dt + wt + ot

# Define module parameters
parameters <- list(
  fireSense_EscapeFit = list(
    formula = formula,
    data = "dataFireSense_EscapeFit"
  )
)

# Define from where and how data will be loaded in the simList environment
inputs <- data.frame(
  objectName = "dataFireSense_EscapeFit",
  file = normalizePath("../inputs/dataFireSense_EscapeFit.rds"),
  fun = "readRDS",
  package = "base",
  loadTime = 1
)

# Create the simList
sim <- simInit(
  times = times,
  modules = modules,
  params = parameters,
  paths = paths,
  inputs = inputs
)

sim <- spades(sim)
sim$fireSense_EscapeFitted

