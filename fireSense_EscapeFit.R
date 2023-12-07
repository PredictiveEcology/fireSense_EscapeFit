# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireSense_EscapeFit",
  description = paste("Fit statistical models that can be used to parameterize (calibrate)",
                      "the fire escape component of landscape fire models (e.g. fireSense)."),
  keywords = c("escape probability", "fire frequency", "logistic", "fireSense"),
  authors = person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0", fireSense_EscapeFit = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_EscapeFit.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter(".runInitialTime", "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start time of the simulation."),
    defineParameter(".runInterval", "numeric", default = NA,
                    desc = paste("optional. Interval between two runs of this module,",
                                 "expressed in units of simulation time.",
                                 "By default, NA, which means that this module only runs once per simulation.")),
    defineParameter(".saveInitialTime", "numeric", default = NA,
                    desc = "optional. When to start saving output to a file."),
    defineParameter(".saveInterval", "numeric", default = NA,
                    desc = "optional. Interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity and time are not relevant."))
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "fireSense_escapeCovariates", objectClass = "data.frame",
      desc = "table of aggregated covariates with annual ignitions and escapes"),
    expectsInput("fireSense_escapeFormula", "character",
                 desc = "a formula describing the model to be fitted, as character.")
  ),
  outputObjects = createsOutput(
    objectName = "fireSense_EscapeFitted",
    objectClass = "fireSense_EscapeFit",
    desc = "formula - as a character - describing the model to be fitted."
  )
))

doEvent.fireSense_EscapeFit = function(sim, eventTime, eventType, debug = FALSE) {
  moduleName <- current(sim)$moduleName

  switch(
    eventType,
    init = {

      sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime, moduleName, "checkData")

      sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime, moduleName, "run")

      if (!is.na(P(sim)$.saveInitialTime))
        sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, moduleName, "save", .last())
    },
    checkData = {
      sim <- escapeFitInit(sim)
    },
    run = {

      sim <- escapeFitRun(sim)

      if (!is.na(P(sim)$.runInterval))
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.runInterval, moduleName, "run")
    },
    save = {
      sim <- escapeFitSave(sim)

      if (!is.na(P(sim)$.saveInterval))
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, moduleName, "save", .last())
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )

  invisible(sim)
}

escapeFitInit <- function(sim) {

  invisible(sim)
}

escapeFitRun <- function(sim) {
  moduleName <- current(sim)$moduleName
  currentTime <- time(sim, timeunit(sim))

  fireSense_escapeFormula <- as.formula(sim$fireSense_escapeFormula, env = .GlobalEnv)


  if (is.empty.model(fireSense_escapeFormula))
    stop(moduleName, "> The formula describes an empty model.")

  terms <- terms.formula(fireSense_escapeFormula)

  if (!attr(terms, "response")) {
    stop(moduleName, "> Incomplete formula, the LHS is missing.")
  }

  allxy <- all.vars(fireSense_escapeFormula)
  missing <- !allxy %in% ls(sim$fireSense_escapeCovariates, all.names = TRUE)

  if (s <- sum(missing))
    stop(
      moduleName, "> '", allxy[missing][1L], "'",
      if (s > 1) paste0(" (and ", s - 1L, " other", if (s > 2) "s", ")"),
      " not found in sim$fireSense_escapeCovariates."
    )

  model <- glm(formula = fireSense_escapeFormula, data = sim$fireSense_escapeCovariates, family = "binomial")
  class(model) <- c("fireSense_EscapeFit", class(model))

  sim$fireSense_EscapeFitted <- model

  invisible(sim)
}

escapeFitSave <- function(sim) {
  moduleName <- current(sim)$moduleName
  timeUnit <- timeunit(sim)
  currentTime <- time(sim, timeUnit)

  saveRDS(
    sim$fireSense_EscapeFitted,
    file = file.path(paths(sim)$out, paste0("fireSense_EscapeFitted_", timeUnit, currentTime, ".rds"))
  )

  invisible(sim)
}
