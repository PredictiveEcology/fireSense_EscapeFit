# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireSense_EscapeFit",
  description = "Fit statistical models that can be used to parameterize (calibrate) 
                 the fire escape component of landscape fire models (e.g. fireSense).",
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
    #defineParameter("paramName", "paramClass", default, min, max, "parameter description"),
    defineParameter(name = "formula", class = "formula", default = NA,
                    desc = "a formula describing the model to be fitted."),
    defineParameter(name = "data", class = "character", default = "dataFireSense_EscapeFit",
                    desc = "a character vector indicating the names of objects 
                            in the `simList` environment in which to look for 
                            variables present in the model formula. `data` 
                            objects should be data.frames. If variables are not
                            found in `data` objects, they are searched in the
                            `simList` environment."),
    defineParameter(name = ".runInitialTime", class = "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start 
                    time of the simulation."),
    defineParameter(name = ".runInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between two runs of this module,
                    expressed in units of simulation time."),
    defineParameter(name = ".saveInitialTime", class = "numeric", default = NA, 
                    desc = "optional. When to start saving output to a file."),
    defineParameter(name = ".saveInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = expectsInput(
    objectName = "dataFireSense_EscapeFit",
    objectClass = "data.frame",
    desc = "One or more objects of class data.frame in which to look for variables present in the model formula.",
    sourceURL = NA_character_
  ),
  outputObjects = createsOutput(
    objectName = "fireSense_EscapeFitted",
    objectClass = "fireSense_EscapeFit",
    desc = "A fitted model object of class fireSense_EscapeFit (inheriting from the class glm)."
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.fireSense_EscapeFit = function(sim, eventTime, eventType, debug = FALSE) 
{
  switch(
    eventType,
    init = { sim <- escapeFitInit(sim) },
    run = { sim <- escapeFitRun(sim) },
    save = { sim <- escapeFitSave(sim) },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  
  invisible(sim)
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
escapeFitInit <- function(sim) 
{
  moduleName <- current(sim)$moduleName
  
  if (!is(P(sim)$formula, "formula")) 
    stop(moduleName, "> The supplied object for the 'formula' parameter is not of class formula.")
  
  sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime, moduleName, "run")
  
  if (!is.na(P(sim)$.saveInitialTime))
    sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, moduleName, "save", .last())
  
  invisible(sim)
}

escapeFitRun <- function(sim) 
{
  moduleName <- current(sim)$moduleName
  currentTime <- time(sim, timeunit(sim))
  endTime <- end(sim, timeunit(sim))
  
  # Load inputs in the data container
  # list2env(as.list(envir(sim)), envir = mod)
  
  for (x in P(sim)$data)
  {
    if (!is.null(sim[[x]])) 
    {
      if (is.data.frame(sim[[x]])) 
      {
        list2env(sim[[x]], envir = mod)
      }
      else stop(moduleName, "> '", x, "' is not a data.frame.")
    }
  }
  
  if (is.empty.model(P(sim)$formula))
    stop(moduleName, "> The formula describes an empty model.")
  
  terms <- terms.formula(P(sim)$formula)
  
  if (!attr(terms, "response"))
    stop(moduleName, "> Incomplete formula, the LHS is missing.")

  allxy <- all.vars(P(sim)$formula)
  missing <- !allxy %in% ls(mod, all.names = TRUE)
  
  if (s <- sum(missing))
    stop(
      moduleName, "> '", allxy[missing][1L], "'",
      if (s > 1) paste0(" (and ", s-1L, " other", if (s>2) "s", ")"),
      " not found in data objects nor in the simList environment."
    )
  
  model <- glm(formula = P(sim)$formula, data = mod, family = "binomial")
  class(model) <- c("fireSense_EscapeFit", class(model))
  
  sim$fireSense_EscapeFitted <- model
  
  if (!is.na(P(sim)$.runInterval))
    sim <- scheduleEvent(sim, currentTime + P(sim)$.runInterval, moduleName, "run")
  
  invisible(sim)
}


escapeFitSave <- function(sim)
{
  moduleName <- current(sim)$moduleName
  timeUnit <- timeunit(sim)
  currentTime <- time(sim, timeUnit)
  
  saveRDS(
    sim$fireSense_EscapeFitted, 
    file = file.path(paths(sim)$out, paste0("fireSense_EscapeFitted_", timeUnit, currentTime, ".rds"))
  )
  
  if (!is.na(P(sim)$.saveInterval))
    sim <- scheduleEvent(sim, currentTime + P(sim)$.saveInterval, moduleName, "save", .last())
  
  invisible(sim)
}
