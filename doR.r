# doR: A complete basic foreach adapter using the 'callr' package
#
# This toy adapter does not even run code in parallel! It's designed to
# illustrate most of the key parts of writing a foreach adapter as simply as
# possible.

library(foreach)
library(iterators)
library(callr)    # for `r` used below
library(future)   # for `getGlobalsAndPackages` used below

# Register doR with %doparallel%
# The run-time 'chunkSize' option specifies an upper bound on the number
# of loop iterations to be processed per task.
registerDoR <- function(chunkSize = 1)
{
  setDoPar(doR, data = list(chunkSize = chunkSize))
}

# The %dopar% API:
# obj    foreach object
# expr   the expression to evaluate
# envir  the local enclosing environment
# data   run-time options
doR <- function(obj, expr, envir, data)
{
  if (!inherits(obj, "foreach")) stop("obj must be a foreach object")
  it <- iter(obj)
# Set up the accumulator function that will process returned results
  accumulator <- makeAccum(it)
# Construct a list of iterator variable values
  argsList <- as.list(it)

# Set up an environment for export within with to evaluate expr Just in case we're called from within a function,
# we need an environment will all the optional `...` arguments (if any).
  exportenv <- tryCatch({
    qargs <- quote(list(...))
    as.environment(eval(qargs, envir))
  }, error = function(e) {
    new.env(parent = emptyenv())
  })

# The foreach `getexports` function automagically determines a list of R objects required by 'expr'.  `getexports`
# is not functional, it modifies its 'exportenv' argument. Beta version of foreach use the `getGlobalsAndPacakges`
# function from the future package. We use that explicitly below to discover required packages.  We combine
# those required packages with explicitly listed packages in 'obj' and the optional package parent environment for
# a complete list of require packages.
  noexport <- union(obj$noexport, obj$argnames)
  getexports(expr, exportenv, envir, bad=noexport)
  packages <- unique(c(obj$packages,
    future::getGlobalsAndPackages(expr, envir)$packages, obj$parentenv))

# There may still be some additional *explicitly* specified objects to  export. These require some additional
# processing, similar to what  `getexports` does under the hood. It would be nice to expand the  foreach
# `getexports` function to handle these objects too!
  export <- setdiff(obj$export, unique(c(ls(exportenv), obj$noexport)))
  for (sym in export) {
    if (!exists(sym, envir, inherits=TRUE))
      stop(sprintf('unable to find variable "%s"', sym))
    val <- get(sym, envir, inherits=TRUE)
    if (is.function(val) &&
        (identical(environment(val), .GlobalEnv) ||
         identical(environment(val), envir))) {
      # Changing this function's environment to exportenv allows it to
      # access/execute any other functions defined in exportenv.
      environment(val) <- exportenv
    }
    assign(sym, val, pos = exportenv, inherits = FALSE)
  }


# A simple initialization + evaluation function run by the worker  This loads required packages, optionally
# sets a parent package  environment (when called from *within* a package namespace),  and evaluates the
# expression. It does not set a random seed, something you probably would want to do in an actual adapter
# for real-world use (via, for example, R's L'Ecuyer RNG stream).
  taskInit <- function(expr, envir, packages, parentenv, args)
  {
    function() {
      for (p in packages) library(p, character.only = TRUE)
      parent.env(envir) <- if(is.null(parentenv)) globalenv() else
        getNamespace(parentenv)
      Map(function(a) {
        Map(function(n) assign(n, a[[n]], pos = envir), names(a))
        serialize(eval(expr, envir), NULL)  # the result for this set of iterations
      }, args)
    }
  }

# Proceed through the loop in chunks of arguments at most 'chunksize' specified in the `setDoPar` function
# above. Note that we could have used callr's  `r` function to directly call `taskInit` and retrieve the
# result; the serialization steps here are in fact redundant.  But in a more typical setting, serialization
# is probably desired (the coordinator simply hands out  binary blobs to the workers for evaluation).
  for(i in seq(ceiling(length(argsList) / data$chunkSize))) {
    idx <-seq(from = (i - 1) * data$chunkSize + 1, to = min(i * data$chunkSize, length(argsList)))
    go <- serialize(taskInit(expr, exportenv, packages, packageName(envir), argsList[idx]), NULL)
    val <- Map(unserialize, callr::r(function(blob) unserialize(blob)(), args = list(blob = go), show = TRUE))
    accumulator(val, idx) # basically, invoke the .combine function
    i <- i + 1
  }
# return the accumulated results
  getResult(it)
}
