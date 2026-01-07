#' Derive burial dates from deposition dates in simulated profiles
#'
#' This routine derives burial dates (`bdate`) from deposition dates (`ddate`).
#' Optionally, burial dates can be adjusted to align more closely with human
#' interpretation (see Details in [guessDatetagsSimple]).
#'
#' @param x a [snowprofileSet], [snowprofile] or [snowprofileLayers] object
#' @param adjust_bdates boolean switch to compute bdates similar to human interpretation.
#' @param ... passed on to subsequent methods
#' @return The input object will be returned with the column `bdate` added to the profile layers
#' @author fherla
#'
#' @export
deriveBDate <- function(x, adjust_bdates = TRUE, ...) UseMethod("deriveBDate")

#' @describeIn deriveBDate for [snowprofileSet]s
#' @export
deriveBDate.snowprofileSet <- function(x, adjust_bdates = TRUE, ...) {
  return(snowprofileSet(lapply(x, deriveBDate.snowprofile, adjust_bdates = adjust_bdates, ...)))
}

#' @describeIn deriveBDate for [snowprofile]s
#' @export
deriveBDate.snowprofile <- function(x, adjust_bdates = TRUE, checkMonotonicity = FALSE, ...) {
  x$layers <- deriveBDate.snowprofileLayers(x$layers, adjust_bdates = adjust_bdates,
                                            checkMonotonicity = checkMonotonicity, ...)
  return(x)
}

#' @describeIn deriveBDate for [snowprofileLayers]
#' @param checkMonotonicity check ascending order of layers. This acts as a check for whether multiple layers objects are stacked, which is not allowed.
#' @export
deriveBDate.snowprofileLayers <- function(x, adjust_bdates = TRUE, checkMonotonicity = TRUE, ...) {

  ## --- Assertions / Initializations ----
  layers <- x
  nL <- nrow(layers)
  if (checkMonotonicity) {
    if (!all(diff(layers$height) > 0)) stop("Either your snowprofileLayers object is malformatted, or you're stacking multiple layers objects, which is not allowed!")
  }

  ## ---Compute bdate----
  if ("ddate" %in% names(layers) & nL > 1) {

    ## don't create bdate for unobserved basal layer:
    lag <- 0
    if (hasUnobservedBasalLayer(layers)) lag <- 1

    ## derive bdate from ddate:
    layers[(1+lag):(nL-1), "bdate"] <- layers[(2+lag):nL, "ddate"]
    ## check
    if (any(layers[1:(nL-1), "bdate"] < layers[1:(nL-1), "ddate"], na.rm = TRUE)) stop("Layer order incorrect, layers get buried before they form!")

    ## make bdates more realistic: layers with identical ddate, gtype, & hardness get the same (oldest) bdate:
    if (adjust_bdates) {
      vars2test <- c("gtype", "hardness")
      ## test for identical values in select layer properties:
      olderBdateRequired <- c(rowSums(layers[1:(nL-1), vars2test] == layers[2:nL, vars2test]) == length(vars2test), FALSE)
      ## test for ddates within 12 hours and assume those to be identical
      olderBdateRequired_ddate <- c(layers[2:nL, "ddate"] - layers[1:(nL-1), "ddate"] < as.difftime(12, units = "hours"), FALSE)
      ## require both conditions to be met
      olderBdateRequired <- olderBdateRequired & olderBdateRequired_ddate
      ## calculate which layers' bdates remain unchanged, and which ones get adjusted
      idx_properBdates <- which(olderBdateRequired == FALSE | is.na(olderBdateRequired))
      idx_toAdjust <- which(olderBdateRequired == TRUE)
      if (length(idx_toAdjust) > 0) {
        takeBdateFrom <- sapply(idx_toAdjust, function(i) idx_properBdates[idx_properBdates > i][1])
        layers$bdate[idx_toAdjust] <- layers$bdate[takeBdateFrom]
      }
    }
  } else if (nL == 1) {
    layers$bdate <- as.POSIXct(NA)
  } else if (!"ddate" %in% names(layers)) {
    warning("No ddate info available, returning NA bdates")
    layers$bdate <- as.POSIXct(NA)
  }

  return(layers)
}


#' Guess datetags from deposition dates in simulated profiles
#'
#' This routine provides a simple heuristic for assigning datetags to layers.
#' Datetags usually are deposition dates for crust layers, and burial dates for
#' other weak layers (e.g., SH, FC). If no datetags can be derived, a datetag
#' column of NAs will be added. Burial dates (`bdate`) are computed via
#' [deriveBDate].
#'
#' @param x a [snowprofileSet], [snowprofile] or [snowprofileLayers] object
#' @param adjust_bdates boolean switch to compute bdates similar to human interpretation. see Details.
#' @param ... passed on to subsequent methods
#' @return The input object will be returned with the columns `datetag` and `bdate` added to the profile layers
#' @author fherla
#'
#' @export
guessDatetagsSimple <- function(x, adjust_bdates = TRUE, ...) UseMethod("guessDatetagsSimple")

#' @describeIn guessDatetagsSimple for [snowprofileSet]s
#' @export
guessDatetagsSimple.snowprofileSet <- function(x, adjust_bdates = TRUE, ...) {
  return(snowprofileSet(lapply(x, guessDatetagsSimple.snowprofile, adjust_bdates = adjust_bdates, ...)))
}

#' @describeIn guessDatetagsSimple for [snowprofile]s
#' @export
guessDatetagsSimple.snowprofile <- function(x, adjust_bdates = TRUE, checkMonotonicity = FALSE, ...) {
  x$layers <- guessDatetagsSimple.snowprofileLayers(x$layers, adjust_bdates = adjust_bdates,
                                                    checkMonotonicity = checkMonotonicity, ...)
  return(x)
}

#' @describeIn guessDatetagsSimple for [snowprofileLayers]
#' @param checkMonotonicity check ascending order of layers. This acts as a check for whether multiple layers objects are stacked, which is not allowed.
#' @export
guessDatetagsSimple.snowprofileLayers <- function(x, adjust_bdates = TRUE, checkMonotonicity = TRUE, ...) {

  ## --- Assertions / Initializations ----
  layers <- x
  nL <- nrow(layers)

  ## ---Calculations----
  if ("ddate" %in% names(layers) & nL > 1) {
    layers <- deriveBDate.snowprofileLayers(layers, adjust_bdates = adjust_bdates,
                                            checkMonotonicity = checkMonotonicity)
    ## merge ddate and bdate into datetag:
    layers[, "datetag"] <- as.Date(as.character(layers[, "bdate"]))  # double conversion to prevent time zone issues!
    if ("gtype" %in% names(layers)) {
      crust <- layers[, "gtype"] %in% c("MFcr", "IF")
      layers[crust, "datetag"] <- as.Date(as.character(layers[crust, "ddate"]))
    }

  } else if (nL == 1) {
    layers$bdate <- as.POSIXct(NA)
    layers$datetag <- as.Date(NA)
  } else if (!"ddate" %in% names(layers)) {
    warning("No ddate info available, returning NA datetags")
    layers$bdate <- as.POSIXct(NA)
    layers$datetag <- as.Date(NA)
  }

  return(layers)
}


#' Derive datetag from deposition dates in simulated profiles
#'
#' This routine is deprecated; use [guessDatetagsSimple], [deriveBDate], and/or [assignDatetags] instead.
#' This routine derives the datetags of simulated snow profile layers from deposition dates. Datetags usually are deposition dates
#' for crust layers, and burial dates for other weak layers (e.g., SH, FC). If no datetags can be derived, a datetag column of NAs will
#' nevertheless be added to the snowprofile layers. The routine also adds a `bdate` column for burial dates that are calculated along the way.
#'
#' `bdate`s are computed by taking the `ddate` of the overlying layer. For snowpack simulations with thin layer
#' resolution, this approach yields very similar `ddate`s and `bdate`s for most layers, since most layers form and
#' instantly get buried by another layer of the same storm. To make `bdate`s more similar to human interpretation,
#' `bdate`s can be adjusted, so that (similar) layers with the same `ddate` (i.e., same storm) inherit the same `bdate`
#' (similar means: identical gtype & hardness).
#'
#' @param x a [snowprofileSet], [snowprofile] or [snowprofileLayers] object
#' @param adjust_bdates boolean switch to compute bdates similar to human interpretation. see Details.
#' @param ... passed on to subsequent methods
#' @return The input object will be returned with the columns `datetag` and `bdate` added to the profile layers
#' @author fherla
#' @note Deprecated; use [guessDatetagsSimple] and/or [assignDatetags] instead.
#'
#' @export
deriveDatetag <- function(x, adjust_bdates = TRUE, ...) {
  .Deprecated("guessDatetagsSimple")
  UseMethod("deriveDatetag")
}

#' @describeIn deriveDatetag for [snowprofileSet]s
#' @export
deriveDatetag.snowprofileSet <- function(x, adjust_bdates = TRUE, ...) {
  return(guessDatetagsSimple.snowprofileSet(x, adjust_bdates = adjust_bdates, ...))
}

#' @describeIn deriveDatetag for [snowprofile]s
#' @export
deriveDatetag.snowprofile <- function(x, adjust_bdates = TRUE, ...) {
  return(guessDatetagsSimple.snowprofile(x, adjust_bdates = adjust_bdates, ...))
}

#' @describeIn deriveDatetag for [snowprofileLayers]
#' @param checkMonotonicity check ascending order of layers. This acts as a check for whether multiple layers objects are stacked, which is not allowed.
#' @export
deriveDatetag.snowprofileLayers <- function(x, adjust_bdates = TRUE, checkMonotonicity = TRUE, ...) {
  return(guessDatetagsSimple.snowprofileLayers(x, adjust_bdates = adjust_bdates, checkMonotonicity = checkMonotonicity, ...))
}
