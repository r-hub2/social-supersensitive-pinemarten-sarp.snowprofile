#' Assign layers in simulated snow profiles to precomputed datetags
#'
#' This routine assigns each layer of simulated snow profiles to precomputed datetags. Conceptually, each datetag represents a date
#' when the surface layers get buried by new snow. To assign layers to datetags, the routine chooses the closest datetag that is older than the
#' layer formation date. This routine needs precomputed datetags, which are currently computed based on precipitation patterns
#' by functions in other packages. Once layers are assigned to datetags, layers from different locations or times can easily
#' be grouped by their datetags.
#'
#' As a side effect and to issue warnings when layers might not be assigned to meaningful datetags,
#' the routine also computes the exact burial dates (`bdate`s) of layers, which is the `ddate` of the overlying layer.
#' For snowpack simulations with thin layer
#' resolution, this approach yields very similar `ddate`s and `bdate`s for most layers, since most layers form and
#' instantly get buried by another layer of the same storm. Before implementing the aforementioned approach of pre-computing datetags
#' based on weather patterns, this routine attempted to make `bdate`s more similar to human interpretation by adjusting
#' `bdate`s, so that (similar) layers with the same `ddate` (i.e., same storm) inherit the same `bdate`
#' (similar means: identical gtype & hardness). This feature is still available by setting `adjust_bdates` to TRUE, which is not
#' recommended, though.
#'
#' @param x a [snowprofileSet], [snowprofile] or [snowprofileLayers] object
#' @param potentialDatetags a character or Date array of all potential datetags of the season. This array is currently provided by
#' `sarp.2021.herla.snowprofilevalidation::derivePotentialDatetags()` only.
#' @param vdate date when the profile and layer information is valid (i.e., `profile$date`)
#' @param adjust_bdates boolean switch to compute bdates that are more similar to human interpretation. Deprecated and not recommeneded, see Details.
#' @param computeBDate compute burial dates (`bdate`s) from layer deposition dates?
#' @param ... passed on to subsequent methods
#' @return The input object will be returned with the columns `datetag` and `bdate` added to the profile layers
#' @author fherla
#' @seealso [deriveBDate], [guessDatetagsSimple]
#'
#' @examples
#' ## TODO: create a meaningful example!
#'
#' @export
assignDatetags <- function(x, potentialDatetags, vdate = NULL, adjust_bdates = FALSE,
                           computeBDate = TRUE, checkMonotonicity = ifelse(computeBDate, TRUE, FALSE), ...) {
  UseMethod("assignDatetags")
}

#' @describeIn assignDatetags for [snowprofileSet]s
#' @export
assignDatetags.snowprofileSet <- function(x, potentialDatetags, vdate = NULL, adjust_bdates = FALSE,
                                          computeBDate = TRUE, checkMonotonicity = ifelse(computeBDate, TRUE, FALSE), ...) {
  warn_messages <- character(0)
  warn_profiles <- integer(0)
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    vdate_i <- vdate
    if (!is.null(vdate) && length(vdate) == length(x)) vdate_i <- vdate[i]
    out[[i]] <- withCallingHandlers(
      assignDatetags.snowprofile(x[[i]], potentialDatetags,
                                 vdate = vdate_i, adjust_bdates = adjust_bdates,
                                 computeBDate = computeBDate, checkMonotonicity = checkMonotonicity, ...),
      warning = function(w) {
        warn_messages <<- c(warn_messages, conditionMessage(w))
        warn_profiles <<- c(warn_profiles, i)
        invokeRestart("muffleWarning")
      }
    )
  }
  if (length(warn_messages) > 0) {
    msg_counts <- sort(table(warn_messages), decreasing = TRUE)
    msg_summary <- paste(sprintf("%dx %s", as.integer(msg_counts), names(msg_counts)), collapse = "; ")
    warning(paste0("assignDatetags: warnings from ", length(unique(warn_profiles)),
                   " profiles (", msg_summary, ")."),
            call. = FALSE)
  }
  return(snowprofileSet(out))
}

#' @describeIn assignDatetags for [snowprofile]s
#' @export
assignDatetags.snowprofile <- function(x, potentialDatetags, vdate = NULL, adjust_bdates = FALSE,
                                       computeBDate = TRUE, checkMonotonicity = ifelse(computeBDate, TRUE, FALSE), ...) {
  if (is.null(vdate)) vdate <- x$date
  x$layers <- assignDatetags.snowprofileLayers(x$layers, potentialDatetags, vdate = vdate,
                                               adjust_bdates = adjust_bdates, computeBDate = computeBDate,
                                               checkMonotonicity = checkMonotonicity, ...)
  return(x)
}

#' @describeIn assignDatetags for [snowprofileLayers]
#' @param checkMonotonicity check ascending order of layers. This acts as a check for whether multiple layers objects are stacked, which is not allowed.
#' @export
assignDatetags.snowprofileLayers <- function(x, potentialDatetags, vdate, adjust_bdates = FALSE, computeBDate = TRUE, checkMonotonicity = ifelse(computeBDate, TRUE, FALSE), ...) {

  ## --- Assertions / Initializations ----
  layers <- x
  nL <- nrow(layers)
  if (checkMonotonicity) {
    if (!all(diff(layers$height) > 0)) stop("Either your snowprofileLayers object is malformatted, or you're stacking multiple layers objects, which is not allowed when you compute bdates! Please set computeBDate to FALSE.")
  }

  ## ---Compute bdate----
  if (computeBDate) {
    layers <- deriveBDate.snowprofileLayers(layers, adjust_bdates = adjust_bdates,
                                            checkMonotonicity = checkMonotonicity)
  }

  ## ---Assign datetag----
  ## Deprecated approach: merge ddate and bdate into datetag:
  # layers[, "datetag"] <- as.Date(as.character(layers[, "bdate"]))  # double conversion to prevent time zone issues!
  # layers[layers[, "gtype"] %in% c("MFcr", "IF"), "datetag"] <- as.Date(as.character(layers[layers[, "gtype"] %in% c("MFcr", "IF"), "ddate"]))

  if (nL > 0) {
    if (all(c("ddate", "bdate") %in% names(layers))) {

      ## in a tmp variable, replace NA bdates with vdate (i.e., surface layers that didn't get buried yet)
      layers_bdate <- as.Date(layers$bdate)
      layers_bdate[is.na(layers_bdate)] <- rep(as.Date(vdate), sum(is.na(layers_bdate)))

      ## choose the first datetag that is older than the layer's ddate
      layers$datetag <- as.Date(sapply(seq(nL), function(i) {
        as.character(potentialDatetags)[potentialDatetags >= as.Date(layers$ddate[i])][1]
      }))

      ## check potentially unsuited datetags raise warnings
      tmpflagA <- which(layers$datetag - layers_bdate > 21)
      tmpflagB <- which(layers$datetag - layers_bdate < -3)
      if (any(c(tmpflagA, tmpflagB))) {
        warning(paste0(sum(tmpflagA, tmpflagB), " out of ", nL, " layers are potentially assigned to inappropriate datetags.\n",
                       ifelse(any(tmpflagA), paste0(sum(tmpflagA), " layers formed more than 3 weeks before their datetags. Check whether there was indeed such a long dry period!",
                                                    "\nrelevant datetag(s): ",
                                                    paste0(unique(layers$datetag[tmpflagA]), collapse = ", "),
                                                    "\nrelevant layer indices: ", paste0(tmpflagA, collapse = ", ")), ""),
                       ifelse(any(tmpflagB), paste0(sum(tmpflagB), " layers did not get buried until more than three days *after* their datetag. Their datetags might not be good choices.",
                                                    "\nrelevant datetag(s): ",
                                                    paste0(unique(layers$datetag[tmpflagB]), collapse = ", "),
                                                    "\nrelevant layer indices: ", paste0(tmpflagB, collapse = ", ")), "")
        )
        )
      }  # END check

    } else {
      warning("No ddate or bdate info available, returning NA datetags")
      layers$datetag <- as.Date(NA)
    }

  }

  return(layers)
}
