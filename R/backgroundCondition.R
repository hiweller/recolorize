#' Generate a background condition for masking
#'
#' Internal function for parsing potential background conditions. Prioritizes
#' transparency masking if conflicting options are provided. See details.
#'
#' @param lower,upper RGB triplet ranges for setting a bounding box of pixels to mask.
#' @param center,radius RGB triplet and radius (as a proportion) for masking
#'   pixels within a spherical range.
#' @param transparent Logical or `NULL`. Use transparency to mask? Requires an
#'   alpha channel.
#' @param alpha.channel Logical. Is there an alpha channel?
#' @param quietly Logical. Print a message about background masking parameters?
#'
#' @return
#' A list with background masking parameters. Can be one of 4 classes:
#' \enumerate{
#'     \item `bg.rect`: If `lower` and `upper` are specified.
#'     \item `bg.sphere`: If `center` and `radius` are specified.
#'     \item `bg.t`: If `transparent` is `TRUE` and there is an alpha channel
#'     with transparent pixels.
#'     \item `bg.none`: If no background masking is specified (or transparency
#'     was specified but there are no transparent pixels).
#' }
#'
#' @details
#' Prioritizes transparency. If `transparency = TRUE` but other options (such as
#' `lower` and `upper`) are specified, then only transparent pixels will be masked.
#' If `transparency = TRUE` but there is no alpha channel (as in a JPEG image),
#' this flag is ignored and other options (`lower` and `upper` or `center` and `radius`)
#' are used instead.
#'
#' This is an internal convenience function sourced by \code{\link{backgroundIndex}}.
#'
#' @examples
#'
#' # masking a white background:
#' backgroundCondition(lower = rep(0.9, 3), upper = rep(1, 3), quietly = FALSE)
#'
#' # masking transparent pixels:
#' backgroundCondition(transparent = TRUE, alpha.channel = TRUE, quietly = FALSE)
#'
#' # oops, no alpha channel:
#' backgroundCondition(transparent = TRUE, alpha.channel = FALSE, quietly = FALSE)
#'
#' # oops, no alpha channel, but with white background as a fallback:
#' backgroundCondition(lower = rep(0.9, 3), upper = rep(1, 3),
#'                     transparent = TRUE, alpha.channel = FALSE,
#'                     quietly = FALSE)
#'
#' @export
backgroundCondition <- function(lower = NULL, upper = NULL,
                                center = NULL, radius = NULL,
                                transparent = NULL,
                                alpha.channel = FALSE,
                                quietly = TRUE) {

  # shut up...this makes sense
  if (!isTRUE(alpha.channel)) {
    alpha.channel <- NULL
  }

  # put potential background conditions into a list
  args.list <- list(lower = lower, upper = upper,
                    center = center, radius = radius,
                    transparent = transparent,
                    alpha.channel = alpha.channel)

  # make a vector of which values are NOT NULL
  null.count <- which(!unlist(lapply(args.list, is.null)))
  null.count <- paste(null.count, collapse = "")

  # these combinations are allowed:
  combos <- data.frame(vec = c("", # no background
                               "12", "34", # rectangle | sphere
                               "5", "125", "345", # oops: no transparency channel
                               "56", "1256", "3456"), # transparency reigns
                       category = c("none",
                                    "rect", "sphere",
                                    "none", "rect", "sphere",
                                    rep("transparent", 3)))


  # do the supplied arguments fit one of these combinations?
  category.idx <- match(null.count, combos$vec)

  # if they don't, mask nothing
  if (is.na(category.idx)) {
    warning("Could not parse background parameters; no pixels will be masked")
    category <- "none"
  } else {
    # otherwise, we have our bg category!
    category <- combos$category[category.idx]
  }

  # for each category: define background condition
  # classify it
  # provide message if quietly = FALSE
  if (category == "transparent") {

    bg.condition <- "transparent"
    class(bg.condition) <- "bg.t"
    msg <- "Using transparency to mask pixels"

  } else if (category == "rect") {

    bg.condition <- list(lower = lower, upper = upper)
    class(bg.condition) <- "bg.rect"
    msg <- paste("Masking pixels in range:\n",
                   paste("R: ", paste(lower[1], "-", upper[1], sep = ""),
                         "; G: ", paste(lower[2], "-", upper[2], sep = ""),
                         "; B: ", paste(lower[3], "-", upper[3], sep = ""),
                         sep = ""))

  } else if (category == "sphere") {

    bg.condition <- list(center = center, radius = radius)
    class(bg.condition) <- "bg.sphere"
    msg <- paste("Masking pixels in range:\n",
                 paste("Center: ", paste(center, collapse = ", "),
                       " +/- ", radius * 100, "%", sep = ""))

  } else if (category == "none") {

    bg.condition <- NA
    class(bg.condition) <- "bg.none"
    msg <- "Using all pixels"

  } else {

    # this shouldn't be possible so
    stop("uh oh!!")

  }

  # print message
  if (!quietly) { message(msg) }

  # return background
  return(bg.condition)

}

