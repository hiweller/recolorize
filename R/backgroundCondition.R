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
#' @param alpha_channel Logical. Is there an alpha channel?
#' @param quietly Logical. Print a message about background masking parameters?
#'
#' @return
#' A list with background masking parameters. Can be one of 4 classes:
#' \enumerate{
#'     \item `bg_rect`: If `lower` and `upper` are specified.
#'     \item `bg_sphere`: If `center` and `radius` are specified.
#'     \item `bg_t`: If `transparent` is `TRUE` and there is an alpha channel
#'     with transparent pixels.
#'     \item `bg_none`: If no background masking is specified (or transparency
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
#' backgroundCondition(transparent = TRUE, alpha_channel = TRUE, quietly = FALSE)
#'
#' # oops, no alpha channel:
#' backgroundCondition(transparent = TRUE, alpha_channel = FALSE, quietly = FALSE)
#'
#' # oops, no alpha channel, but with white background as a fallback:
#' backgroundCondition(lower = rep(0.9, 3), upper = rep(1, 3),
#'                     transparent = TRUE, alpha_channel = FALSE,
#'                     quietly = FALSE)
#'
#' @export
backgroundCondition <- function(lower = NULL, upper = NULL,
                                center = NULL, radius = NULL,
                                transparent = NULL,
                                alpha_channel = FALSE,
                                quietly = TRUE) {

  # shut up...this makes sense
  if (!isTRUE(alpha_channel)) {
    alpha_channel <- NULL
  }

  # put potential background conditions into a list
  args_list <- list(lower = lower, upper = upper,
                    center = center, radius = radius,
                    transparent = transparent,
                    alpha_channel = alpha_channel)

  # make a vector of which values are NOT NULL
  null_count <- which(!unlist(lapply(args_list, is.null)))
  null_count <- paste(null_count, collapse = "")

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
  category_idx <- match(null_count, combos$vec)

  # if they don't, mask nothing
  if (is.na(category_idx)) {
    warning("Could not parse background parameters; no pixels will be masked")
    category <- "none"
  } else {
    # otherwise, we have our bg category!
    category <- combos$category[category_idx]
  }

  # for each category: define background condition
  # classify it
  # provide message if quietly = FALSE
  if (category == "transparent") {

    bg_condition <- "transparent"
    class(bg_condition) <- "bg_t"
    msg <- "Using transparency to mask pixels"

  } else if (category == "rect") {

    bg_condition <- list(lower = lower, upper = upper)
    class(bg_condition) <- "bg_rect"
    msg <- paste("Masking pixels in range:\n",
                   paste("R: ", paste(lower[1], "-", upper[1], sep = ""),
                         "; G: ", paste(lower[2], "-", upper[2], sep = ""),
                         "; B: ", paste(lower[3], "-", upper[3], sep = ""),
                         sep = ""))

  } else if (category == "sphere") {

    bg_condition <- list(center = center, radius = radius)
    class(bg_condition) <- "bg_sphere"
    msg <- paste("Masking pixels in range:\n",
                 paste("Center: ", paste(center, collapse = ", "),
                       " +/- ", radius * 100, "%", sep = ""))

  } else if (category == "none") {

    bg_condition <- NA
    class(bg_condition) <- "bg_none"
    msg <- "Using all pixels"

  } else {

    # this shouldn't be possible so
    stop("uh oh!!")

  }

  # print message
  if (!quietly) { message(msg) }

  # return background
  return(bg_condition)

}

