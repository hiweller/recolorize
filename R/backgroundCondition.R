# bg.condition can be one of EITHER:
# list with attributes "lower" and "upper"
# list with attributes "center" and "radius"
# "transparent"
# anything else will be parsed as no background masking and all pixels will be
# returned as non-background
# ex: backgroundCondition(lower = rep(0, 3), upper = rep(0.2, 3))
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
                 paste("Center: ", paste(center[i, ], collapse = ", "),
                       " +/- ", radius[i] * 100, "%", sep = ""))

  } else if (category == "none") {

    bg.condition <- NA
    class(bg.condition) <- "bg.none"
    msg <- "Using all pixels"

  } else {

    # this shouldn't be possible so
    stop("you lost me, bud")

  }

  # print message
  if (!quietly) { message(msg) }

  # return background
  return(bg.condition)

}

