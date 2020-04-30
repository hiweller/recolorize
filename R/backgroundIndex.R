# fxn 2: flat array + condition in -> original, index + non-bg out
# 3a. bg mask: get index of bg pixels
# 3b. make non-bg-only pixel matrix for operations
# 3c. convert to color space of choice

backgroundIndex <- function(img, bg.condition) {

  # flatten it first -- faster indexing!
  img.dims <- dim(img)
  flattened.img <- img
  dim(flattened.img) <- c(img.dims[1] * img.dims[2],
                          img.dims[3])

  # mask according to background condition
  if (class(bg.condition) == "bg.rect") {

    lower <- bg.condition$lower
    upper <- bg.condition$upper
    idx <- which((lower[1] <= img[ , , 1] &
                    img[ , , 1] <= upper[1]) &
                   (lower[2] <= img[ , , 2] &
                      img[ , , 2] <= upper[2]) &
                   (lower[3] <= img[ , , 3] &
                      img[ , , 3] <= upper[3]))

    # transparency
  } else if (class(bg.condition) == "bg.t") {

    # if there's no transparency, oops
    if (ncol(flattened.img) != 4) {

      warning("Image has no transparency channel; clustering all pixels")
      idx <- character(0)

    } else {

      # otherwise use it
      idx <- which(flattened.img[ , 4] < 1)

    }

  } else if (class(bg.condition) == "bg.sphere") {

    stop("Center/radius masking coming soon...")

  } else if (class(bg.condition) == "bg.none") {

    idx <- character(0)

  } else {
    stop("bg.condition must be output from backgroundCondition()")
  }

  # make returnables
  if (length(idx) == 0) {
    non.bg <- flattened.img
    idx.flat <- idx
    message("No pixels satisfying masking conditions; clustering all pixels")
  } else {
    non.bg <- flattened.img[-idx, ]
    idx.flat <- idx
    idx <- arrayInd(idx.flat, .dim = dim(flattened.img))
  }


  # set S3 class - arbitrary but useful for checking
  bg.index <- list(flattened.img = flattened.img,
                   img.dims = img.dims,
                   non.bg = non.bg,
                   idx = idx,
                   idx.flat = idx.flat)
  class(bg.index) <- "bg.index"

  return(bg.index)
}
