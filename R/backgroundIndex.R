# fxn 2: flat array + condition in -> original, index + non-bg out
# 3a. bg mask: get index of bg pixels
# 3b. make non-bg-only pixel matrix for operations
# 3c. convert to color space of choice

#' Index and remove background pixels for color clustering
#'
#' Largely internal function for identifying, indexing, and removing background
#' pixels from an image.
#'
#' @param img An image array, preferably the output of \code{\link[png]{readPNG}},
#'  \code{\link[jpeg]{readJPEG}}, or \code{link[recolorize]{readImage}}.
#' @param bg.condition Background condition, output of
#'   \code{\link{backgroundCondition}}.
#'
#' @return A list with the following elements:
#' \enumerate{
#'     \item `flattened.img`: The original image, flattened into a 2D matrix
#'     (rows = pixels, columns = channels).
#'     \item `img.dims`: Dimensions of the original image.
#'     \item `non.bg`: Pixels from `flattened.img` that fall outside the
#'     background masking conditions. Used for further color clustering and
#'     analysis.
#'     \item `idx`: 2D (row-column) indices for background pixels.
#'     \item `idx.flat`: Same as `idx`, but flattened to vector order.
#' }
#'
#' @details
#' This function flattens a 3-channel image into a 2D matrix before indexing and
#' removing background pixels to take advantage of faster indexing procedures.
#' The `idx`, `idx.flat`, and `img.dims` elements are used to reconstruct the
#' original and recolored images by other functions, namely
#' \code{\link[recolorize]{recolorImage}}.
#'
#' @examples
#' # get image path and read in image
#' img.path <- system.file("extdata/chongi.png", package = "recolorize")
#' img <- png::readPNG(img.path)
#' recolorize::plotImageArray(img)
#'
#' # generate a white background condition
#' bg.condition <- backgroundCondition(lower = rep(0.9, 3),
#'                                     upper = rep(1, 3))
#'
#' # index background pixels
#' bg.indexed <- backgroundIndex(img, bg.condition)
#'
#' # we can reconstruct the original image from the flattened array
#' img2 <- bg.indexed$flattened.img
#' dim(img2) <- bg.indexed$img.dims
#'
#' # notice the original background color (light gray) now shows
#' recolorize::plotImageArray(img2)
#'
#' @export
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

  # remove alpha channel from flattened image (no longer required)
  flattened.img <- flattened.img[ , 1:3]
  img.dims[3] <- 3

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
                   non.bg = non.bg[ , 1:3],
                   idx = idx,
                   idx.flat = idx.flat)
  class(bg.index) <- "bg.index"

  return(bg.index)
}
