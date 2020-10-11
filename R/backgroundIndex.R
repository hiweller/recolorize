#' Index and remove background pixels for color clustering
#'
#' Largely internal function for identifying, indexing, and removing background
#' pixels from an image.
#'
#' @param img An image array, preferably the output of \code{\link[png]{readPNG}},
#'  \code{\link[jpeg]{readJPEG}}, or \code{link[recolorize]{readImage}}.
#' @param bg_condition Background condition, output of
#'   \code{\link{backgroundCondition}}.
#'
#' @return A list with the following elements:
#' \enumerate{
#'     \item `flattened_img`: The original image, flattened into a 2D matrix
#'     (rows = pixels, columns = channels).
#'     \item `img_dims`: Dimensions of the original image.
#'     \item `non_bg`: Pixels from `flattened_img` that fall outside the
#'     background masking conditions. Used for further color clustering and
#'     analysis.
#'     \item `idx`: 2D (row-column) indices for background pixels.
#'     \item `idx_flat`: Same as `idx`, but flattened to vector order.
#' }
#'
#' @details
#' This function flattens a 3-channel image into a 2D matrix before indexing and
#' removing background pixels to take advantage of faster indexing procedures.
#' The `idx`, `idx_flat`, and `img_dims` elements are used to reconstruct the
#' original and recolored images by other functions, namely
#' \code{\link[recolorize]{recolorImage}}.
#'
#' @examples
#' # get image path and read in image
#' img_path <- system.file("extdata/chongi.png", package = "recolorize")
#' img <- png::readPNG(img_path)
#' recolorize::plotImageArray(img)
#'
#' # generate a white background condition
#' bg_condition <- backgroundCondition(lower = rep(0.9, 3),
#'                                     upper = rep(1, 3))
#'
#' # index background pixels
#' bg_indexed <- backgroundIndex(img, bg_condition)
#'
#' # we can reconstruct the original image from the flattened array
#' img2 <- bg_indexed$flattened_img
#' dim(img2) <- bg_indexed$img_dims
#'
#' # notice the original background color (light gray) now shows
#' recolorize::plotImageArray(img2)
#'
#' @export
backgroundIndex <- function(img, bg_condition) {

  # flatten it first -- faster indexing!
  img_dims <- dim(img)
  flattened_img <- img
  dim(flattened_img) <- c(img_dims[1] * img_dims[2],
                          img_dims[3])

  # mask according to background condition
  if (class(bg_condition) == "bg_rect") {

    lower <- bg_condition$lower
    upper <- bg_condition$upper
    idx <- which((lower[1] <= img[ , , 1] &
                    img[ , , 1] <= upper[1]) &
                   (lower[2] <= img[ , , 2] &
                      img[ , , 2] <= upper[2]) &
                   (lower[3] <= img[ , , 3] &
                      img[ , , 3] <= upper[3]))

    # transparency
  } else if (class(bg_condition) == "bg_t") {

    # if there's no transparency, oops
    if (ncol(flattened_img) != 4) {

      warning("Image has no transparency channel; clustering all pixels")
      idx <- character(0)

    } else {

      # otherwise use it
      idx <- which(round(flattened_img[ , 4]) < 1)

    }

  } else if (class(bg_condition) == "bg_sphere") {

    stop("Center/radius masking coming soon...")

  } else if (class(bg_condition) == "bg_none") {

    idx <- character(0)

  } else {
    stop("bg_condition must be output from backgroundCondition()")
  }

  # remove alpha channel from flattened image (no longer required)
  flattened_img <- flattened_img[ , 1:3]
  img_dims[3] <- 3

  # make returnables
  if (length(idx) == 0) {
    non_bg <- flattened_img
    idx_flat <- idx
    message("No pixels satisfying masking conditions; clustering all pixels")
  } else {
    non_bg <- flattened_img[-idx, ]
    idx_flat <- idx
    idx <- arrayInd(idx_flat, .dim = dim(flattened_img))
  }


  # set S3 class - arbitrary but useful for checking
  bg_index <- list(flattened_img = flattened_img,
                   img_dims = img_dims,
                   non_bg = non_bg[ , 1:3],
                   idx = idx,
                   idx_flat = idx_flat)
  class(bg_index) <- "bg_index"

  return(bg_index)
}
