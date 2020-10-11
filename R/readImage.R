#' Read in an image as a 3D array
#'
#' Reads in and processes an image as a 3D array. Extremely simple wrapper for
#' \code{\link[imager]{load.image}}, but it strips the depth channel (resulting
#' in a 3D, not 4D, array). This will probably change.
#'
#' @param img_path Path to the image (a string).
#' @param resize Fraction by which to reduce image size. Important for speed.
#' @param rotate Number of degrees to rotate the image.
#'
#' @return
#' A 3D RGB array (pixel rows x pixel columns x color channels). RGB channels
#' are all scaled 0-1, not 0-255.
#'
#' @examples
#' corbetti <- system.file("extdata/corbetti.png", package = "recolorize")
#' img <- readImage(corbetti)
#' plotImageArray(img)
#'
#' @export
readImage <- function(img_path, resize = NULL, rotate = NULL) {

  # read in image
  img_ext <- tolower(tools::file_ext(img_path))
  if (img_ext %in% c("jpeg", "jpg", "png", "bmp")) {
    img <- imager::load.image(img_path)
  } else {
    stop("Image must be either JPG, PNG, or BMP")
  }

  # resize if specified
  if (!is.null(resize)) {
    img <- imager::imresize(img, scale = resize, interpolation = 6)
  }

  # rotate if specified
  if (!is.null(rotate)) {
    img <- imager::imrotate(img, angle = rotate)
  }

  # undo what cimg does for some reason
  img <- imager::imrotate(img, -90)

  # drop depth channel
  # i don't want to talk about this
  # someday we'll do it all with cimg objects
  temp <- array(dim = dim(img)[c(1:2, 4)])
  temp <- img[ , , 1, ]

  # flip the image
  # this is a bit slow!
  # another reason to switch to all cimg objects!
  # imager is just not friendly to me
  temp[ , , ] <- apply(temp, 3, function(mat) mat[ , ncol(mat):1, drop=FALSE])
  img <- temp
  rm(temp)

  return(img)

}
