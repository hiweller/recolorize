# reads in an image as a 3D array
readImage <- function(img.path, resize = NULL, rotate = NULL) {

  # read in image
  img.ext <- tolower(tools::file_ext(img.path))
  if (img.ext %in% c("jpeg", "jpg", "png", "bmp")) {
    img <- imager::load.image(img.path)
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
