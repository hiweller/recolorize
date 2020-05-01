
recolorize <- function(img.path, method = "kmeans",
                       bins = 3, n = 10,
                       lower = NULL, upper = NULL,
                       transparent = TRUE,
                       resize = NULL, rotate = NULL,
                       plotting = TRUE, horiz = TRUE) {

  # get method
  method <- match.arg(tolower(method), c("kmeans", "histogram"))

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
  img <- temp
  rm(temp)

  # make background condition
  alpha.channel <- dim(img)[3] == 4 # is there a transparency channel?
  bg.condition <- backgroundCondition(lower = lower, upper = upper,
                                      center = NULL, radius = NULL,
                                      transparent = transparent,
                                      alpha.channel = alpha.channel)

  # index background
  bg.indexed <- backgroundIndex(img, bg.condition)

  # color clusters & assign pixels
  color.clusters <- colorClusters(bg.indexed$non.bg, method = method,
                                  n = n, bins = bins)

  # recolor based on assignments/centers
  recolored <- recolorImage(bg.indexed, color.clusters, plotting = T)

  # plot result
  if (plotting) {
    plotRecolorized(recolored$recolored.img,
                    recolored$centers, horiz = horiz)
  }

  # returnables:
  original.img <- img
  recolored.img <- recolored$recolored.img
  method <- if( method == "kmeans" ) {
    list(method = "kmeans", n = n)
  } else {
    list(method = "histogram", bins = bins)
  }
  color.space <- "RGB"
  centers <- color.clusters$centers
  sizes <- table(color.clusters$pixel.assignments)
  pixel.assignments <- color.clusters$pixel.assignments

  # return em
  return.list <- list(original.img = original.img,
                      recolored.img = recolored.img,
                      method = method,
                      color.space = color.space,
                      centers = centers,
                      sizes = sizes,
                      pixel.assignments = pixel.assignments)

}
