#' Split color clusters in a recolorize object into layers
#'
#' Separates color clusters from a \code{\link{recolorize}},
#' \code{\link{recluster}}, or \code{\link{imposeColors}} object
#' into binary masks.
#'
#' @param recolorize_obj A recolorize object from \code{\link{recolorize}},
#'   \code{\link{recluster}}, or \code{\link{imposeColors}}.
#' @param colors Either `"all"` or a numeric vector of which color centers to
#'   return.
#' @param plot_method Plotting method for plotting the color layers. Options
#'   are`"overlay"`, `"binary"`, `"colormask"`, or `"none"`.
#'
#' @return A list of binary masks (white = color presence, black = color
#'   absence), one per color center.
#'
#' @examples
#' # get original fit
#' corbetti <- system.file("extdata/corbetti.png", package = "recolorize")
#' recolored_corbetti <- recolorize::recolorize(corbetti, plotting = TRUE)
#'
#' # make a layout
#' layout(matrix(c(1, 1:9), nrow = 2))
#'
#' # plot original
#' plotImageArray(recolored_corbetti$original_img)
#'
#' # plot layers
#' corbetti_layers <- splitByColor(recolored_corbetti, plot_method = "over")
#'
#' # plot one of the binary maps
#' layout(matrix(1:2, nrow = 1))
#' plotImageArray(corbetti_layers[[2]], main = "original layer")
#'
#' # make an imager pixset
#' px <- imager::as.cimg(corbetti_layers[[2]] == 1)
#' px_clean <- imager::clean(px, 3)
#'
#' # convert back to an image for plotting
#' plotImageArray(recolorize:::cimg_to_array(px_clean), main = "cleaned layer")
#'
#' @export
splitByColor <- function(recolorize_obj,
                         colors = "all",
                         plot_method = "overlay") {

  # if only plotting some layers, then extract those centers
  if (is.numeric(colors)) {

    centers <- recolorize_obj$centers[colors, ]

  } else {

    # use all colors
    centers <- recolorize_obj$centers

  }

  # convert img to cimg object
  img <- recolorize_obj$recolored_img
  dim(img) <- c(dim(img)[1:2], 1, 4)
  img <- imager::as.cimg(img)

  # make an empty list for the layer bitmaps
  color_masks <- vector("list", length = nrow(centers))

  # for each color center...
  for (i in 1:nrow(centers)) {

    # extract color
    cc <- recolorize_obj$centers[i, ]
    color <- grDevices::rgb(cc[1], cc[2], cc[3])

    # get r, g, b channel matches
    rpx <- imager::imsub(img, cc == 1) == cc[1]
    gpx <- imager::imsub(img, cc == 2) == cc[2]
    bpx <- imager::imsub(img, cc == 3) == cc[3]
    opx <- imager::imsub(img, cc == 4) > 0 # opacity

    # get pixset for color indices
    px <- imager::parall(list(rpx, gpx, bpx, opx))

    # get color layer as a bitmap
    layer <- as.numeric(px)
    dim(layer) <- dim(img)[1:2]

    # store bitmap
    color_masks[[i]] <- layer
  }

  plot_method <- match.arg(plot_method,
                           choices = c("overlay",
                                       "binary",
                                       "colormask",
                                       "none"))

  # plot as colored overlay of grayscale image
  if (plot_method == "overlay") {

    # get transparent pixset
    alpha_px <- imager::imsub(img, cc == 4) == 0

    # make grayscale image
    grimg <- imager::grayscale(imager::rm.alpha(img), drop = FALSE)
    grimg[alpha_px] <- 1

    # plot em!
    for (i in 1:length(color_masks)) {

      # get color
      color <- grDevices::rgb(centers[i, 1],
                              centers[i, 2],
                              centers[i, 3])

      # get pixset
      px <- imager::as.cimg(color_masks[[i]]) > 0

      # colorize
      index_image <- as.array(imager::colorise(grimg, px, col = color),
                              dim = dim(img))

      # reshape and plot
      dim(index_image) <- dim(index_image)[c(1:2, 4)]
      plotImageArray(index_image, main = i)

    }

  }

  # plot as a binary mask
  if (plot_method == "binary") {

    # plot as binary map
    for (i in 1:length(color_masks)) {
      plotImageArray(color_masks[[i]], main = i)
    }

  }

  # plot as a binary mask with colored pixels
  if (plot_method == "colormask") {

    for (i in 1:length(color_masks)) {
      # get color
      color <- grDevices::rgb(centers[i, 1],
                   centers[i, 2],
                   centers[i, 3])

      # get pixset
      px <- imager::as.cimg(color_masks[[i]]) > 0

      # color it in
      index_image <- imager::colorise(imager::as.cimg(color_masks[[i]]),
                                      px, col = color)
      index_image <- as.array(index_image, dim = dim(img))
      dim(index_image) <- dim(index_image)[c(1:2, 4)]

      # and plot it
      plotImageArray(index_image, main = i)

    }
  }

  # return binary masks
  return(color_masks)
}
