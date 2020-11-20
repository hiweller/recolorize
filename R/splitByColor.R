#' Split color clusters in a recolorize object into layers
#'
#' Separates color clusters from a \code{\link{recolorize}},
#' \code{\link{recluster}}, or \code{\link{imposeColors}} object
#' into binary masks.
#'
#' @param recolorize_obj A recolorize object from \code{\link{recolorize}},
#'   \code{\link{recluster}}, or \code{\link{imposeColors}}.
#' @param layers Either `"all"` or a numeric vector of which color centers to
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
                         layers = "all",
                         plot_method = "overlay") {

  # check layers argument
  if (layers == "all") {
    layer_idx <- 1:nrow(recolorize_obj$centers)
  } else if (is.numeric(layers)) {
    # use all colors
    layer_idx <- layers
  } else {
    stop("'layers' must be 'all' or a numeric vector of layer indices
         matching the order of the color centers")
  }

  # get color centers
  centers <- recolorize_obj$centers[layer_idx, ]

  # make an empty list for the layer bitmaps
  color_masks <- vector("list", length = length(layer_idx))

  # convert pixel assignment to cimg object
  img <- imager::as.cimg(recolorize_obj$pixel_assignments)

  for (i in 1:length(layer_idx)) {

    # get index
    px <- imager::imsub(img) == layer_idx[i]

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
    alpha_px <- imager::imsub(img) == 0
    rgb_img <- array_to_cimg(recolorize_obj$recolored_img)

    # make grayscale image
    grimg <- imager::grayscale(imager::rm.alpha(rgb_img), drop = FALSE)
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
      plotImageArray(color_masks[[i]],
                     #xlim = c(-0.05, 1.05),
                     #ylim = c(-0.05, 1.05),
                     main = i)
      graphics::rect(0, 0, 1, 1,
                     border = grDevices::rgb(centers[i, 1],
                                             centers[i, 2],
                                             centers[i, 3]),
                     lwd = 8,
                     ljoin = 3)
      # rect(-0.1, 0, 0, 1.,
      #      col = rgb(centers[i, 1],
      #                   centers[i, 2],
      #                   centers[i, 3]),
      #      border = NA, lwd = 5)
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
