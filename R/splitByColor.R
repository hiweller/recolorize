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
  centers <- matrix(recolorize_obj$centers[layer_idx, ], ncol = 3)

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
    rgb_img <- array_to_cimg(recoloredImage(recolorize_obj))

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
      # get dimensions:
      d <- dim(color_masks[[i]]); d1 <- round(d[1] * 0.08); d2 <- d[2]

      # make a little color block for the top:
      m <- lapply(as.list(centers[i, ]),
                     function(j) matrix(j, d1, d2))
      col_block <- abind::abind(m[[1]], m[[2]], m[[3]], along = 3)

      # make the binary mask a color image so we can add the block:
      obj <- abind::abind(color_masks[[i]],
                          color_masks[[i]],
                          color_masks[[i]], along = 3)

      # and paste them together:
      obj2 <- abind::abind(col_block, obj, along = 1)
      plotImageArray(obj2)
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
