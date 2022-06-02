#' Calculates the distance between non-transparent pixels in images
#'
#' Compares two versions of the same image (probably original and recolored)
#' by calculating the color distance between the colors of each pair of pixels.
#'
#' @param im1,im2 Images to compare; must have the same dimensions. Distances
#'   will be calculated between each pair of non-transparent pixels.
#' @param color_space Color space in which to calculate distances. One of
#'   "sRGB", "Lab", "Luv", or "XYZ". Passed to
#'   [grDevices::convertColor()].
#' @param ref_white Passed to [grDevices::convertColor()] if
#'   `color_space = "Lab`. Reference white for CIE Lab space.
#' @param metric Distance metric to be used for calculating pairwise pixel
#'   distances in the given color space; passed to [stats::dist()].
#' @param plotting Logical. Plot heatmap of color distances?
#' @param palette If plotting, the color palette to be used. Default is blue to
#'   red (`colorRamps::blue2red(100)`).
#' @param main Plot title.
#' @param ... Parameters passed to [graphics::image()].
#'
#' @return A matrix of the same dimensions as the original images,
#' with the distance between non-transparent pixels at each pixel coordinate.
#' Transparent pixels are returned as `NA`.
#'
#' @examples
#' fulgidissima <- system.file("extdata/fulgidissima.png",
#'                              package = "recolorize")
#' fulgidissima <- png::readPNG(fulgidissima)
#' # make an initial histogram fit
#' # this doesn't look great:
#' fulgidissima_2bin <- recolorize(fulgidissima, "hist", bins = 2)
#'
#' # we can compare with the original image by creating the recolored
#' # image from the colormap
#' recolored_2bin <- constructImage(fulgidissima_2bin$pixel_assignments,
#'                                 fulgidissima_2bin$centers)
#' dist_2bin <- imDist(im1 = fulgidissima,
#'                     im2 = recolored_2bin)
#'
#' # using 3 bins/channel looks much better:
#' fulgidissima_3bin <- recolorize(fulgidissima, "hist", bins = 3)
#'
#' # and we can see that on the heatmap:
#' recolored_3bin <- constructImage(fulgidissima_3bin$pixel_assignments,
#'                                 fulgidissima_3bin$centers)
#' dist_3bin <- imDist(im1 = fulgidissima,
#'                     im2 = recolored_3bin)
#'
#' # default behavior is to set the color range to the range of distances
#' # in a single matrix; to compare two different fits, we have to provide
#' # the same `zlim` scale for both
#' r <- range(c(dist_2bin, dist_3bin), na.rm = TRUE)
#'
#' # to reset graphical parameters:
#' current_par <- graphics::par(no.readonly = TRUE)
#'
#' # now we can plot them to compare the fits:
#' layout(matrix(1:2, nrow = 1))
#' imHeatmap(dist_2bin, range = r)
#' imHeatmap(dist_3bin, range = r)
#'
#' # we can also use other color spaces:
#' rgb_3bin <- imDist(fulgidissima,
#'                    recolored_3bin,
#'                    color_space = "sRGB")
#'
#' # looks oddly worse, but to keep things in perspective,
#' # you can set the range to the maximum color distance in RGB space:
#' imHeatmap(rgb_3bin, range = c(0, sqrt(3)))
#' # not useful for troubleshooting, but broadly reassuring!
#'
#' # reset:
#' graphics::par(current_par)
#' @export
imDist <- function(im1, im2,
                   color_space = c("Lab", "sRGB", "XYZ", "Luv"),
                   ref_white = "D65",
                   metric = "euclidean",
                   plotting = TRUE,
                   palette = "default",
                   main = "",
                   ...) {

  color_space <- match.arg(color_space)

  # get dimensions
  dims <- dim(im1)

  # make sure image dimensions are shared
  if (any(dim(im2) != dim(im1))) {
    stop("Images must be the same dimensions.")
  }

  # flat for easier indexing
  dim(im1) <- c(dims[1] * dims[2],
                dims[3])
  dim(im2) <- dim(im1)

  # only measure non-transparent ones
  pix_idx <- which(im1[ , 4] == 1 & im2[ , 4] == 1)

  # convert the non-transparent pixels
  im1_px <- grDevices::convertColor(im1[pix_idx, 1:3],
                         "sRGB", to = color_space,
                         to.ref.white = ref_white)
  im2_px <- grDevices::convertColor(im2[pix_idx, 1:3],
                         "sRGB", to = color_space,
                         to.ref.white = ref_white)

  # make a distance vector of NA
  d <- matrix(NA,
              nrow = dims[1],
              ncol = dims[2])

  # uh...again, could be smarter
  for (i in 1:length(pix_idx)) {
    d[pix_idx[i]] <- stats::dist(rbind(im1_px[i, ],
                                im2_px[i, ]),
                          method = metric)
  }

  # reshape
  dim(d) <- dims[1:2]

  # plot
  if (plotting) {
    imHeatmap(d, palette = palette, main = main, ...)
  }

  # return the matrix of distances
  return(d)

}

#' Plot a heatmap of a matrix of color distances
#'
#' Plots the output of [imDist()] as a heatmap.
#'
#' @param mat A color distance matrix, preferably output of
#'   [imDist()].
#' @param palette The color palette to be used. Default is blue to
#'   red (`colorRamps::blue2red(100)`).
#' @param main Plot title.
#' @param range Range for heatmap values. Defaults to the range of values in the
#'   matrix, but should be set to the same range for all images if comparing
#'   heatmaps.
#' @param legend Logical. Add a continuous color legend?
#' @param ... Parameters passed to [graphics::image()].
#'
#' @return Nothing; plots a heatmap of the color residuals.
#'
#' @examples
#' chongi <- system.file("extdata/chongi.png", package = "recolorize")
#' chongi <- png::readPNG(chongi)
#' chongi_k <- recolorize(chongi, "k", n = 5)
#'
#' recolored_chongi <- constructImage(chongi_k$pixel_assignments,
#'                                    chongi_k$centers)
#' d <- imDist(chongi,
#'             recolored_chongi, plotting = FALSE)
#'
#' # original flavor
#' imHeatmap(d)
#'
#' # bit offputting
#' imHeatmap(d, palette = colorRamps::ygobb(100))
#'
#' # just dreadful
#' imHeatmap(d, palette = colorRamps::primary.colors(100))
#' @export
imHeatmap <- function(mat,
                      palette = "default",
                      main = "",
                      range = NULL,
                      legend = TRUE,
                      ...) {

  # reverse
  d2 <- mat[nrow(mat):1, ]

  # transpose
  d2 <- t(d2)

  # get aspect ratio
  asp <- dim(d2)[2] / dim(d2)[1]

  # make colors if not provided
  if (length(palette) == 1) {
    palette <- colorRamps::blue2red(100)
  }

  # get range if not specified
  if (is.null(range)) {
    range <- range(mat, na.rm = TRUE)
  }

  # plot
  graphics::image(d2, axes = F, asp = asp,
                  col = palette,  zlim = range,
                  ...)
  graphics::title(main, line = 0)

  if(legend) {
    plotfunctions::gradientLegend(valRange = range,
                                  color = palette, dec = 1,
                                  side = 4, pos = 0.5,
                                  inside = TRUE)
  }

}
