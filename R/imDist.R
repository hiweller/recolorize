#' Calculates the distance between non-transparent pixels in images
#'
#' Compares two versions of the same image (probably original and recolored)
#' by calculating the color distance between the colors of each pair of pixels.
#'
#' @param im1,im2 Images to compare; must have the same dimensions. Distances
#'   will be calculated between each pair of non-transparent pixels.
#' @param color.space Color space in which to calculate distances. One of
#'   "sRGB", "Lab", "Luv", or "XYZ". Passed to
#'   \code{\link[grDevices]{convertColor}}.
#' @param ref.white Passed to \code{\link[grDevices]{convertColor}} if
#'   `color.space = "Lab`. Reference white for CIE Lab space.
#' @param metric Distance metric to be used for calculating pairwise pixel
#'   distances in the given color space; passed to \code{\link[stats]{dist}}.
#' @param plotting Logical. Plot heatmap of color distances?
#' @param palette If plotting, the color palette to be used. Default is blue to
#'   red (`colorRamps::matlab.like(100)`).
#'
#' @return A matrix of the same dimensions as the original images,
#' with the distance between non-transparent pixels at each pixel coordinate.
#' Transparent pixels are returned as `NA`.
#'
#' @examples
#' fulgidissima <- system.file("extdata/fulgidissima.png", package = "recolorize")# make an initial histogram fit
#'
#'  # make an initial histogram fit
#' # this doesn't look great:
#' fulgidissima_2bin <- recolorize(fulgidissima, "hist", bins = 2)
#'
#' # and we can see that everywhere but the head has pretty high residuals:
#' dist_2bin <- imDist(fulgidissima_2bin$original.img,
#'                     fulgidissima_2bin$recolored.img)
#'
#' # using 3 bins/channel looks much better:
#' fulgidissima_3bin <- recolorize(fulgidissima, "hist", bins = 3)
#'
#' # and we can see that on the heatmap:
#' dist_3bin <- imDist(fulgidissima_3bin$original.img,
#'                     fulgidissima_3bin$recolored.img)
#'
#' # default behavior is to se the color range to the range of distances
#' # in a single matrix; to compare two different fits, we have to provide
#' # the same `zlim` scale for both
#' zlim <- range(c(dist_2bin, dist_3bin), na.rm = TRUE)
#'
#' # now we can plot them to compare the fits:
#' layout(matrix(1:2, nrow = 1))
#' imHeatmap(dist_2bin, zlim = zlim)
#' imHeatmap(dist_3bin, zlim = zlim)
#'
#' # we can also use other color spaces:
#' rgb_3bin <- imDist(fulgidissima_3bin$original.img,
#'                    fulgidissima_3bin$recolored.img,
#'                    color.space = "sRGB")
#'
#' # looks oddly worse, but to keep things in perspective,
#' # you can set the zlim range to the maximum color distance in RGB space:
#' imHeatmap(rgb_3bin, zlim = c(0, sqrt(3)))
#' # not useful for troubleshooting, but broadly reassuring!
#'
#'
#' @export
imDist <- function(im1, im2,
                   color.space = "Lab",
                   ref.white = "D65",
                   metric = "euclidean",
                   plotting = TRUE,
                   palette = "default") {

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
  color.space <- match.arg(color.space,
                           c("sRGB", "XYZ", "Lab", "Luv"))
  im1_px <- grDevices::convertColor(im1[pix_idx, 1:3],
                         "sRGB", to = color.space,
                         to.ref.white = ref.white)
  im2_px <- grDevices::convertColor(im2[pix_idx, 1:3],
                         "sRGB", to = color.space,
                         to.ref.white = ref.white)

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
    imHeatmap(d, palette = palette)
  }

  # return the matrix of distances
  return(d)

}

#' Plot a heatmap of a matrix of color distances
#'
#' Plots the output of \code{\link{imDist}} as a heatmap.
#'
#' @param mat A color distance matrix, preferably output of
#'   \code{\link{imDist}}.
#' @param palette The color palette to be used. Default is blue to
#'   red (`colorRamps::matlab.like(100)`).
#' @param main Plot title.
#' @param ... Parameters passed to \code{\link[graphics]{image}}.
#'
#' @examples
#' chongi <- system.file("extdata/chongi.png", package = "recolorize")
#' chongi_k <- recolorize(chongi, "k", n = 5)
#'
#' d <- imDist(chongi_k$original.img,
#'             chongi_k$recolored.img, plotting = FALSE)
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
                      ...) {

  # reverse
  d2 <- mat[nrow(mat):1, ]

  # transpose
  d2 <- t(d2)

  # get aspect ratio
  asp <- dim(d2)[2] / dim(d2)[1]

  # make colors if not provided
  if (length(palette) == 1) {
    palette <- colorRamps::matlab.like(100)
  }

  # set parameters
  op <- graphics::par(mar = c(0, 0, 2, 0))

  # plot
  graphics::image(d2, ann = F, axes = F, asp = asp,
                  col = palette,  useRaster = T,
                  main = main, ...)

  # reset
  graphics::par(op)
}
