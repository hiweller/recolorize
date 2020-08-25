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
#' # make an initial histogram fit
#' ocellata <- system.file("extdata/ocellata.png", package = "recolorize")
#' ocellata_2bin <- recolorize(ocellata, "hist", bins = 2)
#' dist_2bin <- imDist(ocellata_2bin$original.img,
#'                     ocellata_2bin$recolored.img)
#'
#' # we can compare it to finer binning
#' ocellata_3bin <- recolorize(ocellata, "hist", bins = 3)
#' dist_3bin <- imDist(ocellata_3bin$original.img,
#'                     ocellata_3bin$recolored.img, plotting = FALSE)
#'
#' # to make the scales even, we can combine the color distance matrices first
#' combo <- cbind(dist_2bin, dist_3bin)
#'
#' # 2 bin on the left, 3 bin on the right:
#' imHeatmap(combo)
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
#' # just bad
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
                  main = main)

  # reset
  graphics::par(op)
}
