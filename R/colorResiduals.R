#' Calculate squared residuals for color centers
#'
#' Calculates the squared distance between each pixel and its assigned color
#' center. Mostly for internal use by \code{\link{classifyColorManual}}.
#'
#' @param pixel.matrix 2D matrix of pixels to classify (rows = pixels, columns =
#'   channels).
#' @param pixel.assignments A vector of color center assignments for each
#'   pixel. Must match the order of `pixel.matrix`.
#' @param color.centers A matrix of color centers, with rows as centers and
#'   columns as color channels. Rows are assumed to match the index values of
#'   `pixel.assignments`, e.g. a pixel assigned `1` in the assignment vector
#'   is assigned to the color in the first row of `color.centers`.
#' @param color.space Color space in which to calculate distances. One of
#'   "sRGB", "Lab", "Luv", or "XYZ". Passed to
#'   \code{\link[grDevices]{convertColor}}.
#' @param metric Distance metric to be used for calculating pairwise pixel
#'   distances in the given color space; passed to \code{\link[stats]{dist}}.
#' @param ref.white Passed to \code{\link[grDevices]{convertColor}} if
#'   `color.space = "Lab`. Reference white for CIE Lab space.
#'
#' @return
#' A list with the following attributes:
#' \enumerate{
#'     \item `sq_residuals`: The squared residual for every pixel in pixel.matrix.
#'     \item `tot_residuals`: The sum of all squared residuals.
#'     \item `avg_residual`: The average squared residual.
#'     \item `residuals_by_center`: A list of squared residuals for every color center.
#'     \item `avg_by_center`: The average squared residual for every color center.
#' }
#'
#' @examples
#' # RGB extremes (white, black, red, green, blue, yellow, magenta, cyan)
#' ctrs <- matrix(c(1, 1, 1,
#'                  0, 0, 0,
#'                  1, 0, 0,
#'                  0, 1, 0,
#'                  0, 0, 1,
#'                  1, 1, 0,
#'                  1, 0, 1,
#'                  0, 1, 1), byrow = TRUE, ncol = 3)
#'
#' # plot it
#' recolorize::plotColorPalette(ctrs)
#'
#' # create a pixel matrix of random colors
#' pixel.matrix <- matrix(runif(3000), ncol = 3)
#'
#' # assign pixels
#' # see `assignPixels` function for details
#' reassigned <- assignPixels(ctrs, pixel.matrix, adjust.centers = TRUE)
#'
#' # find residuals from original color centers
#' color_residuals <- colorResiduals(pixel.matrix = pixel.matrix,
#'                                   pixel.assignments = reassigned$pixel.assignments,
#'                                   color.centers = ctrs)
#'
#' # compare to residuals from adjusted color centers
#' color_residuals_adjust <- colorResiduals(pixel.matrix = pixel.matrix,
#'                                   pixel.assignments = reassigned$pixel.assignments,
#'                                   color.centers = reassigned$centers)
#'
#' layout(matrix(1:2, nrow = 2))
#' hist(color_residuals$sq_residuals,
#'  breaks = 30, border = NA, col = "tomato",
#'  xlim = c(0, 1), xlab = "Squared residual",
#'  main = "Original centers")
#'
#' hist(color_residuals_adjust$sq_residuals,
#' breaks = 30, border = NA, col = "cornflowerblue",
#' xlim = c(0, 1), xlab = "Squared residual",
#' main = "Adjusted centers")
#'
#' @export
colorResiduals <- function(pixel.matrix,
                           pixel.assignments,
                           color.centers,
                           color.space = "Lab",
                           metric = "euclidean",
                           ref.white = "D65") {

  # make sure all the pixels are assigned to a center that exists
  if (any(!unique(pixel.assignments) %in% 1:nrow(color.centers))) {
    stop("Not all pixel assignments correspond to a provided color center.")
  }

  if (color.space != "sRGB") {
    pixel.matrix <- grDevices::convertColor(pixel.matrix,
                                            from = "sRGB",
                                            to = color.space,
                                            to.ref.white = ref.white)
    color.centers <- grDevices::convertColor(color.centers,
                                             from = "sRGB",
                                             to = color.space,
                                             to.ref.white = ref.white)
  }

  # calculate all squared residuals
  sq_residuals <- sapply(1:length(pixel.assignments),
                         function(i) dist(rbind(pixel.matrix[i, ],
                                                color.centers[pixel.assignments[i], ]),
                                          method = metric))

  # make a list of residuals by color center
  residuals_by_center <- vector("list", length = nrow(color.centers))

  # sort it
  # there's a less dumb way to do this but I don't...care
  for (i in 1:nrow(color.centers)) {
    residuals_by_center[[i]] <- sq_residuals[which(pixel.assignments == i)]
  }

  # returnables
  return(list(sq_residuals = sq_residuals,
              tot_residuals = sum(sq_residuals),
              avg_residual = mean(sq_residuals),
              residuals_by_center = residuals_by_center,
              avg_by_center = lapply(residuals_by_center, mean)))

}
