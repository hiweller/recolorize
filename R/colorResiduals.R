#' Calculate squared residuals for color centers
#'
#' Calculates the squared distance between each pixel and its assigned color
#' center. Mostly for internal use by \code{\link{classifyColorManual}}.
#'
#' @param pixel_matrix 2D matrix of pixels to classify (rows = pixels, columns =
#'   channels).
#' @param pixel_assignments A vector of color center assignments for each
#'   pixel. Must match the order of `pixel_matrix`.
#' @param color_centers A matrix of color centers, with rows as centers and
#'   columns as color channels. Rows are assumed to match the index values of
#'   `pixel_assignments`, e.g. a pixel assigned `1` in the assignment vector
#'   is assigned to the color in the first row of `color_centers`.
#' @param color_space Color space in which to calculate distances. One of
#'   "sRGB", "Lab", "Luv", or "XYZ". Passed to
#'   \code{\link[grDevices]{convertColor}}.
#' @param metric Distance metric to be used for calculating pairwise pixel
#'   distances in the given color space; passed to \code{\link[stats]{dist}}.
#' @param ref_white Passed to \code{\link[grDevices]{convertColor}} if
#'   `color_space = "Lab`. Reference white for CIE Lab space.
#'
#' @return
#' A list with the following attributes:
#' \enumerate{
#'     \item `sq_residuals`: The squared residual for every pixel in pixel_matrix.
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
#' pixel_matrix <- matrix(runif(3000), ncol = 3)
#'
#' # assign pixels
#' # see `assignPixels` function for details
#' reassigned <- assignPixels(ctrs, pixel_matrix, adjust_centers = TRUE)
#'
#' # find residuals from original color centers
#' color_residuals <- colorResiduals(pixel_matrix = pixel_matrix,
#'                                   pixel_assignments = reassigned$pixel_assignments,
#'                                   color_centers = ctrs)
#'
#' # compare to residuals from adjusted color centers
#' color_residuals_adjust <- colorResiduals(pixel_matrix = pixel_matrix,
#'                                   pixel_assignments = reassigned$pixel_assignments,
#'                                   color_centers = reassigned$centers)
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
colorResiduals <- function(pixel_matrix,
                           pixel_assignments,
                           color_centers,
                           color_space = "Lab",
                           metric = "euclidean",
                           ref_white = "D65") {

  # make sure all the pixels are assigned to a center that exists
  if (any(!unique(pixel_assignments) %in% 1:nrow(color_centers))) {
    stop("Not all pixel assignments correspond to a provided color center.")
  }

  if (color_space != "sRGB") {
    pixel_matrix <- grDevices::convertColor(pixel_matrix,
                                            from = "sRGB",
                                            to = color_space,
                                            to.ref.white = ref_white)
    color_centers <- grDevices::convertColor(color_centers,
                                             from = "sRGB",
                                             to = color_space,
                                             to.ref.white = ref_white)
  }

  # calculate all squared residuals
  sq_residuals <- sapply(1:length(pixel_assignments),
                         function(i) stats::dist(rbind(pixel_matrix[i, ],
                                   color_centers[pixel_assignments[i], ]),
                                                 method = metric))

  # make a list of residuals by color center
  residuals_by_center <- vector("list", length = nrow(color_centers))

  # sort it
  # there's a less dumb way to do this but I don't...care
  for (i in 1:nrow(color_centers)) {
    residuals_by_center[[i]] <- sq_residuals[which(pixel_assignments == i)]
  }

  # returnables
  return(list(sq_residuals = sq_residuals,
              tot_residuals = sum(sq_residuals),
              avg_residual = mean(sq_residuals),
              residuals_by_center = residuals_by_center,
              avg_by_center = lapply(residuals_by_center, mean)))

}
