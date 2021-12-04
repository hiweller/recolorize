# a function to...with the werner colors...whatever...
#' Remap an image to Werner's nomenclature
#'
#' Remaps a recolorize object to the colors in Werner's Nomenclature of Colors
#' by Patrick Syme (1821), one of the first attempts at an objective color
#' reference in western science, notably used by Charles Darwin.
#'
#' @param recolorize_obj A recolorize object as returned by
#'   \code{\link{recolorize}}, \code{\link{recluster}}, or
#'   \code{\link{imposeColors}}.
#' @param which_img Which image to recolor; one of either "original" or
#'   "recolored".
#' @param n_colors Number of colors to list out in plotting, in order of
#'   size. Ex: `n_colors = 5` will plot the 5 largest colors and their names.
#'   All colors are returned as a normal recolorize object regardless of
#'   `n_colors`; this only affects the plot.
#'
#' @return
#' A `recolorize` object with an additional list element, `werner_names`,
#' listing the Werner color names for each center.
#'
#' @details
#' See \url{https://www.c82.net/werner/} to check out the original colors.
#'
#' @examples
#'
#'
#' # get an initial fit:
#' corbetti <- system.file("extdata/corbetti.png", package = "recolorize")
#' recolored_corbetti <- recolorize(corbetti, plotting = FALSE)
#'
#' # recolor original image
#' corbetti_werner <- wernerColor(recolored_corbetti,
#'                                which_img = "original",
#'                                n_colors = 6)
#'
#' # we can simplify the colors and then do it again:
#' corbetti_recluster <- recluster(recolored_corbetti,
#'                                 cutoff = 45,
#'                                 plot_hclust = FALSE)
#' corbetti_werner <- wernerColor(corbetti_recluster,
#'                                which_img = "recolored")
#'
#' @export
wernerColor <- function(recolorize_obj,
                        which_img = "original",
                        n_colors = 5) {

  # first, convert werner colors to lab
  werner_rgb <- t(grDevices::col2rgb(recolorize::werner$hex)) / 255
  werner_lab <- grDevices::convertColor(werner_rgb,
                                        "sRGB", "Lab")
  # get centers
  centers <- recolorize_obj$centers
  centers <- grDevices::convertColor(centers,
                                     "sRGB", "Lab")

  # get distances
  tmp <- sapply(1:nrow(centers),
                function(i) apply(werner_lab, 1,
                                  function(v) sqrt(sum((centers[i, ]-v)^2))))

  # find index of min distance
  werner_idx <- max.col(-t(tmp))

  # get new colors
  werner_centers <- werner_rgb[werner_idx, ]

  # re-fit either original or recolored image
  which_img <- match.arg(which_img, c("original", "recolored"))

  # pick the image
  if (which_img == "original") {

    # make RGB array and refit
    img <- raster_to_array(recolorize_obj$original_img)
    werner_fit <- imposeColors(img,
                               werner_centers, adjust_centers = FALSE,
                               plotting = FALSE)

  } else {

    # we just have to swap out the centers
    werner_fit <- recolorize_obj
    werner_fit$centers <- werner_centers
  }


  # if n_colors is too big...
  n_colors <- min(c(n_colors, nrow(werner_fit$centers)))

  # text labels?
  idx <- order(werner_fit$sizes / sum(werner_fit$sizes),
               decreasing = TRUE)[1:n_colors]
  ctrs <- werner_fit$centers[idx, ]
  labels <- gsub(" ", "\n", recolorize::werner$name[werner_idx[idx]])
  cols <- recolorize::werner$hex[werner_idx[idx]]

  # set parameters and layout
  op <- graphics::par(mar = c(2, 0, 2, 0))
  graphics::layout(matrix(1:3, nrow = 1), widths = c(0.2, 0.55, 0.25))

  # plot the palette
  plotColorPalette(ctrs, cex_text = 0,
                   horiz = FALSE)

  # plot the recolored image
  plotImageArray(recoloredImage(werner_fit))

  # plot the labels
  graphics::plot(0:1, 0:1, ann = F, axes = F, type = "n")
  graphics::text(0.5, seq(0.1, 0.9, length.out = n_colors),
       labels = labels,
       col = cols,
       cex = 2, font = 2)

  # append the call
  werner_fit$call <- append(recolorize_obj$call, match.call())

  # return the fit
  werner_fit$werner_names <- recolorize::werner$name[werner_idx]
  return(werner_fit)

  graphics::par(op)

}

#' Werner's nomenclature of colors
#'
#' A table of the 110 colors described in "Werner's Nomenclature of Colors", the
#' 1821 color reference by Patrick Syme (building on work by Abraham Gottlob
#' Werner), notably used by Charles Darwin. Colors represent the average pixel
#' color of each scanned swatch.
#'
#' @format A data frame with 110 rows and 13 variables:
#' \describe{
#'   \item{index}{The color index.}
#'   \item{family}{The broad color category (white, red, etc).}
#'   \item{name}{The original color name.}
#'   \item{hex}{Color hex code.}
#' }
#' @source \url{https://www.c82.net/werner/#colors}
"werner"
