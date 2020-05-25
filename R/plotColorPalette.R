#' Plot a color palette
#'
#' Plots a color palette as a single bar, optionally scaling each color to a
#' vector of sizes.
#'
#' @param color.centers Colors to plot in palette, as an n x 3 matrix (rows =
#'   colors, columns = channels). Assumes RGB in 0-1 range.
#' @param sizes An optional numeric vector of sizes for scaling each color. If
#'   no sizes are provided, colors are plotted equal proportions.
#' @param cex.text Size of the numbers displayed on each color, relative to the
#'   default. Passed to \code{\link[graphics]{barplot}}. Text is only plotted if
#'   `sizes = NULL`.
#' @param horiz Logical. Should the palette be plotted vertically or
#'   horizontally?
#' @param ... Additional parameters passed to \code{\link[graphics]{barplot}}.
#'
#' @details
#' `plotColorPalette` does not reorder or convert colors between color spaces,
#' so users working in other colorspaces should convert to RGB before plotting.
#'
#' @examples
#' # plot 10 random colors
#' rand.colors <- matrix(runif(30), ncol = 3)
#' plotColorPalette(rand.colors)
#'
#' # plot 10 random colors with arbitrary sizes
#' sizes <- runif(10, max = 1000)
#' plotColorPalette(rand.colors, sizes = sizes)
#'
#' # reorder to plot smallest to largest
#' size.order <- order(sizes)
#' plotColorPalette(rand.colors[size.order, ],
#'                  sizes[size.order])
#'
#' @export
plotColorPalette <- function(color.centers, sizes = NULL,
                             cex.text = 2, horiz = TRUE, ...) {

  # make color vector
  rgb.exp <- grDevices::rgb(color.centers[, 1],
                            color.centers[, 2],
                            color.centers[, 3])

  # make a plot
  if (is.null(sizes)) {

    # if sizes are not included, make bars equal in size
    colorbar <- rep(1, nrow(color.centers))
    stats::setNames(colorbar, as.character(1:nrow(color.centers)))

    # we're making the palettes in different ways, so horizontal plotting is
    # reversed; this makes it consistent
    horiz <- !horiz

  } else {

    # if so, make a fake "table" with counts
    # this is a bit hacky, but it does make the bars adjacent instead of stacked
    sizes <- sizes / sum(sizes)
    sizes <- round(sizes * 1000)
    colorbar <- unlist(sapply(1:length(sizes),
                              function(j) rep(j, sizes[j])))
    colorbar <- table(colorbar, rep("", length(colorbar)))

    # remove any empty values
    if (any(sizes == 0)) {
      rgb.exp <- rgb.exp[-which(sizes == 0)]
    }

  }


  # plot the colors as a uniform bar
  graphics::barplot(colorbar, col = rgb.exp,
                    axes = FALSE, space = 0, horiz = horiz,
                    border = NA, axisnames = FALSE, ...)

  # text colors - black if the color is light, white if the color is dark
  hsv.exp <- grDevices::rgb2hsv(t(color.centers), maxColorValue = 1)
  text.colors <- round(hsv.exp[3, ]) + 1

  # make text locations
  if (horiz == FALSE) {
    text.x <- seq(0.5, length(rgb.exp) - 0.5)
    text.y <- 0.5
  } else {
    text.y <- seq(0.5, length(rgb.exp) - 0.5)
    text.x <- 0.5
  }

  # only plot numbers if the sizes are equal
  # when they're distorted by sizes it gets too wacky
  if (is.null(sizes)) {
    graphics::text(text.x, text.y,
                   cex = cex.text,
                   col = c("white", "black")[text.colors])
  }

}
