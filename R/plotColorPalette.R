#' Plot a color palette
#'
#' Plots a color palette as a single bar, optionally scaling each color to a
#' vector of sizes.
#'
#' @param color.centers Colors to plot in palette. Accepts either a character
#'   vector of hex codes or an n x 3 matrix (rows = colors, columns =
#'   channels). Assumes RGB in 0-1 range.
#' @param sizes An optional numeric vector of sizes for scaling each color. If
#'   no sizes are provided, colors are plotted in equal proportions.
#' @param cex.text Size of the numbers displayed on each color, relative to the
#'   default. Passed to \code{\link[graphics]{barplot}}. Text is only plotted if
#'   `sizes = NULL`. `cex.text = 0` will remove numbering.
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
#' # plot a vector of hex colors, turn off numbering
#' hex_colors <- rgb(rand.colors)
#' plotColorPalette(hex_colors, cex.text = 0)
#'
#' @export

plotColorPalette <- function(color.centers, sizes = NULL,
                             cex.text = 2, horiz = TRUE, ...) {

  # check if hex codes
  if (is.vector(color.centers)) {
    if (sum(grepl("#", color.centers)) == length(color.centers)) {

      hex_colors <- color.centers

    } else {

      stop("'color.centers' must be either a numeric RGB matrix with colors\n
          as rows or a character vector of hex codes")

    }
  } else if (dim(color.centers)[2] != 3) {

    stop("'color.centers' must have colors as rows and RGB coordinates as columns")

    } else {

    # make color vector
    hex_colors <- grDevices::rgb(color.centers[, 1],
                              color.centers[, 2],
                              color.centers[, 3])


    }

  # get HSV colors
  hsv_colors <- grDevices::rgb2hsv(grDevices::col2rgb(hex_colors))

  # make a plot
  if (is.null(sizes)) {

    # if sizes are not included, make bars equal in size
    colorbar <- rep(1, length(hex_colors))
    stats::setNames(colorbar, as.character(1:length(hex_colors)))

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
      hex_colors <- hex_colors[-which(sizes == 0)]
    }

  }


  # plot the colors as a uniform bar
  graphics::barplot(colorbar, col = hex_colors,
                    axes = FALSE, space = 0, horiz = horiz,
                    border = NA, axisnames = FALSE, ...)

  # text colors - black if the color is light, white if the color is dark
  text.colors <- round(hsv_colors[3, ]) + 1

  # make text locations
  if (horiz == FALSE) {
    text.x <- seq(0.5, length(hex_colors) - 0.5)
    text.y <- 0.5
  } else {
    text.y <- seq(0.5, length(hex_colors) - 0.5)
    text.x <- 0.5
  }

  # only plot numbers if the sizes are equal
  # when they're distorted by sizes it gets too wacky
  if (is.null(sizes) & cex.text > 0) {
    graphics::text(text.x, text.y,
                   cex = cex.text,
                   col = c("white", "black")[text.colors])
  }

}
