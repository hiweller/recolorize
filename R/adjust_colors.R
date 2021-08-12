#' Adjust the saturation and brightness of a color
#'
#' Adjusts the saturation and brightness of RGB colors.
#'
#' @param rgb_color Matrix of RGB colors (0-1 scale).
#' @param which_colors The indices of the colors to change. Can be a numeric
#'   vector or "all" to adjust all colors.
#' @param saturation Factor by which to multiply saturation. > 1 = more saturated,
#' < 1 = less saturated.
#' @param brightness Factor by which to multiply brightness.
#' @param plotting Logical. Plot resulting color palettes?
#'
#' @return A matrix of adjusted RGB colors.
#'
#' @examples
#' # generate a palette:
#' p <- grDevices::palette.colors()
#'
#' # convert to RGB using col2rgb, then divide by 255 to get it into a
#' # 0-1 range:
#' p <- t(col2rgb(p)/ 255 )
#'
#' # we can adjust the saturation and brightness by the same factor:
#' p_1 <- adjust_color(p, saturation = 2,
#'                     brightness = 1.5,
#'                     plotting = TRUE)
#'
#' # or we can pass a vector for the factors:
#' p_2 <- adjust_color(p,
#'                     saturation = seq(0, 2, length.out = 9),
#'                     plotting = TRUE)
#'
#' # or we can target a single color:
#' p_3 <- adjust_color(p, which_colors = 4,
#'                     saturation = 2, brightness = 2,
#'                     plotting = TRUE)
#'
#' @export
adjust_color <- function(rgb_color,
                         which_colors = "all",
                         saturation = 1,
                         brightness = 1,
                         plotting = FALSE) {

  original_rgb <- rgb_color

  if (which_colors == "all") {
    which_colors <- 1:nrow(rgb_color)
  }

  # convert to HSV
  rgb_color_temp <- matrix(rgb_color[which_colors, ], ncol = 3)
  as_hsv <- col2col(rgb_color_temp, to = "HSV")

  # multiply
  as_hsv[ , 2] <- as_hsv[ , 2] * saturation
  as_hsv[ , 3] <- as_hsv[ , 3] * brightness

  # rescale
  as_hsv[which(as_hsv > 1)] <- 1

  # convert to RGB
  as_rgb <- col2col(as_hsv, "HSV", "sRGB")

  rgb_color[which_colors, ] <- as_rgb

  if (plotting) {

    # courtesy:
    current_par <- graphics::par()

    graphics::layout(matrix(1:2, nrow = 2))
    graphics::par(mar = rep(1, 4))
    plotColorPalette(original_rgb)
    plotColorPalette(rgb_color)

    # be nice!
    graphics::par(mfrow = current_par$mfrow,
                  mar = current_par$mar)
  }

  # spit out
  return(rgb_color)
}


