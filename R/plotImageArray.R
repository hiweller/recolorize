plotImageArray <- function(rgb.array, main = "") {

  # Make sure the array is 3-dimensional
  if (length(dim(rgb.array)) != 3) {
    stop("RGB_array must be an array of three dimensions (pixel rows,
             pixel columns, and color channels)")
  }

  # Change graphical parameters for image display
  op <- graphics::par(mar = c(0, 0, 2, 0))
  asp <- dim(rgb.array)[1] / dim(rgb.array)[2]

  # Initialize empty plot window
  graphics::plot(0:1, 0:1, type = "n", ann = F, axes = F, asp = asp)

  # Use rasterImage to actually plot the image
  graphics::rasterImage(rgb.array, 0, 0, 1, 1)
  graphics::title(main, line = 0)

  # Return to original graph window settings
  graphics::par(op)
}
