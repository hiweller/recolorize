% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorResiduals.R
\name{colorResiduals}
\alias{colorResiduals}
\title{Calculate squared residuals for color centers}
\usage{
colorResiduals(
  pixel.matrix,
  pixel.assignments,
  color.centers,
  color.space = "Lab",
  metric = "euclidean",
  ref.white = "D65"
)
}
\arguments{
\item{pixel.matrix}{2D matrix of pixels to classify (rows = pixels, columns =
channels).}

\item{pixel.assignments}{A vector of color center assignments for each
pixel. Must match the order of \code{pixel.matrix}.}

\item{color.centers}{A matrix of color centers, with rows as centers and
columns as color channels. Rows are assumed to match the index values of
\code{pixel.assignments}, e.g. a pixel assigned \code{1} in the assignment vector
is assigned to the color in the first row of \code{color.centers}.}

\item{color.space}{Color space in which to calculate distances. One of
"sRGB", "Lab", "Luv", or "XYZ". Passed to
\code{\link[grDevices]{convertColor}}.}

\item{metric}{Distance metric to be used for calculating pairwise pixel
distances in the given color space; passed to \code{\link[stats]{dist}}.}

\item{ref.white}{Passed to \code{\link[grDevices]{convertColor}} if
\verb{color.space = "Lab}. Reference white for CIE Lab space.}
}
\value{
A list with the following attributes:
\enumerate{
\item \code{sq_residuals}: The squared residual for every pixel in pixel.matrix.
\item \code{tot_residuals}: The sum of all squared residuals.
\item \code{avg_residual}: The average squared residual.
\item \code{residuals_by_center}: A list of squared residuals for every color center.
\item \code{avg_by_center}: The average squared residual for every color center.
}
}
\description{
Calculates the squared distance between each pixel and its assigned color
center. Mostly for internal use by \code{\link{classifyColorManual}}.
}
\examples{
# RGB extremes (white, black, red, green, blue, yellow, magenta, cyan)
ctrs <- matrix(c(1, 1, 1,
                 0, 0, 0,
                 1, 0, 0,
                 0, 1, 0,
                 0, 0, 1,
                 1, 1, 0,
                 1, 0, 1,
                 0, 1, 1), byrow = TRUE, ncol = 3)

# plot it
recolorize::plotColorPalette(ctrs)

# create a pixel matrix of random colors
pixel.matrix <- matrix(runif(3000), ncol = 3)

# assign pixels
# see `assignPixels` function for details
reassigned <- assignPixels(ctrs, pixel.matrix, adjust.centers = TRUE)

# find residuals from original color centers
color_residuals <- colorResiduals(pixel.matrix = pixel.matrix,
                                  pixel.assignments = reassigned$pixel.assignments,
                                  color.centers = ctrs)

# compare to residuals from adjusted color centers
color_residuals_adjust <- colorResiduals(pixel.matrix = pixel.matrix,
                                  pixel.assignments = reassigned$pixel.assignments,
                                  color.centers = reassigned$centers)

layout(matrix(1:2, nrow = 2))
hist(color_residuals$sq_residuals,
 breaks = 30, border = NA, col = "tomato",
 xlim = c(0, 1), xlab = "Squared residual",
 main = "Original centers")

hist(color_residuals_adjust$sq_residuals,
breaks = 30, border = NA, col = "cornflowerblue",
xlim = c(0, 1), xlab = "Squared residual",
main = "Adjusted centers")

}