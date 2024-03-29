% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blurImage.R
\name{blurImage}
\alias{blurImage}
\title{Blur an image}
\usage{
blurImage(
  img,
  blur_function = c("medianblur", "isoblur", "blur_anisotropic", "boxblur", "boxblur_xy"),
  ...,
  plotting = TRUE
)
}
\arguments{
\item{img}{An image array, as read in by \link[png:readPNG]{png::readPNG} or \link{readImage}.}

\item{blur_function}{A string matching the name of an imager blur function.
One of c("isoblur", "medianblur", "blur_anisotropic", "boxblur",
"boxblur_xy").}

\item{...}{Parameters passed to whichever \code{blur_function} is called.}

\item{plotting}{Logical. Plot the blurred image next to the input
for comparison?}
}
\value{
An image array of the blurred image.
}
\description{
Blurs an image using the one of five blur functions in \code{imager}.
Useful for decreasing image noise.
}
\details{
The parameters passed with the \code{...} argument are specific
to each of the five blur functions; see their documentation for what to
specify: \link[imager:isoblur]{imager::isoblur}, \link[imager:medianblur]{imager::medianblur}, \link[imager:boxblur]{imager::boxblur},
\link[imager:blur_anisotropic]{imager::blur_anisotropic}, \link[imager:boxblur_xy]{imager::boxblur_xy}. The \code{medianblur} and
\code{blur_anisotropic} functions are best for preserving edges.
}
\examples{
img_path <- system.file("extdata/fulgidissima.png", package = "recolorize")
img <- readImage(img_path)
median_img <- blurImage(img, "medianblur", n = 5, threshold = 0.5)
anisotropic_img <- blurImage(img, "blur_anisotropic",
                             amplitude = 5, sharpness = 0.1)
boxblur_img <- blurImage(img, "boxblur", boxsize = 5)

# save current graphical parameters:
current_par <- graphics::par(no.readonly = TRUE)
graphics::layout(matrix(1:4, nrow = 1))

plotImageArray(img, "original")
plotImageArray(median_img, "median")
plotImageArray(anisotropic_img, "anisotropic")
plotImageArray(boxblur_img, "boxblur")

# and reset:
graphics::par(current_par)
}
