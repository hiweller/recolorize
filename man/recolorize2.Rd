% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recolorize2.R
\name{recolorize2}
\alias{recolorize2}
\title{Recolorize with automatic thresholding}
\usage{
recolorize2(
  img,
  method = "histogram",
  bins = 2,
  n = 5,
  cutoff = 20,
  channels = 1:3,
  n_final = NULL,
  color_space = "sRGB",
  recluster_color_space = "Lab",
  refit_method = "impose",
  ref_white = "D65",
  lower = NULL,
  upper = NULL,
  transparent = TRUE,
  resize = NULL,
  rotate = NULL,
  plotting = TRUE
)
}
\arguments{
\item{img}{Path to the image (a character vector) or a 3D image array as read
in by \code{\link[png:readPNG]{png::readPNG()}} \code{{readImage}}.}

\item{method}{Method for clustering image colors. One of either \code{histogram}
or \code{kmeans}. See details.}

\item{bins}{If \code{method = "histogram"}, either the number of bins per color
channel (if a single number is provided) OR a vector of length 3 with the
number of bins for each channel.}

\item{n}{If \code{method = "kmeans"}, the number of color clusters to fit.}

\item{cutoff}{Numeric similarity cutoff for grouping color centers
together. The range is in absolute Euclidean distance. In CIE Lab space,
it is greater than 0-100, but cutoff values between 20 and 80
will usually work best. In RGB space, range is 0-sqrt(3).
See \link{recluster} details.}

\item{channels}{Numeric: which color channels to use for clustering.
Probably some combination of 1, 2, and 3, e.g., to consider only luminance
and blue-yellow (b-channel) distance in CIE Lab space, channels = c(1, 3
(L and b).}

\item{n_final}{Final number of desired colors; alternative to specifying
a similarity cutoff. Overrides \code{similarity_cutoff} if provided.}

\item{color_space}{Color space in which to minimize distances, passed to
\code{[grDevices]{convertColor}}. One of "sRGB", "Lab", or "Luv".
Default is "sRGB".}

\item{recluster_color_space}{Color space in which to group colors for
reclustering. Default is CIE Lab.}

\item{refit_method}{Method for refitting the image with the new color
centers. One of either "impose" or "merge". \code{\link[=imposeColors]{imposeColors()}}
refits the original image using the new colors (slow but often better
results). \code{\link[=mergeLayers]{mergeLayers()}} merges the layers of the existing
recolored image. This is faster since it doesn't require a new fit, but can
produce messier results.}

\item{ref_white}{Reference white for converting to different color spaces.
D65 (the default) corresponds to standard daylight.}

\item{lower, upper}{RGB triplet ranges for setting a bounding box of pixels to
mask. See details.}

\item{transparent}{Logical. Treat transparent pixels as background? Requires
an alpha channel (PNG).}

\item{resize}{A value between 0 and 1 for resizing the image (ex. \code{resize = 0.5} will reduce image size by 50\%). Recommended for large images as it can
speed up analysis considerably. See details.}

\item{rotate}{Degrees to rotate the image clockwise.}

\item{plotting}{Logical. Plot final results?}
}
\value{
An object of S3 class \code{recolorize} with the following attributes:
\enumerate{
\item \code{original_img}: The original image, as a raster array.
\item \code{centers}: A matrix of color centers in RGB (0-1 range).
\item \code{sizes}: The number of pixels assigned to each color cluster.
\item \code{pixel_assignments}: A matrix of color center assignments for each
pixel.
\item \code{call}: The call(s) used to generate the \code{recolorize} object.
}
}
\description{
Calls \link{recolorize} and \link{recluster} in sequence, since these are often
very effective in combination.
}
\examples{
# get image path
img <- system.file("extdata/corbetti.png", package = "recolorize")

# fit recolorize:
rc <- recolorize2(img, bins = 2, cutoff = 45)

}
\seealso{
\link{recolorize}, \link{recluster}
}
