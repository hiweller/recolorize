% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotColorClusters.R
\name{plotColorClusters}
\alias{plotColorClusters}
\title{Plot color clusters in a color space}
\usage{
plotColorClusters(
  centers,
  sizes,
  scaling = 10,
  plus = 0,
  color_space = "sRGB",
  ...
)
}
\arguments{
\item{centers}{A matrix of color centers, with rows for centers and columns
as channels. These are interpreted as coordinates.}

\item{sizes}{A vector of color sizes. Can be relative or absolute; it's going
to be scaled for plotting.}

\item{scaling}{Factor for scaling the cluster sizes. If your clusters are
way too big or small on the plot, tinker with this.}

\item{plus}{Value to add to each scaled cluster size; can be helpful for
seeing small or empty bins when they are swamped by larger clusters.}

\item{color_space}{The color space of the centers. Important for setting the
axis ranges and for converting the colors into hex codes for plotting. The
function assumes that the \code{centers} argument is already in this color space.}

\item{...}{Further parameters passed to \link[scatterplot3d:scatterplot3d]{scatterplot3d::scatterplot3d}.}
}
\description{
Plots color clusters in a 3D color space.
}
\details{
This function does very little on your behalf (e.g. labeling the
axes, setting the axis ranges, trying to find nice scaling parameters,
etc). You can pass those parameters using the \code{...} function to
\link[scatterplot3d:scatterplot3d]{scatterplot3d::scatterplot3d}, which is probably a good idea.
}
\examples{
corbetti <- system.file("extdata/corbetti.png", package = "recolorize")
init_fit <- recolorize(corbetti,
                       color_space = "Lab",
                       method = "k",
                       n = 30)

# we still have to convert to Lab color space first, since the centers are always RGB:
centers <- grDevices::convertColor(init_fit$centers, "sRGB", "Lab")
plotColorClusters(centers, init_fit$sizes,
                  scaling = 25,
                  color_space = "Lab",
                  xlab = "Luminance",
                  ylab = "a (red-green)",
                  zlab = "b (blue-yellow)")
}
