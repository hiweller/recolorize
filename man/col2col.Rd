% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorClusters.R
\name{col2col}
\alias{col2col}
\title{Modified convertColor}
\usage{
col2col(
  pixel_matrix,
  from = c("sRGB", "Lab", "Luv", "HSV"),
  to = c("sRGB", "Lab", "Luv", "HSV"),
  ref_white = "D65"
)
}
\arguments{
\item{pixel_matrix}{A matrix of pixel colors, rows are pixels and columns
are channels.}

\item{from}{Color space to convert from.}

\item{to}{Color space to convert to.}

\item{ref_white}{Reference white.}
}
\value{
A pixel matrix in the specified \code{to} color space.
}
\description{
Just like \link[grDevices:convertColor]{grDevices::convertColor}, but with HSV as an option.
}
\details{
As my mother used to say: good enough for government work.
}
