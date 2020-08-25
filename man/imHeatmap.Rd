% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imDist.R
\name{imHeatmap}
\alias{imHeatmap}
\title{Plot a heatmap of a matrix of color distances}
\usage{
imHeatmap(
  mat,
  palette = "default",
  main = "",
  range = NULL,
  legend = TRUE,
  ...
)
}
\arguments{
\item{mat}{A color distance matrix, preferably output of
\code{\link{imDist}}.}

\item{palette}{The color palette to be used. Default is blue to
red (\code{colorRamps::blue2red(100)}).}

\item{main}{Plot title.}

\item{range}{Range for heatmap values. Defaults to the range of values in the
matrix, but should be set to the same range for all images if comparing
heatmaps.}

\item{legend}{Logical. Add a continuous color legend?}

\item{...}{Parameters passed to \code{\link[graphics]{image}}.}
}
\description{
Plots the output of \code{\link{imDist}} as a heatmap.
}
\examples{
chongi <- system.file("extdata/chongi.png", package = "recolorize")
chongi_k <- recolorize(chongi, "k", n = 5)

d <- imDist(chongi_k$original.img,
            chongi_k$recolored.img, plotting = FALSE)

# original flavor
imHeatmap(d)

# bit offputting
imHeatmap(d, palette = colorRamps::ygobb(100))

# just dreadful
imHeatmap(d, palette = colorRamps::primary.colors(100))
}