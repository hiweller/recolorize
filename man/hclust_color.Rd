% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hclust_color.R
\name{hclust_color}
\alias{hclust_color}
\title{Plot and group colors by similarity}
\usage{
hclust_color(
  rgb_centers,
  dist_method = "euclidean",
  hclust_method = "complete",
  channels = 1:3,
  color_space = "Lab",
  ref_white = "D65",
  cutoff = NULL,
  n_final = NULL,
  return_list = TRUE,
  plotting = TRUE
)
}
\arguments{
\item{rgb_centers}{A matrix of RGB centers. Rows are centers and columns
are R, G, and B values.}

\item{dist_method}{Method passed to \link[stats:dist]{stats::dist}. One of  "euclidean",
"maximum", "manhattan", "canberra", "binary" or "minkowski".}

\item{hclust_method}{Method passed to \link[stats:hclust]{stats::hclust}. One of "ward.D",
"ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA),
"median" (= WPGMC) or "centroid" (= UPGMC).}

\item{channels}{Numeric: which color channels to use for clustering. Probably
some combination of 1, 2, and 3, e.g., to consider only luminance and
blue-yellow (b-channel) distance in CIE Lab space, \verb{channels = c(1, 3} (L
and b).}

\item{color_space}{Color space in which to do the clustering.}

\item{ref_white}{Reference white for converting to different color spaces.
D65 (the default) corresponds to standard daylight. See
\link[grDevices:convertColor]{grDevices::convertColor}.}

\item{cutoff}{Either \code{NULL} or a numeric cutoff passed to \link[stats:cutree]{stats::cutree}.
Distance below which to combine clusters, i.e. height at which the tree
should be cut.}

\item{n_final}{Numeric. Desired number of groups. Overrides \code{cutoff} if
both are provided.}

\item{return_list}{Logical. Return a list of new group assignments from
the \code{cutoff} or \code{n_final} values?}

\item{plotting}{Logical. Plot a colored dendrogram?}
}
\value{
A list of group assignments (i.e. which centers belong to which
groups), if \code{return_list = TRUE}.
}
\description{
A wrapper for \link[stats:hclust]{stats::hclust} for clustering colors by similarity.
This works by converting a matrix of RGB centers to a given color space
(CIE Lab is the default), generating a distance matrix for those colors
in that color space (or a subset of channels of that color space),
clustering them, and plotting them with labels and colors. If either a
cutoff or a final number of colors is provided and \code{return_list = TRUE},
function also returns a list of which color centers to combine.
}
\details{
This is mostly useful in deciding where and in which color space
to place a cutoff for a \code{recolorize} object, since it is very fast. It
is called by \link{recluster} when combining layers by similarity.
}
\examples{

# 50 random RGB colors
rgb_random <- matrix(runif(150), nrow = 50, ncol = 3)

# default clustering (Lab space):
hclust_color(rgb_random, return_list = FALSE)

# clustering in RGB space (note change in Y-axis scale):
hclust_color(rgb_random, color_space = "sRGB", return_list = FALSE)

# clustering using only luminance:
hclust_color(rgb_random, channels = 1, return_list = FALSE)

# or only red-green ('a' channel):
hclust_color(rgb_random, channels = 2, return_list = FALSE)

# or only blue-yellow ('b' channel(:
hclust_color(rgb_random, channels = 3, return_list = FALSE)

# use a cutoff to get groups:
groups <- hclust_color(rgb_random, cutoff = 100)
print(groups)

}
\seealso{
\link{recluster}
}
