% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mergeLayers.R
\name{clean_merge_params}
\alias{clean_merge_params}
\title{Clean up parameters passed to mergeLayers}
\usage{
clean_merge_params(recolorize_obj, merge_list, color_to)
}
\arguments{
\item{recolorize_obj}{Object of \code{recolorize} class.}

\item{merge_list}{List of layers to merge.}

\item{color_to}{Argument for coloring new layers.}
}
\value{
A list of \code{mergeLayers} parameters in a standardized format.
}
\description{
Internal function for tidiness.
}
