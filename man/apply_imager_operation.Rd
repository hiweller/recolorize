% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_imager_operation.R
\name{apply_imager_operation}
\alias{apply_imager_operation}
\title{Apply imager operations to layers of an image}
\usage{
apply_imager_operation(pixset, imager_function, ...)
}
\arguments{
\item{pixset}{An object of class \code{pixset}. Usually a layer from
\code{\link{splitByColor}} that has been converted to a \code{pixset}
object.}

\item{imager_function}{The name of an imager morphological operation that can
be performed on a pixset, passed as a string. See details.}

\item{...}{Further arguments passed to the imager function being used.}
}
\value{
The resulting pixset after applying the specified morphological
operation.
}
\description{
Internal wrapper function for applying any of several
\code{imager} morphological operations for cleaning pixsets.
}
\details{
Current imager operations are:
\itemize{
\item \code{\link[imager]{grow}}: Grow a pixset
\item \code{\link[imager]{shrink}}: Shrink a pixset
\item \code{\link[imager]{fill}}: Remove holes in an pixset. Accomplished by
growing and then shrinking a pixset.
\item \code{\link[imager]{clean}}: Remove small isolated elements (speckle).
Accomplished by shrinking and then growing a pixset.
}
}
\examples{
img <- system.file("extdata/chongi.png", package = "recolorize")
recolorize_obj <- recolorize(img)
layers <- splitByColor(recolorize_obj)
init_layer <- imager::as.cimg(layers[[2]])

recolorize:::apply_imager_operation(init_layer, "clean", 4)
}
