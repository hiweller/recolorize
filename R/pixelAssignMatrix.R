#' Make pixel assignment matrix for recoloring
#'
#' Internal function. Generates a sort of 'paint-by-numbers' matrix, where each
#' cell is the index of the color in the color centers matrix to which that
#' pixel is assigned. An index of 0 indicates a background pixel.
#'
#' @param bg_indexed An object returned by \code{\link{backgroundIndex}}.
#' @param color_clusters An object returned by \code{\link{colorClusters}}.
#'
#' @return A matrix of pixel color assignments (`pixel_assignments`)
#' and a corresponding dataframe of color centers (`centers`).
pixelAssignMatrix <- function(bg_indexed, color_clusters) {

  # make a vector of 0's, one per image pixel
  pix_assign <- rep(0, nrow(bg_indexed$flattened_img))

  # swap in the color assignments for the pixels
  if (length(bg_indexed$idx_flat) == 0) {
    pix_assign <- color_clusters$pixel_assignments
  } else {
    pix_assign[-bg_indexed$idx_flat] <- color_clusters$pixel_assignments
  }

  # and reshape:
  dim(pix_assign) <- bg_indexed$img_dims[1:2]

  # return it!
  return(list(pixel_assignments = pix_assign,
              centers = color_clusters$centers))

}

