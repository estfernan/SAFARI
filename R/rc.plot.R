# ******************************************************************************
#
# R package SAFARI by Esteban Fernández and Qiwei Li
# Copyright (C) 2021
#
# This file is part of the R package SAFARI.
#
# The R package SAFARI is free software: You can redistribute it and/or
# modify it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or any later
# version (at your option). See the GNU General Public License at
# <https://www.gnu.org/licenses/> for details.
#
# The R package SAFARI is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# ******************************************************************************

#' Reconstructed Image Plot
#'
#' Plot a reconstructed image obtained from various types of modalities.
#'
#' The argument \code{type} specifies the image type with the following options:
#' \itemize{
#'     \item binary: reconstructed binary image, representing a greyscale medical image;
#'     \item segments: segmented matrix, where each region is denoted by an integer value.
#' }
#'
#' @param x           an integer matrix that represents the reconstructed image.
#' @param type        a character string that specifies the image type.
#' @param publication a logical value, whether to create a figure without key nor scales.
#' @param ...         additional graphical parameters passed to \code{\link[lattice]{levelplot}}.
#'
#' @return No return value, called for side effects i.e. plotting.
#'
#' @seealso
#' \code{\link{binary.segmentation}} for more information on reconstructed images.
#'
#' @export
#' @importFrom lattice levelplot
#'
rc.plot <- function(
    x, type = c("binary", "segments"),
    publication = FALSE,
    ...
)
{
    # default labels
    l <- c("Empty", "Tumor", "Non-Malignant")

    # dimensions
    L <- nrow(x) # x-value
    W <- ncol(x) # y-value

    # number of regions
    s <- max(x)

    # colors and labels
    info <- switch(type,
                   binary = list(cols = 1:0, labels = l[1:2]),
                   segments = list(cols = 0:s, labels = 0:s),
                   stop("argument 'type' is not a valid option")
    )

    # publication style
    if (publication)
    {
        # hide scales
        scales <- list(x = list(at = NULL), y = list(at = NULL))

        # hide color key
        region <- FALSE
    }

    # default style
    else
    {
        # left-bottom axes
        scales <- list(tck = 1:0)

        # properly segment key
        region <- list(labels = list(at = seq(.5, s + 1, 1), labels = info$labels))
    }

    # create figure
    plot(
        levelplot(x, aspect = "iso",
                  scales = scales,
                  xlab = "", xlim = c(0, L),
                  ylab = "", ylim = c(0, W),
                  at = 0:(s + 1) - 0.1,
                  pretty = TRUE,
                  useRaster = FALSE,
                  colorkey = region,
                  col.regions = info$cols,
                  ...)
    )
}
