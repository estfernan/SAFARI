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

#' Segmentation Procedure for Binary Images
#'
#' Identify and segment the regions of interest (ROI) in a reconstructed binary
#' image, while having the option to extract quantitative shape-based features.
#'
#' The argument \code{id} specifies the ID's corresponding to the sample.
#' A usual example that is used if the argument is an unnamed vector is composed
#' of the following entries:
#' \itemize{
#'     \item \code{cohort}: name of cohort the sample belongs to;
#'     \item \code{patient.id}: unique identifier for patient the sample belongs to;
#'     \item \code{slide.id}: unique identifier for sample.
#' }
#'
#' The argument \code{filter} specifies how the ROI should be filtered. There
#' are two ways to filter them, either by only specifying a minimum net area or
#' by additionally specifying the \eqn{n} largest regions to keep in a two element
#' vector. The default value is \code{NA} which sets the minimum net area as
#' one-fourth the largest region.
#'
#' The argument \code{categories} specifies the shape features to compute. The
#' default is "none" which computes no features.
#'
#' @section Reconstructed Binary Image:
#'
#' To produce a reconstructed binary image, a greyscale scan or an image easily
#' converted to binary, resulting from different modalities, is converted
#' to a matrix representation, usually by standard image processing algorithms.
#'
#' The resulting matrix is composed of two integer values that help represent the
#' regions of interest in the scan. Usually, as in the case of pathology
#' imaging, these are empty regions and tumor tissues, where we refer to the
#' integer values as categories. We note that the regions of interest must be
#' represented by \code{1}'s and what we consider the empty regions by \code{0}'s.
#'
#' @param x          a binary matrix that represents the reconstructed image.
#' @param id         a named character vector that contains the ID's pertaining to the sample.
#' @param filter     an integer vector that indicates the filtering procedure.
#' @param k          an integer that specifies the factor to enlarge the regions.
#' @param categories a character string or vector, see \code{\link{compute.features}}.
#'
#' @return A \code{list} object whose components are the following:
#' \itemize{
#'     \item \code{desc}:       a \code{data.frame} of the shape features
#'                              corresponding to each segmented ROI;
#'     \item \code{holes}:      an integer matrix that contains the holes within
#'                              each ROI, labeled according to
#'                              the \code{regions};
#'     \item \code{id}:         a character vector that is identical to the
#'                              \code{id} argument passed;
#'     \item \code{k}:          argument \code{k} passed to function;
#'     \item \code{n}:          an integer that indicates the number of
#'                              segmented regions;
#'     \item \code{plg.chains}: a \code{list} where each component is the
#'                              polygonal chain of a segmented ROI;
#'     \item \code{regions}:    an integer matrix that contains the segmented
#'                              ROI, labeled from largest to smallest.
#' }
#'
#' @example inst/examples/ex_binary.R
#'
#' @export
#'
binary.segmentation <- function(
    x, id,
    filter = NA,
    k = 3,
    categories = c("none", "geometric", "boundary", "topological")
)
{
    # empty image check
    if (is.empty(x))
    {
        stop("argument 'x' is an empty matrix")
    }

    # empty ID's
    if (length(id) == 3 & !is.named(id))
    {
        names(id) <- c("cohort", "patient.id", "slide.id")
    }

    # categories check
    v <- ifelse("none" %in% categories, NA, 0)

    # initial values
    n <- length(filter)

    # default filters
    min.area  <- NA
    n.largest <- NA
    minimum   <- NA

    # argument length check
    if (n > 2)
    {
        warning("only the first two elements in 'filter' will be used")
    }

    # filtering check
    if (n == 2)
    {
        minimum   <- filter[1]
        n.largest <- filter[2]
    }
    else if (n == 1)
    {
        if (!is.na(filter))
        {
            min.area  <- filter
        }
    }

    # identify and segment tumor regions
    seg <- image.fsf(x,
        min.area = min.area,
        n.largest = n.largest,
        minimum = minimum
    )

    # number of tumors
    s <- max(seg)

    # no regions after filtering
    if (s <= 0)
    {
        stop("no regions of interest present after filtering procedure")
    }

    # segmented shape objects
    regions    <- seg * x
    holes      <- seg - regions
    plg.chains <- polygonal.chain.list(seg, k = k)

    # compute shape features
    if (!is.na(v))
    {
        v <- compute.features.list(regions, plg.chains, id, categories)
    }

    # prepare results
    list(desc = v,
         holes      = holes,
         id         = id,
         k          = k,
         n          = s,
         plg.chains = plg.chains,
         regions    = regions)
}
