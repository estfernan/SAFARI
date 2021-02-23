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

#' Shape Features
#'
#' Compute the shape descriptors of the given region, represented by its
#' binary matrix and the polygonal chain of its boundary.
#'
#' The shape descriptors are divided into three categories: geometric, boundary,
#' and topological. The default will compute the features from all categories.
#' We also note that, for the binary matrix, the ones and zeros make up the
#' region of interest and background, respectively.
#'
#' @param region a binary matrix that represents the region.
#' @param plg.chain an \eqn{n}-by-\eqn{2} numeric matrix that represents the boundary.
#' @param categories a character string or vector, specifying which features to compute.
#'
#' @return A numeric vector composed of the shape descriptors.
#'
#' @seealso
#' See \href{https://en.wikipedia.org/wiki/Polygonal_chain}{Polygonal Chain}
#' for more information on the closed polygonal chain.
#'
#' @export
#'
compute.features <- function(
    region, plg.chain,
    categories = c("geometric", "boundary", "topological")
)
{
    # empty object
    desc <- c()

    # shape features
    if ("geometric" %in% categories)
    {
        desc <- c(
            desc,
            geometric.features(region, plg.chain)
        )
    }

    # boundary features
    if ("boundary" %in% categories)
    {
        desc <- c(
            desc,
            boundary.features(plg.chain)
        )
    }

    # topological features
    if ("topological" %in% categories)
    {
        desc <- c(
            desc,
            topological.features(region)
        )
    }

    return(desc)
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Internal function to compute multiple regions at once ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#' Shape-Based Features for Segmented Objects
#'
#' @keywords internal features
#' @noRd
#'
compute.features.list <- function(regions, plg.chains, id, categories)
{
    # number of regions
    s <- max(regions)

    # allocate storage
    v <- vector("list", s)

    # compute individual features
    for (i in 1:s)
    {
        # objects
        region     <- (regions == i)
        plg.chain  <- plg.chains[[i]]

        # descriptors
        v[[i]] <- c(
            region.id = i,
            compute.features(region, plg.chain, categories = categories)
        )
    }

    # merge results
    data.frame(
        t(id),
        do.call(rbind, v)
    )
}
