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

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Main representations ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#' Polygonal chain
#'
#' @keywords internal shapes
#' @noRd
#'
polygonal.chain <- function(R, k = 1)
{
    # enlarge regions
    if (k > 1)
    {
        R <- image.enlarge(R, k = k)
    }

    # extract contour
    contr <- image.contour(R)[[1]]

    # find duplicate entries
    dups <- duplicated(contr)

    # validate chain
    if (sum(dups) > 0)
    {
        stop("a polygonal chain could not be created from the region")
    }

    create.chain(contr)
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Shape derivations ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#' Convex hull chain
#'
#' @keywords internal shapes
#' @noRd
#' @importFrom grDevices chull
#'
convex.hull <- function(P)
{
    # open chain
    P <- P[-nrow(P), ]

    # index of points that make up the convex hull
    index <- chull(P)

    # create the convex hull chain
    P.CH <- P[index, ]
    create.chain(P.CH)
}

#' Minimum bounding box chain
#'
#' @keywords internal shapes
#' @noRd
#'
minimum.bounding.box <- function(P.CH)
{
    # directions of edges
    e <- diff(P.CH)

    # lengths of edges
    lengths <- sqrt(rowSums(e^2))

    # unit edge directions
    v <- e / lengths

    # normal directions to edges
    w <- cbind(v[,2], -v[,1]) # original has -v[,2], v[,1]

    # find the MAR
    x <- apply(P.CH %*% t(v), 2, range) # extremes along edges
    y <- apply(P.CH %*% t(w), 2, range) # extremes normal to edges

    # compute possible areas
    areas <- (y[1, ] - y[2, ]) * (x[1, ] - x[2, ])

    # index of the edge with the smallest area
    k <- which.min(areas)

    # form bounding box from extremes of the chosen edge
    rect <- cbind(x[c(1, 2, 2, 1, 1), k], y[c(1, 1, 2, 2, 1), k])

    # orient the bounding box to the correct angle
    rect %*% rbind(v[k, ], w[k, ])
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Boundary representations ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#' Chain Codes
#'
#' @keywords internal shapes
#' @noRd
#'
chain.code <- function(P)
{
    # difference between consecutive points
    D <- diff(P)

    # angle between difference and x-axis
    theta <- apply(D, 1, arctan2)

    # modify angle
    theta <- ifelse(theta >= 0, theta, theta + 2*pi)

    # compute corresponding chain code
    round(theta / (pi / 4))
}

#' Curvature Chain Codes
#'
#' @keywords internal shapes
#' @noRd
#'
curvature.chain <- function(cc)
{
    # difference between consecutive chain codes
    k <- diff(cc)

    # modify negative entries
    k[k < 2] <- k[k < 2] + 8

    # modify positive entries
    k[k > 2] <- k[k > 2] - 8

    # end procedure
    k
}

#' Radial lengths
#'
#' @keywords internal shapes
#' @noRd
#'
radial.lengths <- function(P, normalize = TRUE)
{
    # shape's centroid
    ct <- centroid(P)

    # compute the radial lengths
    r <- sqrt(rowSums(sweep(P, 2, ct)^2))

    # normalize the lengths
    if (normalize)
    {
        r <- r / max(r)
    }

    # delete duplicate entry
    r[-length(r)]
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Internal functions ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#' Create a polygonal chain
#'
#' @keywords internal shapes
#' @noRd
#'
create.chain <- function(V)
{
    # modify columns
    colnames(V) <- c("X", "Y")

    # TODO: check that polygon is clockwise
    # TODO: find starting point (based on criteria)

    # close off chain
    rbind(V, V[1, ])
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Internal function to apply to multiple regions at once ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#' Polygonal Chain for Segments
#'
#' @keywords internal shapes
#' @noRd
#'
polygonal.chain.list <- function(seg, k = 1)
{
    # enlarge regions
    if (k > 1)
    {
        seg <- image.enlarge(seg, k = k)
    }

    # extract contours
    contr <- image.contour(seg)

    # find duplicate entries
    dups <- unlist(
        sapply(contr, duplicated)
    )

    # validate chain
    if (sum(dups) > 0)
    {
        stop("a polygonal chain could not be created from one of the regions")
    }

    # create polygonal chain
    lapply(contr, function(x)
        {
            # modify columns names
            colnames(x) <- c("X", "Y")

            # TODO: check that polygon is clockwise
            # TODO: find starting point (based on criteria)

            # create polygonal chain
            rbind(x, x[1, ])
        }
    )
}
