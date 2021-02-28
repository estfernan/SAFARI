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
# Algorithms ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#' Centroid Formula for Polygons
#'
#' @references https://en.wikipedia.org/wiki/Centroid
#'
#' @keywords internal algorithms
#' @noRd
#'
centroid <- function(P)
{
    # number of boundary points
    n <- nrow(P) - 1

    # compute area of shape
    A <- gauss.area(P, signed = TRUE)

    # initialize centroid
    cent <- c(x = 0, y = 0)

    # compute the shape's centroid
    for (i in 1:n)
    {
        # multiplication factor
        s <- P[i, 1] * P[i + 1, 2] - P[i + 1, 1] * P[i, 2]

        # update the x-coordinate
        cent["x"] <- cent["x"] + (P[i, 1] + P[i + 1, 1]) * s

        # update the y-coordinate
        cent["y"] <- cent["y"] + (P[i, 2] + P[i + 1, 2]) * s
    }

    # divide by factor
    cent / (6 * A)
}

#' Fibre Length
#'
#' @keywords internal algorithms
#' @noRd
#'
fibre.length <- function(P) {
    # quantities for shape
    A   <- gauss.area(P)
    Per <- perimeter(P)

    # discriminant
    D <- Per^2 - 16 * A

    # check for invalid computation
    if (D < 0)
    {
        return(NA)
    }

    # compute fibre length
    (Per - sqrt(D)) / 4
}

#' Fibre Width
#'
#' @keywords internal algorithms
#' @noRd
#'
fibre.width <- function(P)
{
    gauss.area(P) / fibre.length(P)
}

#' Gauss's Area Formula for Polygons
#'
#' @references https://en.wikipedia.org/wiki/Shoelace_formula
#'
#' @keywords internal algorithms
#' @noRd
#'
gauss.area <- function(P, signed = FALSE)
{
    # initial values
    n <- nrow(P) - 1    # number of points
    A <- 0              # counter
    dimnames(P) <- NULL # remove names

    # shoelace formula
    for (i in 1:n)
    {
        A <- A + (
            P[i, 1] * P[i + 1, 2] - P[i + 1, 1] * P[i, 2]
        )
    }

    # obtain un-signed area
    if (!signed)
    {
        A <- abs(A)
    }

    # apply constant
    (1/2) * A
}

#' Perimeter
#'
#' @keywords internal algorithms
#' @noRd
#'
perimeter <- function(P)
{
    sum(sqrt(rowSums(diff(P)^2)))
}

#' Radial Entropy
#'
#' @keywords internal algorithms
#' @noRd
#' @importFrom graphics hist
#'
radial.entropy <- function(r, delta = 0.01)
{
    p.k   <- hist(r, breaks = seq(0, 1, by = delta), plot = FALSE)$density / 100
    -sum(p.k * log(p.k), na.rm = TRUE)
}

#' Population Standard Deviation
#'
#' @keywords internal algorithms
#' @noRd
#'
stdv <- function(x, na.rm = FALSE)
{
    x.hat <- mean(x)
    sqrt(mean((x - x.hat)^2))
}

#' Maximum Region Thickness
#'
#' @keywords internal algorithms
#' @noRd
#'
thickness <- function(R)
{
    # initial values
    k <- matrix(1, 3, 3) # kernel brush
    R <- image.fill(R)   # fill holes
    d <- 0               # thickness variable
    A <- sum(R)          # initial area

    # compute thickness
    while (sum(R) > 0)
    {
        R <- image.erode(R, k) # erode image
        d <- d + 1                # increase thickness

        # prevent infinite loop
        if (sum(R) == A)
        {
            return(NA)
        }
    }

    return(d)
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Basic Operations ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#' Four-Quadrant Inverse Tangent
#'
#' @keywords internal operations
#' @noRd
#'
arctan2 <- function(v)
{
    atan2(v[2], v[1])
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Object validation ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#' Binary Image
#'
#' @keywords internal validation
#' @noRd
#'
is.binary <- function(img)
{
    length(unique(c(img))) <= 2
}

#' Empty Image
#'
#' @keywords internal validation
#' @noRd
#'
is.empty <- function(x)
{
    sum(x) <= 0
}

#' Unnamed object
#'
#' @keywords internal validation
#' @noRd
#'
is.named <- function(x)
{
    !is.null(names(x))
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Utilities ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#' Re-map Values in Matrix
#'
#' @keywords internal utilities
#' @noRd
#'
re.map <- function(x, old, new)
{
    matrix(new[match(x, old)], nrow = nrow(x), ncol = ncol(x))
}
