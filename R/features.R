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
# Shape features categories ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

geometric.features <- function(R, P)
{
    # shape representations
    P.CH  <- convex.hull(P)
    P.MBB <- minimum.bounding.box(P.CH)

    # compute descriptors
    c(
        region.based(R),
        polygonal.based(P),
        convex.hull.based(P, P.CH),
        bounding.box.based(P, P.MBB)
    )
}

boundary.features <- function(P)
{
    # shape representations
    cc <- chain.code(P)
    k  <- curvature.chain(cc)
    r  <- radial.lengths(P)

    # compute descriptors
    c(
        curvature.chain.based(k),
        radial.lengths.based(r)
    )
}

topological.features <- function(R)
{
    # quantities
    f.H <- image.fill(R)
    H   <- f.H - R
    f.S <- image.segment(H)
    B   <- matrix(1, 3, 3)

    # de-noise region
    R.hat <- image.opening(
        image.erode(f.H, B), B
    )

    # compute descriptors
    c(
        number.holes       = max(f.S),
        number.protrusions = ifelse(is.empty(R.hat), 0, sum(f.H - R.hat))
    )
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Geometric and region features ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

region.based <- function(R)
{
    # quantities
    A <- sum(R)
    d <- thickness(R)

    # compute descriptors
    c(
        net.area   = A,
        thickness  = d,
        elongation = A / (2 * d^2)
    )
}

polygonal.based <- function(P)
{
    # quantities
    A   <- gauss.area(P)
    Per <- perimeter(P)

    # compute descriptors
    c(
        area.filled = A,
        perimeter   = Per,
        circularity = (4*pi) * A / Per^2
    )
}

convex.hull.based <- function(P, P.CH)
{
    # quantities
    Area.Filled <- gauss.area(P)
    Area        <- gauss.area(P.CH)
    Per         <- perimeter(P.CH)

    # compute descriptors
    c(
        convex.area = Area,
        convex.perimeter = Per,
        roundness = (4*pi) * Area.Filled / Per^2,
        convexity = Per / perimeter(P),
        solidity = Area.Filled / Area
    )
}

bounding.box.based <- function(P, P.MBB)
{
    # initial values
    b <- diff(P.MBB)        # differences
    l <- sqrt(rowSums(b^2)) # lengths
    k <- which.max(l)       # index of max length

    # remove names
    dimnames(b) <- NULL

    # quantities
    l.max <- max(l)
    l.min <- min(l)

    # fibre measurements
    l <- fibre.length(P)
    w <- fibre.width(P)

    # compute descriptors
    c(
        major.axis.length = l.max,
        major.axis.angle  = atan2(b[k, 2], b[k, 1]), # subtract by pi
        minor.axis.length = l.min,
        bounding.box.area = l.max * l.min,
        eccentricity      = l.min / l.max,
        fibre.length      = l,
        fibre.width       = w,
        curl              = l.max / l
    )
}

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Boundary features ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

curvature.chain.based <- function(k)
{
    c(
        bending.energy      = mean(k^2),
        total.abs.curvature = mean(abs(k))
    )
}

radial.lengths.based <- function(r)
{
    # quantities
    n     <- length(r)
    m     <- mean(r)
    sigma <- stdv(r)

    # boolean values
    below <- r <= m
    above <- r >= m

    # measurements
    Z  <- sum(below  & c(FALSE, above[-n])) + sum(above  & c(FALSE, below[-n]))
    Nm <- (mean((r - m)^4)^(1/4) - mean((r - m)^2)^(1/2)) / m

    # compute descriptors
    c(
        radial.mean       = m,
        radial.sd         = sigma,
        entropy           = radial.entropy(r),
        area.ratio        = (1 / (n*m)) * sum(r[above] - m),
        zero.crossing     = Z,
        normalized.moment = Nm
    )
}
