
<!-- README.md is generated from README.Rmd. Please edit that file -->

# orthinto

<!-- badges: start -->

<!-- badges: end -->

The goal of orthinto is to explore integrals of orthogonal functions.

## Installation

You can install the development version of orthinto from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("mightymetrika/orthinto")
```

## Orthogonal Example

``` r
library(orthinto)

oio <- orthinto(phi = function(x) cos(x),
                psi = function(x) sin(x),
                dl = seq(-pi, 0, pi/10),
                du = seq(pi, 0, -pi/10))

summary(oio)
#> Orthogonal Integration Summary
#> =============================
#> Functions: function(x) cos(x) × function(x) sin(x) 
#> Number of intervals: 11 
#> 
#> Integration Values:
#>   Min: 0.000000 
#>   Max: 0.000000 
#>   Mean: 0.000000 
#>   Sum: 0.000000 
#> 
#> Absolute Errors:
#>   Min: 0.00e+00 
#>   Max: 2.21e-14 
#>   Mean: 1.10e-14 
#> 
#> Orthogonality Assessment:
#>   Total integral: 0.000000 
#>   Assessment: Functions appear to be orthogonal (integral ≈ 0)
plot(oio)
```

<img src="man/figures/README-orthogonal-1.png" width="100%" />

## Non-orthogonal Example

``` r

noio <- orthinto(phi = function(x) cos(x),
                psi = function(x) sin(x),
                dl = seq(-pi, 0, pi/10),
                du = seq(pi, 0, -pi/2))

summary(noio)
#> Orthogonal Integration Summary
#> =============================
#> Functions: function(x) cos(x) × function(x) sin(x) 
#> Number of intervals: 11 
#> 
#> Integration Values:
#>   Min: -0.500000 
#>   Max: 0.500000 
#>   Mean: -0.045455 
#>   Sum: -0.500000 
#> 
#> Absolute Errors:
#>   Min: 1.92e-15 
#>   Max: 1.36e-11 
#>   Mean: 1.25e-12 
#> 
#> Orthogonality Assessment:
#>   Total integral: -0.500000 
#>   Assessment: Functions do not appear to be orthogonal
plot(noio)
```

<img src="man/figures/README-not-orthogonal-1.png" width="100%" />
