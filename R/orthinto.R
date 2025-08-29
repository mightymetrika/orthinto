#' Numerical Integration for Exploring Orthogonal Functions
#'
#' This function computes the integral of the product of two functions phi and psi
#' over specified intervals with the goal of exploring orthogonal functions. Two
#' functions are orthogonal over an interval if the integral of their product equals
#' zero over that interval. This is based on the definition from Chapter 1, Section 5
#' of Fourier Series by Georgi P. Tolstov.
#'
#' @param phi A function of one variable (default: cosine function)
#' @param psi A function of one variable (default: sine function)
#' @param dl Numeric vector of lower integration bounds (default: seq(-pi, 0, pi/10))
#' @param du Numeric vector of upper integration bounds (default: seq(pi, 0, -pi/10))
#' @param ... Additional arguments passed to \code{\link[stats]{integrate}}
#'
#' @return An object of class "oio" (Orthogonal Integrals are 0) containing:
#' \describe{
#'   \item{oio}{A list of integration results from \code{\link[stats]{integrate}}}
#'   \item{dl}{Vector of lower bounds used}
#'   \item{du}{Vector of upper bounds used}
#'   \item{phi_name}{Name of the phi function}
#'   \item{psi_name}{Name of the psi function}
#' }
#'
#' @details
#' The function computes integrals of the form:
#' \deqn{\int_{dl_i}^{du_i} \phi(x) \psi(x) dx}
#'
#' If dl and du have different lengths, they are recycled to match the longer vector.
#' The default functions (cosine and sine) are orthogonal over the interval -pi to pi.
#'
#' @examples
#' # Test orthogonality of cosine and sine (should be near zero)
#' result1 <- orthinto()
#' print(result1)
#'
#' # Test with custom functions
#' result2 <- orthinto(
#'   phi = function(x) x,
#'   psi = function(x) x^2,
#'   dl = -1,
#'   du = 1
#' )
#' summary(result2)
#'
#' @references
#' Tolstov, G. P. (1962). Fourier Series. Translated by R. A. Silverman. Dover Publications, Inc., New York
#'
#' @export
orthinto <- function(phi = function(x) cos(x),
                     psi = function(x) sin(x),
                     dl = seq(-pi, 0, pi/10),
                     du = seq(pi, 0, -pi/10),
                     ...) {

  # Input validation
  if (!is.function(phi)) {
    stop("'phi' must be a function")
  }

  if (!is.function(psi)) {
    stop("'psi' must be a function")
  }

  if (!is.numeric(dl)) {
    stop("'dl' must be numeric")
  }

  if (!is.numeric(du)) {
    stop("'du' must be numeric")
  }

  if (length(dl) == 0 || length(du) == 0) {
    stop("'dl' and 'du' must have length > 0")
  }

  # Test that functions work with a sample input
  tryCatch({
    phi(0)
    psi(0)
  }, error = function(e) {
    stop("Functions 'phi' and 'psi' must be able to evaluate numeric inputs: ", e$message)
  })

  # Handle recycling of dl and du
  max_length <- max(length(dl), length(du))
  dl <- rep_len(dl, max_length)
  du <- rep_len(du, max_length)

  # Check that lower bounds are less than or equal to upper bounds
  if (any(dl > du)) {
    warning("Some lower bounds are greater than upper bounds. This may produce unexpected results.")
  }

  # Set up function to integrate over
  .f <- function(x) {
    tryCatch({
      phi(x) * psi(x)
    }, error = function(e) {
      stop("Error evaluating phi(x) * psi(x): ", e$message)
    })
  }

  # Perform integration
  oio <- vector(mode = "list", length = max_length)

  for (i in 1:max_length) {
    tryCatch({
      oio[[i]] <- stats::integrate(f = .f, lower = dl[i], upper = du[i], ...)
    }, error = function(e) {
      warning(paste("Integration failed for interval", i, ":", e$message))
      oio[[i]] <- list(value = NA, abs.error = NA, message = e$message)
      class(oio[[i]]) <- "integrate"
    })
  }

  # Get function names for display
  phi_name <- deparse(substitute(phi))
  psi_name <- deparse(substitute(psi))

  # Build return object of class "oio"
  res <- list(
    oio = oio,
    dl = dl,
    du = du,
    phi_name = phi_name,
    psi_name = psi_name
  )
  class(res) <- "oio"
  return(res)
}

#' Print Method for oio Objects
#'
#' @param x An object of class "oio"
#' @param ... Additional arguments (currently unused)
#' @export
print.oio <- function(x, ...) {
  cat("Orthogonal Integration Object (oio)\n")
  cat("===================================\n")
  cat("Functions:", x$phi_name, "and", x$psi_name, "\n")
  cat("Number of intervals:", length(x$oio), "\n")

  # Show first few integration results
  n_show <- min(3, length(x$oio))
  cat("\nIntegration results (first", n_show, "intervals):\n")

  for (i in 1:n_show) {
    result <- x$oio[[i]]
    if (is.na(result$value)) {
      cat(sprintf("  [%d] Interval [%.3f, %.3f]: Failed - %s\n",
                  i, x$dl[i], x$du[i], result$message))
    } else {
      cat(sprintf("  [%d] Interval [%.3f, %.3f]: %.6f (±%.2e)\n",
                  i, x$dl[i], x$du[i], result$value, result$abs.error))
    }
  }

  if (length(x$oio) > n_show) {
    cat("  ... and", length(x$oio) - n_show, "more intervals\n")
  }

  invisible(x)
}

#' Summary Method for oio Objects
#'
#' @param object An object of class "oio"
#' @param ... Additional arguments (currently unused)
#' @export
summary.oio <- function(object, ...) {
  cat("Orthogonal Integration Summary\n")
  cat("=============================\n")
  cat("Functions:", object$phi_name, "×", object$psi_name, "\n")
  cat("Number of intervals:", length(object$oio), "\n\n")

  # Extract values and errors
  values <- sapply(object$oio, function(x) x$value)
  errors <- sapply(object$oio, function(x) x$abs.error)

  # Remove NA values for summary statistics
  valid_values <- values[!is.na(values)]
  valid_errors <- errors[!is.na(errors)]

  if (length(valid_values) > 0) {
    cat("Integration Values:\n")
    cat("  Min:", sprintf("%.6f", min(valid_values)), "\n")
    cat("  Max:", sprintf("%.6f", max(valid_values)), "\n")
    cat("  Mean:", sprintf("%.6f", mean(valid_values)), "\n")
    cat("  Sum:", sprintf("%.6f", sum(valid_values)), "\n")

    cat("\nAbsolute Errors:\n")
    cat("  Min:", sprintf("%.2e", min(valid_errors)), "\n")
    cat("  Max:", sprintf("%.2e", max(valid_errors)), "\n")
    cat("  Mean:", sprintf("%.2e", mean(valid_errors)), "\n")
  }

  # Count failed integrations
  n_failed <- sum(is.na(values))
  if (n_failed > 0) {
    cat("\nFailed integrations:", n_failed, "\n")
  }

  # Orthogonality assessment
  if (length(valid_values) > 0) {
    total_integral <- sum(valid_values)
    cat("\nOrthogonality Assessment:\n")
    cat("  Total integral:", sprintf("%.6f", total_integral), "\n")

    # Simple heuristic for orthogonality
    if (abs(total_integral) < 1e-6) {
      cat("  Assessment: Functions appear to be orthogonal (integral ≈ 0)\n")
    } else if (abs(total_integral) < 1e-3) {
      cat("  Assessment: Functions are approximately orthogonal\n")
    } else {
      cat("  Assessment: Functions do not appear to be orthogonal\n")
    }
  }

  invisible(object)
}


#' Plot Method for oio Objects
#'
#' Provides visualization options for orthogonal integration results with enhanced
#' interval visualization to better understand the integration bounds.
#'
#' @param x An object of class "oio"
#' @param type Character string specifying plot type:
#' \describe{
#'   \item{"intervals"}{(default) Bar plot of integral values by interval number}
#'   \item{"bounds"}{Horizontal segment plot showing actual interval ranges}
#' }
#' @param show_bounds Logical; if TRUE, show actual interval bounds as x-axis labels
#'   for "intervals" type. Default is FALSE.
#' @param max_labels Integer; maximum number of interval labels to show on x-axis
#'   when show_bounds=TRUE to avoid overcrowding (default: 10)
#' @param legend_pos Character; position for legend. One of "topright", "topleft",
#'   "bottomright", "bottomleft", "top", "bottom", "left", "right", or "none" to
#'   suppress legend. Default is "bottomright".
#' @param ... Additional arguments passed to underlying plot functions
#'
#' @details
#' The "bounds" plot type shows each interval as a horizontal line segment, with
#' colors indicating the integration result:
#' \itemize{
#'   \item Blue: ≈ 0 (suggesting orthogonality)
#'   \item Green: > 0 (positive integral)
#'   \item Orange: < 0 (negative integral)
#'   \item Red: Failed integration
#' }
#'
#' @examples
#' # Create test data
#' result <- orthinto(dl = c(-1, 0, 1), du = c(0, 1, 2))
#'
#' # Standard interval plot
#' plot(result, type = "intervals")
#'
#' # Show actual bounds on x-axis
#' plot(result, type = "intervals", show_bounds = TRUE)
#'
#' # Bounds visualization
#' plot(result, type = "bounds")
#'
#' # Adjust legend position
#' plot(result, type = "bounds", legend_pos = "bottomright")
#'
#' @export
plot.oio <- function(x, type = "intervals", show_bounds = FALSE, max_labels = 10,
                     legend_pos = "bottomright", ...) {

  type <- match.arg(type, c("intervals", "bounds"))

  values <- sapply(x$oio, function(int) int$value)

  # Create color coding based on integration values
  valid_values <- values[!is.na(values)]
  if (length(valid_values) > 0) {
    colors <- ifelse(is.na(values), "red",
                     ifelse(abs(values) < 1e-6, "blue",
                            ifelse(values > 0, "green", "orange")))
  } else {
    colors <- rep("red", length(values))
  }

  if (type == "intervals") {
    # Plot integration intervals and their values
    n_intervals <- length(x$oio)

    # Create the plot
    graphics::plot(1:n_intervals, values,
                   type = "h",
                   col = colors,
                   main = paste("Integration Results:", x$phi_name, "×", x$psi_name),
                   xlab = if(show_bounds) "Integration Intervals" else "Interval Number",
                   ylab = "Integral Value",
                   xaxt = if(show_bounds) "n" else "s",
                   ...)

    graphics::abline(h = 0, lty = 2, col = "gray")

    # Add custom x-axis labels showing interval bounds
    if (show_bounds && n_intervals <= max_labels) {
      interval_labels <- sprintf("[%.2f,%.2f]", x$dl, x$du)
      graphics::axis(1, at = 1:n_intervals, labels = interval_labels,
                     las = 2, cex.axis = 0.8)
    } else if (show_bounds && n_intervals > max_labels) {
      label_indices <- round(seq(1, n_intervals, length.out = max_labels))
      interval_labels <- sprintf("[%.2f,%.2f]", x$dl[label_indices], x$du[label_indices])
      graphics::axis(1, at = label_indices, labels = interval_labels,
                     las = 2, cex.axis = 0.8)
    }

    # Add legend if requested
    if (legend_pos != "none") {
      graphics::legend(legend_pos,
                       legend = c("~ 0 (orthogonal)", "> 0", "< 0", "Failed"),
                       col = c("blue", "green", "orange", "red"),
                       lty = 1,
                       cex = 0.8,
                       bg = "white")
    }

  } else if (type == "bounds") {
    # Visualize the actual intervals as horizontal segments
    n_intervals <- length(x$oio)

    # Set up plot limits with some padding
    all_bounds <- c(x$dl, x$du)
    x_range <- range(all_bounds)
    x_padding <- diff(x_range) * 0.05
    xlim <- c(x_range[1] - x_padding, x_range[2] + x_padding)

    # Create empty plot with cleaner y-axis
    graphics::plot(xlim, c(0.5, n_intervals + 0.5), type = "n",
                   main = paste("Integration Intervals:", x$phi_name, "×", x$psi_name),
                   xlab = "x",
                   ylab = "",
                   ylim = c(n_intervals + 0.5, 0.5),  # Reverse so interval 1 is at top
                   yaxt = "n",  # Suppress default y-axis
                   ...)

    # Add clean y-axis labels
    graphics::axis(2, at = 1:n_intervals, labels = paste("Interval", 1:n_intervals),
                   las = 1, cex.axis = 0.8)

    # Draw each interval as a horizontal line segment
    for (i in 1:n_intervals) {
      graphics::segments(x$dl[i], i, x$du[i], i, col = colors[i], lwd = 3)

      # Add interval bounds as text labels (only if not too many intervals)
      if (n_intervals <= 15) {
        # Left bound
        graphics::text(x$dl[i], i + 0.3, sprintf("%.2f", x$dl[i]),
                       cex = 0.7, adj = c(0.5, 0), col = "darkgray")
        # Right bound
        graphics::text(x$du[i], i + 0.3, sprintf("%.2f", x$du[i]),
                       cex = 0.7, adj = c(0.5, 0), col = "darkgray")
      }

      # Add integral value as text (only if not too many intervals)
      if (n_intervals <= 12) {
        mid_point <- (x$dl[i] + x$du[i]) / 2
        if (!is.na(values[i])) {
          graphics::text(mid_point, i - 0.35, sprintf("%.3f", values[i]),
                         cex = 0.8, adj = c(0.5, 1), col = colors[i], font = 2)
        } else {
          graphics::text(mid_point, i - 0.35, "FAIL",
                         cex = 0.8, adj = c(0.5, 1), col = "red", font = 2)
        }
      }
    }

    # Add vertical line at x = 0 for reference
    graphics::abline(v = 0, lty = 2, col = "gray50")

    # Add legend if requested
    if (legend_pos != "none") {
      graphics::legend(legend_pos,
                       legend = c("~ 0 (orthogonal)", "> 0", "< 0", "Failed"),
                       col = c("blue", "green", "orange", "red"),
                       lty = 1, lwd = 3,
                       cex = 0.8,
                       bg = "white")
    }
  }
}
