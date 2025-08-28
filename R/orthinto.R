orthinto <- function(phi = function(x)cos(x),
                     psi = function(x)sin(x),
                     dl = seq(-pi, 0, pi/10),
                     du = seq(pi, 0, -pi/10),
                     ...){

  # Set up function to integrate over
  .f <- function(x)phi(x)*psi(x)

  # Perform integration
  dlength <- max(length(dl), length(du))
  ioi <- vector(mode = "list", length = dlength)
  for (i in 1:dlength){
    ioi <- stats::integrate(f = .f, lower = dl[i], upper = du[i], ...)
  }

  # Build return object of class "ioi"
  res <- list(ioi = ioi, dl = dl, du = du)
  class(res) <- "ioi"
  return(res)
}
