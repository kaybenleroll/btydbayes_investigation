
#' These functions calculates the reparameterising the gamma distributions
#'
#' @export gamma_mucv2shaperate
#' @export gamma_shaperate2mucv
#' @export rgamma_mucv

gamma_mucv2shaperate <- function(mu, cv) {
  shape <- 1 / (cv^2)
  rate  <- 1 / (cv^2 * mu)

  return(c(shape = shape, rate = rate))
}


gamma_shaperate2mucv <- function(shape, rate) {
  mu <- shape / rate
  cv <- 1 / sqrt(shape)

  return(c(mu = mu, cv = cv))
}


rgamma_mucv <- function(n, mu, cv, ...) {
  params <- gamma_mucv2shaperate(mu, cv)

  rgamma(n = n, shape = params[1], rate = params[2], ...)
}


