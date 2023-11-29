#' My N Curve
#'
#' @param mu - mu
#' @param sigma - sigma
#' @param a - ad
#' @importFrom stats dnorm
#' @importFrom graphics polygon
#' @importFrom stats integrate
#'
#' @return The area (probability, P(X<=a))
#' @export
#'
#' @example
#' myncurve(mu=10, sigma=5, a=6)
myncurve <- function(mu, sigma, a){
  # Generate the x values for the curve
  x_values <- seq(mu - 3 * sigma, mu + 3 * sigma, length.out = 1000)

  # Calculate the corresponding y values for the curve
  y_values <- dnorm(x_values, mean = mu, sd = sigma)

  # Plot the curve
  plot(x_values, y_values, type = "l", lwd = 2, xlab = "x", ylab = "dnorm(x, mean=mu, sd=sigma)", xlim = c(mu - 3 * sigma, mu + 3 * sigma))

  # Shade the area under the curve from -∞ to a
  x_shaded <- seq(mu - 3 * sigma, a, length.out = 1000)
  y_shaded <- dnorm(x_shaded, mean = mu, sd = sigma)
  polygon(c(x_shaded, a, a), c(y_shaded, 0, 0), col = "lightblue")

  # Calculate the area (probability) P(X <= a)
  probability <- integrate(function(x) dnorm(x, mean = mu, sd = sigma), -Inf, a)$value

  # Return the probability value in a list
  result <- list(probability = probability)

  return(result)
}
