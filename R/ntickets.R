#' NTickets Function
#' This method calculates the number of tickets to be sold
#' using discrete distribution and normal approximation.
#'
#' @param N - number of seats available on the plane
#' @param gamma - probability the plane will be overbooked
#' @param p - probability that a given person will show up
#' @importFrom stats qbinom
#' @importFrom stats qnorm
#' @importFrom stats pbinom
#' @importFrom stats pnorm
#' @importFrom graphics abline
#'
#' @return
#' @export
#'
#' @examples
#' ntickets(400, 0.02, 0.95)
ntickets <- function(N, gamma, p) {
  # Calculate nd using the binomial distribution (discrete)
  nd <- qbinom(1-gamma, N:N+10, p)

  # Calculate nc using the normal approximation
  mean <- nd * p
  sd <- sqrt(nd * p * (1 - p))
  nc <- qnorm(1 - gamma, mean, sd, lower.tail = FALSE)

  # Then we solve for n in the equation N = n * p + z * sqrt(n * p * (1 - p))
  z <- qnorm(gamma, lower.tail = FALSE)
  nc <- (N - z * sqrt(N * p * (1 - p))) / (p - (z^2 * p * (1 - p) / N))

  # Plotting objective function vs n for discrete distribution
  n_seq <- seq(N, N + 20, by = 1) # Adjust the sequence as needed
  objective_discrete <- sapply(n_seq, function(n) pbinom(N, size = n, prob = p, lower.tail = FALSE))

  # Plotting objective function vs n for normal approximation
  objective_continuous <- pnorm(N, mean = n_seq * p, sd = sqrt(n_seq * p * (1 - p)), lower.tail = FALSE)

  plot(n_seq, objective_discrete, type = "l", col = "red", xlab = "Number of tickets (n)", ylab = "Objective Discrete Function")
  abline(h = gamma, col = "green", lty = 2)

  plot(n_seq, objective_continuous, type="l", col = "blue", xlab = "Number of tickets (n)", ylab = "Objective Continuous Function")
  abline(h = gamma, col = "green", lty = 2)

  # Returning a named list containing nd, nc, N, p, and gamma
  return(list(nd = nd, nc = nc))
}
