
# calculates the proportion standard error
prop.se <- function(n, successes) {
  success.probability <- successes / n
  failure.probability <- 1 - success.probability

  sqrt(success.probability * failure.probability / n)
}

# the "plus 4" standard deviation
plus4.sd <- function(n, successes) {
  prop.sd(n + 4, successes + 2)
}

# the number of samples required for a desired margin of error
prop.n <- function(expected.successes, margin.of.error, confidence.level) {
  z <- z.star(confidence.level)
  n <- z / margin.of.error^2 * expected.successes * (1 - expected.successes)

  ceiling(n)
}

# z-statistic for a specified sample size, number of successes and null hypothesis
prop.z <- function(n, successes, p0) {
  p <- successes / n
  z <- (p - p0) / prop.sd(n, successes)

  c(p = p, z = z)
}

# confidence interval
prop.conf <- function(n, successes, confidence.level) {
  p <- successes / n
  z <- z.star(confidence.level)
  delta <- z * prop.sd(n, successes)

  round(c(p = p, delta = delta, min = p - delta, max = p + delta), 4)
}

# "plus four" confidence interval
plus4.conf <- function(n, successes, confidence.level) {
  prop.conf(n + 4, successes + 2, confidence.level)
}

# z-star to avoid having to look it up in Table A
z.star <- function(confidence.level) {
  round(qnorm(1 - (1 - confidence.level) / 2), 3)
}

# confidence interval
prop2.conf <- function(sample1, sample2, confidence.level = 0.95, plus.four = F) {

  if (plus.four) {
    sample1.use <- with(sample1, list(n = n + 2, successes = successes + 1))
    sample2.use <- with(sample2, list(n = n + 2, successes = successes + 1))
  } else {
    sample1.use <- sample1
    sample2.use <- sample2
  }

  prop2.conf.private(sample1.use, sample2.use, confidence.level)
}

# confidence interval
prop2.conf.private <- function(sample1, sample2, confidence.level = 0.95) {

  p <- prop2.diff(sample1, sample2)

  # delta
  se1 <- with(sample1, prop.se(n, successes))
  se2 <- with(sample2, prop.se(n, successes))
  delta <- z.star(confidence.level) * sqrt(se1^2 + se2^2)

  round(c(p = p, delta = delta, min = p - delta, max = p + delta), 4)
}

# computes the difference between two proportions
prop2.diff.private <- function(sample1, sample2) {
  p1 <- with(sample1, successes / n)
  p2 <- with(sample2, successes / n)

  p1 - p2
}

# significance 
prop2.z <- function(sample1, sample2, confidence.level = 0.95) {

  p <- abs(sample1$successes + sample2$successes) / (sample1$n + sample2$n)
  se <- sqrt(p * (1 - p) * (1/sample1$n + 1/sample2$n))

  diff <- prop2.diff.private(sample1, sample2)

  round(diff/se, 4)
}

