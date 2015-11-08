
math205.dir <- '~/Documents/U/ubb/asu/math205'
# data.dir <- paste(math205.dir, 'data', 'bps', 'PC-Text', 'ch19', sep = '/')
# setwd(data.dir)

week.dir <- paste(math205.dir, 'chapter19', sep = '/')
figure.dir <- paste(week.dir, 'hw', 'figures', sep = '/')

prop.sd <- function(n, successes) {
  success.probability <- successes / n
  failure.probability <- 1 - success.probability

  sqrt(success.probability * failure.probability / n)
}

plus4.sd <- function(n, successes) {
  prop.sd(n + 4, successes + 4)
}

prop.n <- function(expected.successes, margin.of.error, confidence.level) {
  z <- z.star(confidence.level)
  n <- z / margin.of.error^2 * expected.successes * (1 - expected.successes)

  ceiling(n)
}

prop.z <- function(n, successes, p0) {
  p <- successes / n
  z <- (p - p0) / prop.sd(n, successes)

  c(p = p, z = z)
}

prop.conf <- function(n, successes, confidence.level) {
  p <- successes / n
  z <- z.star(confidence.level)
  delta <- z * prop.sd(n, successes)

  round(c(p = p, delta = delta, min = p - delta, max = p + delta), 4)
}

plus4.conf <- function(n, successes, confidence.level) {
  prop.conf(n + 4, successes + 2, confidence.level)
}

z.star <- function(confidence.level) {
  round(qnorm(1 - (1 - confidence.level)/2), 3)
}

# ex 25
plus4.conf(1010, 848, 1.96)

# ex 26
plus4.conf(172, 19, 1.96)

# ex 27
prop.conf(1010, 848, 1.96)
1.96 * prop.s(1010, 1010/2)

# ex 28
plus4.conf(127, 107, z.star(0.99)) - prop.conf(127, 107, z.star(0.99)) 
1.96 * prop.s(1010, 1010/2)

# ex 30
plus4.conf(25, 17, 0.95)
plus4.conf(25, 20, 0.95)

z.star(0.90)

# ex 31
plus4.conf(23, 18, 0.90)

# ex 34
z.star(0.99)
184/1832
prop.n(0.2, 0.015, 0.99)
prop.conf(1832, 184, 0.99)

# ex 36
prop.conf(14941, 10010, 0.99)

# ex 37
z.star(0.9)
plus4.conf(12, 5, 0.90)

prop.conf(12, 7, 0.9)

#ex 38
z <- prop.z(803, 304, 0.3333)
z

1 - pnorm(z)

# ex 39
prop.conf(1484, 594, 0.95)

# ex 40
plus4.conf(117, 68, 0.95)

# ex 41
prop.z(1484, 594, 0.5)

# ex 42
prop.z(117, 68, 0.5)

1 - pnorm(1.7802)

# ex 43
plus4.conf(97, 7, 0.95)

# ex 44
prop.n(0.5785, 0.05, 0.95)
