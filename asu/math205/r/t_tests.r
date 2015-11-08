# returns data frame containing values needed to do t-test analysis
t.summary <- function(x) {
  round(data.frame(s = sd(x), m = mean(x), n = length(x)), 4)
}

t.value <- function(mean, mu, s, n) {
  se <- s/sqrt(n)
  t.value.se(mean, mu, se)
}

t.value.se <- function(mean, mu, se) {
  round((mean - mu)/se, 4)
}

t.conf <- function(mean, t.star, s, n) {
  se <- s/sqrt(n)
  confidence.se(mean, t.star, se)
}

t.conf.se <- function(mean, t.star, se) {
  delta <- t.star * se
  round(c(mean = mean, delta = delta, min = mean - delta, max = mean + delta), 4)
}

s.2s <- function(x1, x2, digits = 4) {
  round(sqrt(with(x1, s^2/n) + with(x2, s^2/n)), digits)
}

t.2s <- function(x1, x2) {
  round((x1$mean - x2$mean)/s.2s(x1, x2, digits = 8), 4)
}

