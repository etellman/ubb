library(asbio) 

notes.dir <- paste(root.dir, "week16/notes", sep = "/")
setwd(notes.dir)

s <- 1.83 / sqrt(880)
z <- qnorm(1 - 0.025)

1.92 - s * 2

-qnorm((1 - .99)/2)

7.5/sqrt(654)

confidence <- function(p, sample.mean, s, n) {
  z <- -qnorm((1 - p)/2)
  s.z <- s / sqrt(n)
  margin.of.error <- s.z * z

  result <- data.frame(p = p, z = z, s.z = s.z, 
                       margin.of.error = margin.of.error,
                       min = sample.mean - margin.of.error, 
                       max = sample.mean + margin.of.error)

  round(result, 4)
}

sink('r.tex')
  xtable(t(sapply(c(0.9, 0.95, 0.99), function(p) confidence(p, 26.8, 7.5, 654))))
sink()

.05/pnorm(.2/sqrt(10))

?power.z.test
power.z.test(n = 10, sigma = 1, alpha = 0.05, effect = 0.8, test = "one.tail")

s = 1/sqrt(10)

qnorm(0.95)

zalpha <- qnorm(.05)
zalpha
.8/s

pnorm(zalpha + 0.8/s)

x <- qnorm(0.95) / 3.162
1 - pnorm((x - 0.8) * sqrt(10))
