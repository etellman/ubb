
root.dir <- '~/Documents/U/ubb/sccc/math146'
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
