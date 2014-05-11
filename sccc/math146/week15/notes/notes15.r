root.dir <- '~/Documents/U/ubb/sccc/math146'
notes.dir <- paste(root.dir, "week13/notes", sep = "/")
setwd(notes.dir)

10 * .1^2 * .9^3

choose(10, 2) * 1/6^2 * (5/6)^8

(5/6)^10

choose(100, 9) * 0.1^9 * 0.9^91

1 - 0.31 - 0.19

.5^9

pnorm(1) - pnorm(-1)
pnorm(-1)
pnorm(1)

s <- sqrt(90)
1 - (pbinom(103.5, 1000, 0.1) - pbinom(96.5, 1000, 0.1))

pbinom(15, 25, 0.6)

sqrt(0.6 * 25 * 0.4)

1 - pnorm(14.5, 15, 2.449)

sum(dbinom(97:103, 1000, .1))

pbinom(103.5, 1000, 0.1) - pbinom(96.5, 1000, 0.1)
pnorm(1) - pnorm(-1)

choose(1000, 100) * 0.1^100 * .9^900

s <- sqrt(100 * .9)
s

3/s
pnorm(3/s)
pnorm(3.5/s) - pnorm(-3.5/s)

sum(dbinom(90:110, 1000, 0.1))

m <- 90 * 0.477
s <- sqrt(90 * 0.477 * (1 - 0.477))

z <- (29.5 - m)/s

pnorm(z)

pbinom(29, 90, 0.477)

qbinom(5.1, 10, .5)

1 - pnorm((1520 - 1500)/24.49)

# ex 11
m <- 0.27 * 1535
print(m)
s <- sqrt(m * (1 - .27))
s

1 - pbinom(415, 1535, 0.27)

z <- (416 - m)/s
1 - pnorm(z)

# ex 12
m <- 0.12 * 1500
m
s <- sqrt(m * (1 - .12))
s

z1 <- (c(165,195) - m)/s
z1

pnorm(z1[[2]]) - pnorm(z1[[1]])

z <- (416 - m)/s
1 - pnorm(z)

.12 * 1500

1 - sum(dbinom(0:3, 15, 0.2))

choose(10, 0:3) * .7^7 * .3^3

# ex 8
sqrt(15 * 0.7 * 0.3)
