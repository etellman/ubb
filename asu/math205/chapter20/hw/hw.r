
math205.dir <- '~/Documents/U/ubb/asu/math205'
# data.dir <- paste(math205.dir, 'data', 'bps', 'PC-Text', 'ch19', sep = '/')
# setwd(data.dir)

week.dir <- paste(math205.dir, 'chapter20', sep = '/')
figure.dir <- paste(week.dir, 'hw', 'figures', sep = '/')


# ex 17
younger <- list(n = 170, successes = 117)
older <- list(n = 317, successes = 152)

prop2.conf(younger, older)

# ex 18
not.tested <- list(n = 141, successes = 27)
tested <- list(n = 135, successes = 7)

prop2.conf(not.tested, tested, plus.four = T)

# ex 19
normal <- list(successes = 0, n = 18)
altered <- list(successes = 23, n = 33)

prop2.conf(altered, normal, plus.four = T, confidence.level = 0.99)

# ex20
prop2.z(not.tested, tested)

# ex21
without.help <- list(successes = 135, n = 190)
with.help <- list(successes = 293, n = 514)

prop2.z(without.help, with.help)
prop2.conf(without.help, with.help)

# ex 23
prop2.conf(without.help, with.help)

# ex 25
15/106
7/42

men <- list(successes = 15, n = 106)
women <- list(successes = 7, n = 42)

timesX <- function(x) { function(y) { y * x } }
men.large <- lapply(men, timesX(30))
women.large <- lapply(women, timesX(30))

z <- prop2.z(women, men)
prop2.conf(women.large, men.large)

2 * pnorm(-z)

.1308/.0239

sqrt(30)

# ex 26
urban <- list(successes = 52, n = 65)
rural <- list(successes = 30, n = 55)

prop2.z(urban, rural)

# ex 27
women <- list(successes = 23, n = 34)
men <- list(successes = 60, n = 89)

prop2.z(women, men)

# ex 28
prop2.conf(urban, rural, confidence.level = 0.9)

# ex 32
ny <- list(successes = 183, n = 220)
boston <- list(successes = 68, n = 117)

prop2.z(ny, boston)

13+35+18+31
48/97

# ex 35
impulse <- list(successes = 13, n = 13 + 18)
non.impulse <- list(successes = 35, n = 35 + 31)

prop2.conf(non.impulse, impulse)
prop2.z(non.impulse, impulse)
