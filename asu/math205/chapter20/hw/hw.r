
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

(135+293)/(190+514)
prop2.z(without.help, with.help)

# ex 23
prop2.conf(without.help, with.help)
