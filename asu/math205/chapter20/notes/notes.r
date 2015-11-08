math205.dir <- '~/Documents/U/ubb/asu/math205'

week.dir <- paste(math205.dir, "chapter20", "notes", sep = "/")
setwd(week.dir)

set.seed(15)

prisoners <- c(rep('white', 453500),
               rep('black', 516900),
               rep('hispanic', 308700),
               rep('other', 123300))

us <- c(rep('white', 6190),
               rep('black', 1230),
               rep('hispanic', 1730),
               rep('other', 850))

drug.use <- c(rep('white', round(0.095 * 6190)),
               rep('black', round(0.105 * 1230)),
               rep('hispanic', round(0.088 * 1730)),
               rep('other', round(0.15 * 850)))

drug.offense <- c(rep('white', 67800),
               rep('black', 79900),
               rep('hispanic', 39900),
               rep('other', 20400))

set.seed(2)
prisoners.s <- sample(prisoners, 250)
us.s <- sample(us, 250)
drug.offense.s <- sample(drug.offense, 250)
drug.use.s <- sample(drug.use, 250)

percentages <- function(people) {
  mapply(function(x) x = length(grep(x, people)) / length(people), sort(unique(people)))
}

counts <- function(people) {
  mapply(function(x) x = length(grep(x, people)), sort(unique(people)))
}


counts(drug.use.s)

data <- read.csv("data.csv", strip.white = T)

data.m <- melt(data, id.vars = .(population))

to.sample <- function(data, data.set, race) {
  row <- subset(data, population == data.set & variable == race)
  list(n = 250, successes = row$value)
}


population.conf <- function(data, pop1, pop2, race) {
  sample1 <- to.sample(data, pop1, race)
  sample2 <- to.sample(data, pop2, race)

  prop2.conf(sample1, sample2)
}

population.z <- function(data, pop1, pop2, race) {
  sample1 <- to.sample(data, pop1, race)
  sample2 <- to.sample(data, pop2, race)

  prop2.z(sample1, sample2)
}

pop1 <- "us"
pop2 <- "drug.use"
race <- "black"

population.conf(data.m, pop1, pop2, race)
population.z(data.m, pop1, pop2, race)

set.seed(4)
white <- c(rep(1, round(0.095 * 10000)),
           rep(0, round((1 - 0.095) * 10000)))

black <- c(rep(1, round(0.105 * 10000)),
           rep(0, round((1 - 0.105) * 10000)))

n <- 250
white.s <- sample(white, n)
black.s <- sample(black, n)
sum(sample(white, n)) 
sum(sample(black, n)) 

sum(white.s)
sum(black.s)

sample.w <- list(n = 250, successes = sum(white.s))
sample.b <- list(n = 250, successes = sum(black.s))

prop2.conf(sample.b, sample.w)
prop2.z(sample.b, sample.w)
