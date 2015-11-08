math205.dir <- '~/Documents/U/ubb/asu/math205'
data.dir <- paste(math205.dir, 'data', 'bps', 'PC-Text', 'ch18', sep = '/')
setwd(data.dir)

set.seed(15)
x = c(1,1,1,1,0,0,0,0)
y = c(-1,-1,-1,-1,0,0,0,0)

sd(1:10 - 10:1)

sd(1:10) * 2

set.seed(15)
sd(x[shuffle(10)] - x[shuffle(10)])

sqrt(2 * sd(x)^2)

library(permute)
install.packages("permute")
shuffle(x)
sd(x - 10:1)

sd(x)
t.test(x, y)

sd(x) + sd(y)

y <- y[order(-y)]

mean(x - y)

mean(x)
mean(y)

sd(x - y[order(y)])

# ex 18.5
ex05 <- read.delim('ex18-05.dat', strip.white = T, header = T)

ex05.t <- ddply(ex05, .(Group), summarize, 
                n = length(Trees), mean = mean(Trees), s = sd(Trees))

tex.table(ex05.t)

t.2s(subset(ex05.t, Group == 'U'), subset(ex05.t, Group == 'L'))
s <- s.2s(subset(ex05.t, Group == 'U'), subset(ex05.t, Group == 'L'))

# ex 18.6
ex06 <- read.delim('ta18-01.dat', strip.white = T, header = T)

ex06.t <- ddply(ex06, .(Group), summarize, 
                n = length(Lie), mean = mean(Lie), s = sd(Lie))

ex06.t
tex.table(ex06.t)

t.2s(subset(ex06.t, Group == 'lean'), subset(ex06.t, Group == 'obese'))
s <- s.2s(subset(ex06.t, Group == 'lean'), subset(ex06.t, Group == 'obese'))
s

# ex 18.9
ex09 <- read.delim('ta18-02.dat', strip.white = T, header = T)

ex09
ex09.m <- melt(ex09)

ex09.m <- cbind(ex09.m, lavender = grepl("Lav", ex09.m$variable))
ex09.m <- cbind(ex09.m, time = grepl("Min", ex09.m$variable))
ex09.m

ex09.time <- ddply(subset(ex09.m, time), 
                   .(lavender), summarize, 
                   n = length(value), mean = mean(value), s = sd(value))

ex09.money <- ddply(subset(ex09.m, !time), 
                   .(lavender), summarize, 
                   n = length(value), mean = mean(value), s = sd(value))

ex09.both <- ddply(ex09.m, time ~ lavender, summarize, 
                   n = length(value), mean = mean(value), s = sd(value))
ex09.time
ex09.money
ex09.both

tex.table(ex09.both)

t.2s(subset(ex09.time, lavender), subset(ex09.time, !lavender))
s.2s(subset(ex09.time, lavender), subset(ex09.time, !lavender))

t.2s(subset(ex09.money, lavender), subset(ex09.money, !lavender))
s.2s(subset(ex09.money, lavender), subset(ex09.money, !lavender))


