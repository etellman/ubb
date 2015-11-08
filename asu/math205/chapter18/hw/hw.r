
math205.dir <- '~/Documents/U/ubb/asu/math205'
data.dir <- paste(math205.dir, 'data', 'bps', 'PC-Text', 'ch18', sep = '/')
setwd(data.dir)

week.dir <- paste(math205.dir, 'chapter18', sep = '/')
figure.dir <- paste(week.dir, 'hw', 'figures', sep = '/')

# ex 25

women1 <- data.frame(n = 56, mean = 16177, s = 7520)
men1 <- data.frame(n = 56, mean = 16569, s = 9108)

women2 <- data.frame(n = 27, mean = 16496, s = 7914)
men2 <- data.frame(n = 20,   mean = 12867, s = 8343)

ex25.t1 <- t.value.2s(women1, men1)
ex25.t2 <- t.value.2s(women2, men2)

1 - pt(ex25.t1, 55)
1 - pt(ex25.t2, 19)

# ex 26
10 * sqrt(11)

unrestrained <- data.frame(n = 9, mean = 59, s = 21)
restrained <- data.frame(n = 11, mean = 32, s = 33.16625)

delta <- 1.860 * s.2s(unrestrained, restrained)

27 - delta
27 + delta

# ex 27
round(1.56 * sqrt(6), 4)
round(2.68 * sqrt(7), 4)

or <- data.frame(n = 6, mean = 26.9, s = 3.8212)
ca <- data.frame(n = 7, mean = 11.9, s = 7.0906)

t.value.2s(or, ca)

# ex 28
montessori <- data.frame(n = 30, mean = 19, s = 3.11)
control <- data.frame(n = 25, mean = 17, s = 4.19)

t.value.2s(montessori, control)

# ex 29
ginkgo <- data.frame(n = 21, mean = 0.06383, s = 0.01462)
control <- data.frame(n = 18, mean = 0.05342, s = 0.01549)

t.value.2s(ginkgo, control)

# ex 31
asian <- data.frame(n = 12, mean = 1.92, s = 0.6)
european <- data.frame(n = 9, mean = 1.74, s = 0.57)

t.value.2s(asian, european)

# ex 32
t.value(mean = 29, mu = 0, s = 59, n = 427)

try1 <- data.frame(n = 427, mean = 500, s = 92)
try2 <- data.frame(n = 427, mean = 529, s = 97)

t.2s(try2, try1)

delta <- 2.626*59/sqrt(427)
delta
29 - delta
delta

# ex 33
coached <- data.frame(n = 427, mean = 29, s = 59)
uncoached <- data.frame(n = 2733, mean = 21, s = 52)

t.2s(coached, uncoached)
delta <- round(s.2s(coached, uncoached) * 2.626, 4)

8 + delta

# ex 38
ex38 <- read.delim('ex18-38.dat', strip.white = T, header = T)

ex38.t <- ddply(ex38, .(Process), summarize, 
                n = length(Strength), mean = mean(Strength), s = sd(Strength))

t.2s(subset(ex38.t, Process == 'HY'), subset(ex38.t, Process == 'Perm'))

# ex 39
ex39 <- read.delim('ex18-39.dat', strip.white = T, header = T)

t.test(subset(ex39, Process == 'HY', select = 'Angle'), 
       subset(ex39, Process == 'Perm', select = 'Angle'))

ex39.t <- ddply(ex39, .(Process), summarize, n = 5, mean = mean(Angle), s = sd(Angle))

tex.table(ex39.t, digits = 4)
t.2s(subset(ex39.t, Process == 'HY'), subset(ex39.t, Process == 'Perm'))

# ex 40
ex38.t

diff(ex38.t$mean)
s <- s.2s(subset(ex38.t, Process == 'HY'), subset(ex38.t, Process == 'Perm'))

round(2.132 * s, 4)

# ex 41
ex39.t

diff(ex39.t$mean)
s <- s.2s(subset(ex39.t, Process == 'HY'), subset(ex39.t, Process == 'Perm'), digits = 8)

s
diff(ex39.t$mean) - s * 2.132 

4/sqrt(10000)
