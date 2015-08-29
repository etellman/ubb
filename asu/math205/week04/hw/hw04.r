math205.dir <- '~/Documents/U/ubb/asu/math205'
data.dir <- paste(math205.dir, 'data', 'bps', 'PC-Text', 'ch04', sep = '/')
setwd(data.dir)

week.dir <- paste(math205.dir, 'week04', sep = '/')
figure.dir <- paste(week.dir, 'hw', 'figures', sep = '/')

# exercise 26
ex26 <- read.delim("ex04-26.dat", header = TRUE, sep = '\t')
with(ex26, round(cor(Women, Men), 4))

plot <- ggplot(ex26, aes(x = Women, y = Men)) + 
  geom_point() +
  theme_ubb +
  labs(x = "Women", y = "Men") 
print(plot)

save.plot(plot, figure.dir, 'ex26.pdf')

# exercise 27
ex27 <- read.delim("ex04-27.dat", header = TRUE, sep = '\t')
str(ex27)

plot <- ggplot(ex27, aes(x = Price, y = Deforest)) + 
  geom_point() +
  labs(x = "Coffee Price", y = "Forest Lost") +
  theme_ubb 
print(plot)

save.plot(plot, figure.dir, 'ex27.pdf')

with(ex27, round(cor(Price, Deforest), 4))

# exercise 28
ex28 <- read.delim("ex04-28.dat", header = TRUE, sep = '\t')
str(ex28)

plot <- ggplot(ex28, aes(x = Newbirds, y = Pctret)) + 
  geom_point() +
  labs(x = "Percent Returning", y = "New Birds") +
  theme_ubb
print(plot)

save.plot(plot, figure.dir, 'ex28.pdf')

with(ex28, round(cor(Pctret, Newbirds), 4))

# exercise 29
ex29 <- read.delim("ex04-29.dat", header = TRUE, sep = '\t')
str(ex29)

plot <- ggplot(ex29, aes(x = Neural, y = Behave)) + 
  geom_point() +
  labs(x = "Neural Activity", y = "Behavior") +
  theme_ubb
print(plot)

save.plot(plot, figure.dir, 'ex29.pdf')

with(subset(ex29, Neural < 100), round(cor(Behave, Neural), 4))

# exercise 31
ex31 <- read.delim("ta04-02.dat", header = TRUE, sep = '\t')
str(ex31)

plot <- ggplot(ex31, aes(x = Time, y = Length, shape = factor(Run))) + 
  geom_point() +
  labs(x = "Time", y = "Length", shape = "Run") +
  theme_ubb
print(plot)

save.plot(plot, figure.dir, 'ex31.pdf')

ddply(ex31, c("Run"), function(df) c(cor = round(cor(df$Time, df$Length), 4)))

# exercise 32
ex32 <- read.delim("ex04-32.dat", header = TRUE, sep = '\t')
str(ex32)

plot <- ggplot(ex32, aes(x = Plants, y = Yield)) + 
  geom_point() +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2) +
  labs(x = "Plants/Acre", y = "Yield") +
  theme_ubb
print(plot)

save.plot(plot, figure.dir, 'ex32.pdf')

with(ex32, round(cor(Plants, Yield), 4))

# exercise 34
ex34 <- read.delim("ex04-26.dat", header = TRUE, sep = '\t')
str(ex34)

cor(ex34$Women + 3, ex34$Women)
cor(ex34$Men, ex34$Women)

# exercise 36
ex36 <- data.frame(x = x, y = y)

x <- c(1, 1.1, 1.21, 1.331, 1.4641, 1.61051) 
y <- c(1, 1.2, 1.44, 1.728, 2.0736, 2.48832)
f(1, 5, 0.2)

f <- function(b, n, r) {
  if (n == 0) {
    b
  } else {
    (1 + r) * f(b, n - 1, r)
  }
}

plot <- ggplot(ex36, aes(x = x, y = y)) + 
  geom_point() +
  theme_ubb
print(plot)

with(ex36, cor(x, y))
save.plot(plot, figure.dir, 'ex36.pdf')

# exercise 43
ex43 <- read.delim("ex04-43.dat", header = TRUE, sep = '\t')
str(ex43)

plot <- ggplot(ex43, aes(x = DD, y = Gas, shape = Data)) + 
  geom_point() +
  labs(x = "Degree Days", y = "Gas Usage", shape = "Solar Panels") +
  scale_shape_discrete(labels = c("After", "Before")) +
  theme_ubb
print(plot)

save.plot(plot, figure.dir, 'ex43.pdf')

with(ex43, cor(DD, Gas))

# exercise 44
ex44 <- read.delim("ex04-44.dat", header = TRUE, sep = '\t')
str(ex44)

plot <- ggplot(ex44, aes(x = Pairs, y = Pct)) + 
  geom_point() +
  labs(x = "Breeding Pairs", y = "Percent Returning") +
  theme_ubb
print(plot)

round(cor(ex44$Pairs, ex44$Pct), 4)
save.plot(plot, figure.dir, 'ex44.pdf')

