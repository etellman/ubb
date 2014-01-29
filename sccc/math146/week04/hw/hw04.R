
# exercise 26
ex26 <- read.delim("ex04-26.dat", header = TRUE, sep = '\t')
str(ex26)

plot <- ggplot(ex26, aes(x = Women, y = Men)) + 
  geom_point() +
  labs(x = "Women", y = "Men") +
  # scale_x_continuous(limits = c(-10, 30)) +
  ggtitle("Exercise 26: Date Heights")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week04/hw/figures/ex26.eps", width = 4, height = 2.5)

with(ex26, round(cor(Women, Men), 4))

# exercise 27
ex27 <- read.delim("ex04-27.dat", header = TRUE, sep = '\t')
str(ex27)

plot <- ggplot(ex27, aes(x = Price, y = Deforest)) + 
  geom_point() +
  labs(x = "Coffee Price", y = "Forest Lost") +
  # scale_x_continuous(limits = c(-10, 30)) +
  ggtitle("Exercise 27: Coffee and Deforestation")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week04/hw/figures/ex27.eps", width = 4, height = 2.5)

with(ex27, round(cor(Price, Deforest), 4))

# exercise 28
ex28 <- read.delim("ex04-28.dat", header = TRUE, sep = '\t')
str(ex28)

plot <- ggplot(ex28, aes(x = Newbirds, y = Pctret)) + 
  geom_point() +
  labs(x = "Percent Returning", y = "New Birds") +
  ggtitle("Exercise 28: Birds")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week04/hw/figures/ex28.eps", width = 4, height = 2.5)

with(ex28, round(cor(Pctret, Newbirds), 4))

# exercise 29
ex29 <- read.delim("ex04-29.dat", header = TRUE, sep = '\t')
str(ex29)

plot <- ggplot(ex29, aes(x = Neural, y = Behave)) + 
  geom_point() +
  labs(x = "Neural Activity", y = "Behavior") +
  ggtitle("Exercise 29: Brain Activity")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week04/hw/figures/ex29.eps", width = 4, height = 2.5)

with(subset(ex29, Neural < 100), round(cor(Behave, Neural), 4))

# exercise 31
ex31 <- read.delim("ta04-02.dat", header = TRUE, sep = '\t')
str(ex31)

plot <- ggplot(ex31, aes(x = Time, y = Length, shape = factor(Run))) + 
  geom_point() +
  labs(x = "Time", y = "Length") +
  ggtitle("Exercise 31: Icicles")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week04/hw/figures/ex31.eps", width = 4, height = 2.5)

ddply(ex31, "Run", function(df) c(cor = round(cor(df$Time, df$Length), 4)))

# exercise 32
ex32 <- read.delim("ex04-32.dat", header = TRUE, sep = '\t')
str(ex32)

plot <- ggplot(ex32, aes(x = Plants, y = Yield)) + 
  geom_point() +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2) +
  labs(x = "Plants/Acre", y = "Yield") +
  ggtitle("Exercise 32: Plant Yields")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week04/hw/figures/ex32.eps", width = 4, height = 2.5)

with(ex32, round(cor(Plants, Yield), 4))

# exercise 34
ex34 <- read.delim("ex04-26.dat", header = TRUE, sep = '\t')
str(ex34)

cor(ex34$Women + 3, ex34$Women)
cor(ex34$Men, ex34$Women)

# exercise 43
ex43 <- read.delim("ex04-43.dat", header = TRUE, sep = '\t')
str(ex43)

plot <- ggplot(ex43, aes(x = DD, y = Gas, shape = Data)) + 
  geom_point() +
  labs(x = "Degree Days", y = "Gas Usage") +
  ggtitle("Exercise 43: Solar Panels")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week04/hw/figures/ex43.eps", width = 4, height = 2.5)

# exercise 44
ex44 <- read.delim("ex04-44.dat", header = TRUE, sep = '\t')
str(ex44)

plot <- ggplot(ex44, aes(x = Pairs, y = Pct)) + 
  geom_point() +
  labs(x = "Breeding Pairs", y = "Percent Returning") +
  ggtitle("Exercise 44: Merlins Breeding")
print(plot)

round(cor(ex44$Pairs, ex44$Pct), 4)
qgsave("~/Documents/U/ubb/sccc/math146/week04/hw/figures/ex44.eps", width = 4, height = 2.5)

