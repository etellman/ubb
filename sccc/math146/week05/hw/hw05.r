root.dir <- "~/Documents/U/ubb/sccc/math146"

data.dir <- paste(root.dir, "data/bps/PC-Text", sep = "/")
hw5.data.dir <- paste(data.dir, "ch05", sep = "/")

hw.dir <- paste(root.dir, "week05/hw", sep = "/")
figures.dir <- paste(hw.dir, "figures", sep = "/")

setwd(hw5.data.dir)

# exercise 27
dd <- function(d) 2.69 + 0.0138 * d
dd(200)

plot <- ggplot(data.frame(x = c(40,300)), aes(x = x)) + 
  stat_function(fun = dd) +
  labs(x = "Depth", y = "Duration") +
  # scale_x_continuous(limits = c(-10, 30)) +
  ggtitle("Exercise 27: Penguin Dives")
print(plot)

ggsave(paste(figures.dir, "ex27.pdf", sep = "/"), width = 4, height = 2.5)


# exercise 30
ex30 <- read.delim("../ch04/ex04-44.dat", header = TRUE, sep = '\t')

plot <- ggplot(ex30, aes(x = Pairs, y = Pct)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = "Pairs", y = "Returning %") +
  ggtitle("Exercise 30: Birds Returning")
print(plot)

ex30.lm <- lm(Pct ~ Pairs, data = ex30)
summary(ex30.lm)
round(predict(ex30.lm, data.frame(Pairs = c(30))), 2)

summary(lm(Pairs ~ Pct, data = ex30))

# exercise 31
ex31 <- function(x) 35.82 + 0.5185 * x
plot <- ggplot(data.frame(x = c(56, 72)), aes(x = x)) + 
  stat_function(fun = ex31) +
  labs(x = "Wife Height", y = "Husband Height") +
  # scale_x_continuous(limits = c(-10, 30)) +
  ggtitle("Exercise 31: Spouse Heights")
print(plot)

ggsave(paste(figures.dir, "ex31.pdf", sep = "/"), width = 4, height = 2.5)

# exercise 32
.6 * 75/280
75 - 0.1607 * 280
y <- function(x) 30.04 + 0.1607 * x
y(300)

# exercise 34
ex34 <- read.delim("ex05-34.dat", header = TRUE, sep = '\t')
ex34.lm <- lm(Sister ~ Brother, data = ex34)

plot <- ggplot(ex34, aes(x = Brother, y = Sister)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = "Brother", y = "Sister") +
  ggtitle("Exercise 34: Sibling Heights")
print(plot)

ggsave(paste(figures.dir, "ex34.pdf", sep = "/"), width = 4, height = 2.5)

round(predict(ex34.lm, data.frame(Brother = c(70))), 2)
summary(ex34.lm)

# exercise 37
ex37 <- read.delim("../ch04/ex04-29.dat", header = TRUE, sep = '\t')
ex37

ex37.lm <- lm(Behave ~ Neural, data = ex37)
summary(ex37.lm)
round(coef(ex37.lm), 4)
round(cor(ex37), 4)


ex37.no.outlier <- subset(ex37, Neural < 150)
ex37.no.outlier.lm <- lm(Behave ~ Neural, data = ex37.no.outlier)
summary(ex37.no.outlier.lm)
round(coef(ex37.no.outlier.lm), 4)
round(cor(ex37.no.outlier), 4)

plot <- ggplot(ex37, aes(x = Neural, y = Behave)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = "Neural", y = "Behavioral") +
  ggtitle("Exercise 37: Loss Aversion")
print(plot)

ggsave(paste(figures.dir, "ex37.pdf", sep = "/"), width = 4, height = 2.5)

# exercise 38
ex38a <- read.delim("ta05-01a.dat", header = TRUE, sep = '\t')
ex38b <- read.delim("ta05-01b.dat", header = TRUE, sep = '\t')
ex38c <- read.delim("ta05-01c.dat", header = TRUE, sep = '\t')
ex38d <- read.delim("ta05-01d.dat", header = TRUE, sep = '\t')

ex38.lm <- lm(y ~ x, data = ex38c)
coef(ex38.lm)
summary(ex38.lm)
round(coef(ex38.lm), 4)
cor(ex38)

plot <- ggplot(ex38d, aes(x = x, y = y)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = "", y = "") +
  ggtitle("Exercise 38 (d)")
print(plot)

ggsave(paste(figures.dir, "ex38d.pdf", sep = "/"), width = 4, height = 2.5)

# exercise 47
ex47 <- function(x) 61.93 + 0.18 * x
ex47(c(70, 80))

# exercise 51
ex51 <- read.delim("ex05-51.dat", header = TRUE, sep = '\t')
str(ex51)

ex51.lm <- lm(Larvae ~ Stumps, data = ex51)
summary(ex51.lm)
round(coef(ex51.lm), 4)
cor(ex51)

plot <- ggplot(ex51, aes(x = Stumps, y = Larvae)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = "Stumps", y = "Larvae") +
  ggtitle("Exercise 51")
print(plot)

ggsave(paste(figures.dir, "ex51.pdf", sep = "/"), width = 4, height = 2.5)

# exercise 53
ex53 <- read.delim("ex05-53.dat", header = TRUE, sep = '\t')
str(ex53)

ex53 <- subset(ex53, Year != 2005)

ex53.lm <- lm(Observed ~ Forecast, data = ex53)
summary(ex53.lm)
round(coef(ex53.lm), 4)
with(ex53, cor(Forecast, Observed))

residuals(ex53.lm)

round(predict(ex53.lm, data.frame(Forecast = c(16))), 2)

plot <- ggplot(ex53, aes(x = Forecast, y = residuals(ex53.lm))) + 
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = "Forecast", y = "Residuals") +
  ggtitle("Exercise 53 Residuals")
print(plot)

ggsave(paste(figures.dir, "ex53_residuals.pdf", sep = "/"), width = 4, height = 2.5)
