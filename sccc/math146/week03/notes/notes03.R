
# coin tosses
samples.per.trial <- 100
num.trials <- 10000
num.flips <- num.trials * samples.per.trial
samples <- sample( c(0, 1), num.flips, replace = TRUE)
sizes <- vector(mode = "integer", length = num.trials)
sizes <- sizes + samples.per.trial
p <- partition.vector(samples, sep = sizes)
num.heads <- mapply(sum, p)
totals <- as.data.frame(num.heads)

z <- function(x, values) (x - mean(values)) / sd(values)

z(55, num.heads)
z(65, num.heads)

x <- 1:9/10
qnorm(x)
df <- cbind(x, p = qnorm(x))

sink("~/Documents/U/ubb/sccc/math146/week03/notes/r.tex")
  xtable(df)
sink()

round(pnorm(65, 50, 5.08802) - pnorm(55, 50, 5.08802), 4)

2 * 5.08802 + 50

plot <- ggplot(totals, aes(x = num.heads)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 1, origin = 0.5) +
  stat_function(fun = function(x, trials, mean, sd) trials * dnorm(x, mean, sd), 
                args = c(nrow(totals), 5, sd(totals$num.heads))) +
  scale_x_continuous(breaks = seq(0, 10)) +
  labs(x = "Heads", y = "Count") +
  theme_grey(base_size = 12) +
  ggtitle("Coin Flips (10000 trials)")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week03/notes/figures/coins_10000_100.eps", 
       width = 4, height = 2.5)

plot <- ggplot(totals, aes(x = num.heads)) + 
  geom_histogram(aes(y = ..count../sum(..count..)), fill = "lightblue", 
                 color = "black", binwidth = 1) +
  stat_function(fun = dnorm, args = c(50, sd(totals$num.heads))) +
  # scale_x_continuous(breaks = seq(0, 10)) +
  labs(x = "Heads", y = "Count") +
  theme_grey(base_size = 12) +
  ggtitle("10,000 Trials, 100 flips per trial")
print(plot)

nrow(subset(totals, num.heads == 10))
min(totals)
max(totals)

(70 - mean(totals$num.heads)) / sd(totals$num.heads)

sd(totals$num.heads)

scaled <- scale(totals$num.heads)
str(scaled)

pnorm(45, mean(totals$num.heads), sd(totals$num.heads))

?dnorm

heads.summary <- summary(totals$num.heads)
sd(totals$num.heads)

heads.summary

sink("~/Documents/U/ubb/sccc/math146/week03/notes/r.tex")
  xtable(as.matrix(heads.summary))
sink()

heads.summary


# z scores
score <- round(rnorm(10000, 70, 15))
df.70 <- as.data.frame(score.70)

score.80 <- round(rnorm(100, 80, 10))
score.80 <- as.data.frame(score.80)

scores <- cbind(score.70, score.80)

plot <- ggplot(NULL, aes(x = 0:120)) + 
  # geom_histogram(aes(y = ..count../sum(..count..)), fill = "lightblue", 
  #                color = "black", binwidth = 1) +
  stat_function(fun = dnorm, args = c(70, 15)) +
  stat_function(fun = dnorm, args = c(60, 25)) +
  labs(x = "Score", y = "Fraction") +
  ggtitle("Two Tests")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week03/notes/figures/two_tests.eps", 
       width = 4, height = 2.5)

?dnorm

qnorm(.10, 80, 10)

sd(score.70$score.70)

ggsave("~/Documents/U/ubb/sccc/math146/week03/notes/figures/students/mean70.eps", 
       width = 4, height = 2.5)

# what is a score of 70 in each class?
(95 - 80)/10

# exercise 50
ex50 <- read.delim("ex03-50.dat", header = TRUE, sep = '\t')
str(ex50)
ex50

ex50.s <- round(summary(ex50$Rain))
sd(ex50$Rain)

round(qnorm(c(0.25, 0.5, 0.75), ex50.s["Mean"], sd(ex50$Rain)))
ex50.s

plot <- ggplot(ex50, aes(x = Rain)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 20) +
  labs(x = "Rain", y = "Years") +
  ggtitle("Rain with Bin Width 20")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week03/hw/figures/ex50_histogram_20.eps", 
       width = 4, height = 2.5)

plot <- ggplot(ex50, aes(x = 1, y = Rain)) + 
  geom_boxplot(color = "black", fill = "lightblue", outlier.shape = 21, 
               outlier.size = 1.5) +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") +
  labs(x = "", y = "Rain") +
  coord_flip() +
  ggtitle("Rain Box Plot")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week03/hw/figures/ex50_box.eps", 
       width = 4, height = 2.5)

sink("~/Documents/U/ubb/sccc/math146/week03/hw/r.tex")
xtable(as.matrix(ex50.s))
sink()

ex50.s["Median"] - ex50.s["1st Qu."]
ex50.s["Median"] - ex50.s["3rd Qu."]

ex50.s["Median"] - ex50.s["Min."]
ex50.s["Median"] - ex50.s["Max."]

# NFL

rush <- read.delim("RUSH.csv", header = TRUE, sep = ',', strip.white = TRUE)
players <- read.delim("PLAYERS.csv", as.is = c(2, 3, 4), header = TRUE, sep = ',', 
                      strip.white = TRUE)
games <- read.delim("GAMES.csv", as.is = c(2, 3, 4), header = TRUE, sep = ',', 
                    strip.white = TRUE)
core <- read.delim("CORE.csv", header = TRUE, sep = ',', strip.white = TRUE)

str(rush)

normal.rush <- subset(rush, -10 <= YDS & YDS <= 15)
mean(normal.rush$YDS)
sd(normal.rush$YDS)

negative.rush <- subset(rush, YDS < 0)
nrow(negative.rush) / nrow(subset(rush, YDS <= 15))

z(0, normal.rush$YDS)
1 - round(pnorm(z(0, normal.rush$YDS)), 4)

plot <- ggplot(normal.rush, aes(x = YDS)) + 
  geom_histogram(aes(y = ..count../sum(..count..)), fill = "lightblue", 
                 color = "black", binwidth = 1) +
  stat_function(fun = dnorm, args = c(mean(normal.rush$YDS), sd(normal.rush$YDS))) +
  labs(x = "Yards", y = "Plays") +
  ggtitle("Rushing Plays, -10 to 15 yards")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week03/hw/figures/rushes.eps", 
       width = 4, height = 2.5)
