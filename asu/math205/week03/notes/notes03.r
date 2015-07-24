math205.dir <- '~/Documents/U/ubb/asu/math205'

week.dir <- paste(math205.dir, 'week03', sep = '/')
figure.dir <- paste(week.dir, 'notes', 'figures', sep = '/')
roulette.dir <- paste(figure.dir, 'roulette', sep = '/')

data.dir <- paste(math205.dir, 'data', sep = '/')
nfl.dir <- paste(data.dir, 'nfl', sep = '/')
setwd(nfl.dir)
nfl.figure.dir <- paste(figure.dir, 'nfl', sep = '/')

library(Hmisc)
library(plyr)

# coin tosses
games.per.player <- 100
num.players <- 10000

?sample

# simulates roulette
#
# games.per.player - number of times to spin in each trial
# num.players - number of trials
spin <- function(games.per.player, num.players, winning.rolls, payout) {
  num.spins <- num.players * games.per.player
  samples <- sample(c(payout, -1), num.spins, replace = T, 
                    c(winning.rolls/38, (38 - winning.rolls)/38))

  sizes <- vector(mode = 'integer', length = num.players) + games.per.player
  p <- partition.vector(samples, sep = sizes)

  money <- mapply(sum, p)
  totals <- as.data.frame(money)

  as.data.frame(totals)
}

s <- spin(games.per.player, 10, winning.rolls, payout)

cbind(s, f = players$a)

odd.even <- spin(games.per.player, num.players, 18, 1)
four.numbers <- spin(games.per.player, num.players, 4, 8)
one.number <- spin(games.per.player, num.players, 1, 35)

sd(one.number$money)
sd(odd.even$money)
sd(four.numbers$money)

spins <- four.numbers
round(z(0, spins$money), 4)
games.per.player <- 50

sd(odd.even$money)

sds <- rbind(
  data.frame(strategy = 'odd.even', s = 7.0),
  data.frame(strategy = 'four.numbers', s = 19.5),
  data.frame(strategy = 'one.number', s = 40.7)
)

?pnorm

pnorm()
s = c(7.0, 19.5, 40.7)
str(s)

max(odd.even)
roulette.to.z <- function(x) { 
  ldply(s, .fun = function(s) 
        data.frame(z = round(z(-2.6, s, x), 4), 
                   p = round(pnorm(x, mean = -2.6, sd = s, x), 2)))
}

?pnorm

roulette.to.z(0)

from.z <- function(a, mu z) {
  z * s + mu
}

roulette.from.z <- function(z) { 
  ldply(s, .fun = function(s) data.frame(x = round(z * s - 2.6, 2)))
}

percentile <- 0.90
qnorm(percentile)
roulette.from.z(qnorm(percentile))


ddply(sds, .(strategy), summarize, z = z(-2.6, s, 0))

?as.data.frame
winning.rolls <- 18
payout <- 1

num.players <- 100000
spins <- spin(games.per.player, num.players, winning.rolls, payout)

odd.even <- spin(games.per.player, num.players, 18, 1)
four.numbers <- spin(games.per.player, num.players, 4, 8)
one.number <- spin(games.per.player, num.players, 1, 35)

spins <- one.number
summary(spins$money)
sd(spins$money)

plot <- ggplot(subset(spins), aes(x = money)) + 
  geom_histogram(aes(y = ..count../sum(..count..)), fill = "grey", 
                 color = "black", binwidth = payout + 1) +
  stat_function(fun = dnorm, args = c(50, sd(totals$money))) +
  theme_ubb + theme(axis.title.y = element_blank()) +
  labs(x = "Winnings") 
print(plot)

save.plot(plot, roulette.dir, sprintf('%d_%d_%d_fraction.pdf', winning.rolls, 
                                   num.players, games.per.player))

mean(spins$money)
plot <- ggplot(spins, aes(x = money)) + 
  geom_histogram(fill = "grey", color = "black", binwidth = payout + 1) +
  theme_ubb + theme(axis.title.y = element_blank()) +
  labs(x = 'Winnings') 
print(plot)

save.plot(plot, coins.dir, sprintf('%d_%d_%d.pdf', winning.rolls, 
                                   num.players, games.per.player))


dbinom(size = 50, prob = .5)
nrow(subset(totals, money == 50))
min(totals)
max(totals)
xtable(summary(totals))

subset(totals, money == max(totals))
(70 - mean(totals$money)) / sd(totals$money)

sd(totals$money)

?scale

scaled <- scale(totals$money)
str(scaled)

pnorm(45, mean(totals$money), sd(totals$money))

?dnorm

heads.sumary <- summary(totals$money)
sd(totals$money)

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
  stat_function(fun = dnorm, args = c(70, 15)) +
  stat_function(fun = dnorm, args = c(60, 25)) +
  scale_x_continuous(breaks = seq(0, 120, 20)) +
  theme_ubb + theme(axis.title = element_blank())
print(plot)

save.plot(plot, figure.dir, 'two_tests.pdf')

round(qnorm(0.95, 60, 25), 4)
round(qnorm(0.95, 70, 15), 4)

sd(score.70$score.70)

ggsave("~/Documents/U/ubb/sccc/math146/week03/notes/figures/students/mean70.eps", 
       width = 4, height = 2.5)

# what is a score of 70 in each class?
(95 - 80)/10

# NFL
rush <- read.delim("RUSH.csv", header = T, sep = ',', strip.white = T)
players <- read.delim("PLAYERS.csv", as.is = c(2, 3, 4), header = T, sep = ',', 
                      strip.white = T)
games <- read.delim("GAMES.csv", as.is = c(2, 3, 4), header = T, sep = ',', 
                    strip.white = T)
core <- read.delim("CORE.csv", header = T, sep = ',', strip.white = T)
team <- read.delim("TEAM.csv", header = T, sep = ',', strip.white = T)

normal.rush <- subset(rush, -10 <= YDS & YDS <= 15)
mean(normal.rush$YDS)
sd(normal.rush$YDS)

negative.rush <- subset(rush, YDS < 0)
nrow(negative.rush) / nrow(subset(rush, YDS <= 15))

z(0, normal.rush$YDS)
1 - round(pnorm(z(0, normal.rush$YDS)), 4)

normal.rush <- subset(rush, -10 < YDS & YDS < 15)
mean(normal.rush$YDS)
sd(normal.rush$YDS)
plot <- ggplot(normal.rush, aes(x = YDS)) + 
  geom_histogram(aes(y = ..count../sum(..count..)), fill = "grey", 
                 color = "black", binwidth = 1) +
  stat_function(fun = dnorm, args = c(mean(normal.rush$YDS), sd(normal.rush$YDS))) +
  theme_ubb + theme(axis.title.y = element_blank()) 
  labs(x = 'Yards') +
  # ggtitle("Rushing Plays, -10 to 15 yards")
print(plot)

save.plot(plot, nfl.figure.dir, 'yards_per_rush.pdf')

# NFL
rush <- read.delim("rush.csv", header = TRUE, sep = ',')
core <- read.delim("core.csv", header = TRUE, sep = ',')
games <- read.delim("games.csv", header = TRUE, sep = ',')
offense <- read.delim("offense.csv", header = TRUE, sep = ',')
kickoffs <- read.delim("kickoffs.csv", header = TRUE, sep = ',')

rush.and.game <- merge(rush, core, by = "PID")

rushes.by.game <- ddply(rush.and.game, c("GID", "OFF"), 
                         function(df) c(nrush = nrow(df), yds = sum(df$YDS)))

nfl.summary <- function(stat) {
  rbind(c(summary = summary(stat), sd = sd(stat)))
}


team.and.game <- merge(team, games, by = "GID")
team.and.game.2012 <- subset(team.and.game, SEAS == 2012)

str(team.and.game)

kickoffs <- subset(kickoffs, KRY > 0)
nfl.summary(team.and.game.2012$PTS)

pnorm(8, 22.89, 10.53)

sink("~/Documents/U/ubb/sccc/math146/week03/notes/r.tex")
  xtable(t(nfl.summary(team.and.game.2012$PTS)))
sink()

normal.rush <- subset(rush, -10 < YDS & YDS < 15)
plot <- ggplot(normal.rush, aes(x = YDS)) + 
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  stat_function(fun = dnorm, args = c(mean(normal.rush$YDS), sd(normal.rush$YDS))) +
  labs(x = "Yards", y = "Games") +
  ggtitle("Rushes")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week03/notes/figures/nfl/kickoff_returns.eps", 
       width = 4, height = 2.5)

plot <- ggplot(rushes.per.game, aes(x = yds)) + 
  geom_histogram(aes(y = ..count../sum(..count..)), 
                 fill = "grey", color = "black") +
  # stat_function(fun = dnorm, 
  #               args = c(mean(rushes.per.game$yds), sd(rushes.per.game$yds))) +
  labs(x = "Statistic", y = "Percent") +
  ggtitle("NFL")
print(plot)

seq(6, 7, 0.1)

mile <- data.frame(x = seq(7.11 - 3 * 0.74, 7.11 + 3 * 0.74, 0.1))
plot <- ggplot(mile, aes(x = x)) + 
  stat_function(fun = dnorm, args = c(7.11, 0.74)) +
  theme_ubb + theme(axis.title.y = element_blank()) +
  labs(x = 'Time (minutes)') 
print(plot)
 
save.plot(plot, figure.dir, 'ayn_3_5.pdf')

?pnorm
pnorm(z.score)

pnorm(1, mean = 0.8, sd = 0.078) - pnorm(0.9, mean = 0.8, sd = 0.078)

round(qnorm(c(0.25, 0.5, 0.75), mean = 0.8, sd = 0.078), 2)

z(680, mean = 515, sd = 114)
z(72, mean = 64, sd = 2.7)

7.11 + 3 * 0.74

68/2

852 + 2*82
