install.packages("xtable")

library("plyr")
library("ggplot2")
library("reshape")

root.dir <- "~/Documents/U/ubb/sccc/math146"
notes.dir <- paste(root.dir, "week05/notes", sep = "/")
figures.dir <- paste(notes.dir, "figures/nfl", sep = "/")

# NFL
setwd(paste(root.dir, "data/nfl", sep = "/"))
games <- read.delim("GAMES.csv", as.is = c(2, 3, 4), header = TRUE, sep = ',', 
                    strip.white = TRUE)
core <- read.delim("CORE.csv", header = TRUE, sep = ',', strip.white = TRUE)
team <- read.delim("TEAM.csv", header = TRUE, sep = ',', strip.white = TRUE)

# add halftime points column
team$HPTS <- team$X1QP + team$X2QP

# plot halftime vs. final points 
plot <- ggplot(team, aes(x = HPTS, PTS)) + 
  geom_point(alpha = 0.4) +
  stat_smooth(method = "lm") +
  labs(x = "Halftime", y = "Final") +
  ggtitle("Halftime vs. Final")
print(plot)

file <- paste(figures.dir, "ht_vs_final.pdf", sep = "/");
ggsave(file, width = 4, height = 2.5)

# plot final points for games with 10 points at halftime
plot <- ggplot(subset(team, HPTS == 10), aes(x = PTS)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 7) +
  labs(x = "Points", y = "Games") +
  ggtitle("Final With Halftime 10")
print(plot)

file <- paste(figures.dir, "ht_10_final.pdf", sep = "/");
ggsave(file, width = 4, height = 2.5)

# plot halftime points vs. mean final
mean.pts <- ddply(team, "HPTS", summarize, 
                  pts.m = mean(PTS),
                  hpts.s = mean(hpts.s), 
                  pts.s = mean(pts.s), 
                  games = length(GID))

# points
plot <- ggplot(mean.pts, aes(x = HPTS, y = pts.m)) + 
  geom_point(aes(size = games)) +
  stat_smooth(method = "lm") +
  labs(x = "Half Time", y = "Final") +
  ggtitle("Halftime vs. Mean Final Score")
print(plot)

file <- paste(figures.dir, "ht_vs_mean_final.pdf", sep = "/");
ggsave(file, width = 4, height = 2.5)

# z-scores
plot <- ggplot(mean.pts, aes(x = hpts.s, y = pts.s)) + 
  geom_point(aes(size = games)) +
  stat_smooth(method = "lm") +
  labs(x = "Half Time", y = "Final") +
  ggtitle("Halftime vs. Mean Final Scaled")
print(plot)

figures.dir

file <- paste(figures.dir, "ht_vs_mean_final_scaled.pdf", sep = "/");
ggsave(file, width = 4, height = 2.5)

# find regression line

# common halftime scores
common <- round(subset(mean.pts, games > 500, select = -games), 4)

sink(paste(notes.dir, "r.tex", sep = "/"))
  xtable(common)
  with(common, diff(pts.m) / diff(HPTS))
sink()


pts.hpts.lm <- lm(PTS ~ HPTS, data = team)
hpts.pts.lm <- lm(HPTS ~ PTS, data = team)

with(team, mean(HPTS) - 0.5195 * mean(PTS))
hpts.pts.lm

predict(pts.hpts.lm, data.frame(HPTS = 3))
predict(hpts.pts.lm, data.frame(PTS = 10))

with(common, 0.73 * diff(pts.m) / diff(HPTS))

sd(team$PTS)
sd(team$HPTS)

mean(with(subset(mean.pts, games > 100), diff(pts.s) / diff(hpts.s)))
with(subset(mean.pts, games > 100), cor(PTS, HPTS))

with(team, cor(HPTS, PTS))

diff(common$pts.s)
diff(common$hpts.s)

z.x <- round(with(team, (17 - mean(HPTS)) / sd(HPTS)), 2)
round(0.7376 * z.x, 2)
mean(team$PTS) + 0.62 * sd(team$PTS)

round(21.5 - 1.04 * 10.9, 2)

lm(team$PTS ~ team$HPTS)

sd(team$PTS)


with(team, cor(HPTS, PTS))

subset(median.pts, hpts == 10)
subset(median.pts, hpts == 14)
subset(median.pts, hpts == 21)

(-.81 + 1.15)/(-1.07 + 1.48)

(0.24 + 0.14) / (0.43 + 0.11)
(0.917 - 0.244) / (1.39 - 0.43) 


with(team, cor(HPTS, PTS))

with(team, data.frame(sd(HPTS), mean(HPTS)))
with(team, data.frame(sd(PTS), mean(PTS)))

scale(team$HPTS, 7)

# examples

.4 * 100/80
