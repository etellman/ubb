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
  geom_histogram(fill = "lightblue", color = "black", binwidth = 5) +
  labs(x = "Points", y = "Games") +
  ggtitle("Final With Halftime 10")
print(plot)

file <- paste(figures.dir, "ht_10_final.pdf", sep = "/");
ggsave(file, width = 4, height = 2.5)

# plot halftime points vs. median final
mean.pts <- ddply(team, "HPTS", summarize, 
                  pts.m = mean(PTS),
                  hpts.s = mean(hpts.s), 
                  pts.s = mean(pts.s), 
                  games = length(GID))

plot <- ggplot(mean.pts, aes(x = HPTS, y = pts.m)) + 
  geom_point(aes(size = games)) +
  stat_smooth(method = "lm") +
  labs(x = "Half Time", y = "Final") +
  ggtitle("Halftime vs. Median Final Score")
print(plot)

lm(PTS ~ HPTS, data = team)

0.73 * 10 / 7

file <- paste(figures.dir, "ht_vs_median_final.pdf", sep = "/");
ggsave(file, width = 4, height = 2.5)

# plot halftime points vs. median final using z-scores
team$hpts.s <- scale(team$HPTS)
team$pts.s <- scale(team$PTS)


common <- round(subset(mean.pts, games > 500, select = -games), 4)
common

with(common, diff(pts.m) / diff(HPTS))

with(common, 0.73 * diff(pts.m) / diff(HPTS))

sd(team$PTS)
sd(team$HPTS)

mean(with(common, diff(pts.s) / diff(hpts.s)))

with(team, cor(HPTS, PTS))

plot <- ggplot(median.pts, aes(x = hpts.s, y = pts.s)) + 
  geom_point(aes(size = games)) +
  stat_smooth(method = "lm") +
  labs(x = "Half Time", y = "Final") +
  ggtitle("Halftime vs. Median Final Scaled")
print(plot)

file <- paste(figures.dir, "ht_vs_median_final_scaled.pdf", sep = "/");
ggsave(file, width = 4, height = 2.5)

subset(median.pts, hpts == 10)
subset(median.pts, hpts == 14)
subset(median.pts, hpts == 21)

(0.24 + 0.14) / (0.43 + 0.11)
(0.917 - 0.244) / (1.39 - 0.43) 


with(team, cor(HPTS, PTS))

with(team, sd(PTS))
with(team, data.frame(sd(HPTS), mean(HPTS)))

