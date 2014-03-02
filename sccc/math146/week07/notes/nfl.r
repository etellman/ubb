
root.dir <- "~/Documents/U/ubb/sccc/math146"
notes.dir <- paste(root.dir, "week07/notes", sep = "/")
figures.dir <- paste(notes.dir, "figures", sep = "/")

# NFL
setwd(paste(root.dir, "data/nfl", sep = "/"))
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

# plot halftime points vs. mean final
mean.pts <- ddply(team, "HPTS", summarize, 
                  pts.m = mean(PTS),
                  games = length(GID))

# points
plot <- ggplot(mean.pts, aes(x = HPTS, y = pts.m)) + 
  geom_point(aes(size = games)) +
  stat_smooth(method = "lm") +
  labs(x = "Half Time", y = "Final") +
  ggtitle("Halftime vs. Mean Final Score")
print(plot)

cor(team$HPTS, team$PTS)
summary(lm(team$HPTS ~ team$PTS))


# turnovers
games <- read.delim("GAMES.csv", as.is = c(2, 3, 4), header = TRUE, sep = ',', 
                    strip.white = TRUE)
defense <- read.delim("DEFENSE.csv", header = TRUE, sep = ',', strip.white = TRUE)

# creates a table of halftime vs. final points
scores.by.team <- function(games) {

  points <- subset(games, select = c(GID, V, H, PTSV, PTSH))
  points.m <- melt(points, id = c("GID", "V", "H"))

  visitor.scores <- subset(points.m, variable == "PTSV", select = c(GID, V, value))
  home.scores <- subset(points.m, variable == "PTSH", select = c(GID, H, value))

  visitor.scores <- rename(visitor.scores, c("V" = "TEAM", value = "PTS"))
  home.scores <- rename(home.scores, c("H" = "TEAM", value = "PTS"))

  rbind(visitor.scores, home.scores)
}

scores <- scores.by.team(games)
defense <- subset(defense, select = c("GID", "TEAM", "INT", 'FORC'))
defense.scores <- merge(defense, scores, by = c("TEAM", "GID"))

turnovers <- ddply(defense.scores, c("TEAM", "GID"), 
                   function(df) with(df, c( PTS = PTS[1], TO = sum(INT, FORC))))

to.diff <- ddply(turnovers, "GID", function(df) c(to = diff(df$TO), pts = diff(df$PTS)))

# to.diff <- subset(to.diff, abs(to) > 3)

plot <- ggplot(to.diff, aes(x = to, y = pts)) + 
  geom_point(alpha = 0.4) +
  stat_smooth(method = "lm") +
  labs(x = "Turnovers", y = "Points") +
  ggtitle("Turnover vs. Point Differential")

print(plot)

with(to.diff, cor(to, pts))
with(to.diff, summary(lm(to ~ pts)))

mean.to <- ddply(to.diff, "to", summarize, 
                  pts.m = mean(pts),
                  games = length(GID))

# points
plot <- ggplot(mean.to, aes(x = to, y = pts.m)) + 
  geom_point(aes(size = games)) +
  stat_smooth(method = "lm") +
  labs(x = 'Turnovers', y = 'Point') +
  ggtitle("Turnovers vs. Mean Point Differential")

print(plot)
