root.dir <- "~/Documents/U/ubb/sccc/math146"
notes.dir <- paste(root.dir, "week04/notes", sep = "/")
figures.dir <- paste(notes.dir, "figures", sep = "/")

# NFL
setwd(paste(root.dir, "data/nfl", sep = "/"))
rush <- read.delim("RUSH.csv", header = TRUE, sep = ',', strip.white = TRUE)
players <- read.delim("PLAYERS.csv", as.is = c(2, 3, 4), header = TRUE, sep = ',', 
                      strip.white = TRUE)
games <- read.delim("GAMES.csv", as.is = c(2, 3, 4), header = TRUE, sep = ',', 
                    strip.white = TRUE)
core <- read.delim("CORE.csv", header = TRUE, sep = ',', strip.white = TRUE)
team <- read.delim("TEAM.csv", header = TRUE, sep = ',', strip.white = TRUE)
fumbles <- read.delim("FUMBLES.csv", header = TRUE, sep = ',', strip.white = TRUE)
ints <- read.delim("INTS.csv", header = TRUE, sep = ',', strip.white = TRUE)
defense <- read.delim("DEFENSE.csv", header = TRUE, sep = ',', strip.white = TRUE)
offense <- read.delim("offense.csv", header = TRUE, sep = ',', strip.white = TRUE)

defense.games <- merge(defense, games, by = "GID")
offense.games <- merge(offense, games, by = "GID")

str(defense.games)

defense.games

visitor.defense <- ddply(subset(defense.games, V == TEAM), "GID", 
      function(df) c(ptsh = df$PTSH[1], fpts = sum(df$FPTS)))

visitor.offense <- ddply(subset(offense.games, V == TEAM), "GID", 
      function(df) c(ptsv = df$PTSV[1], fpts = sum(df$FPTS)))

visitor.defense.s <- visitor.defense[sample(nrow(visitor.defense), 200),]

visitor.offense.s <- visitor.offense[sample(nrow(visitor.offense), 50),]

visitor.defense.s

plot <- ggplot(visitor.offense.s, aes(x = fpts, y = ptsv)) + 
  geom_point() +
  labs(x = "Offense Fantasy Points", y = "Offense Points") +
  ggtitle("Offense Fantasy Points vs. Offense Points")
print(plot)

str(visitor.defense)

fumbles.games <- merge(fumbles.core, games, id = "GID")

str(fumbles.games)
visitor.fumbles <- subset(fumbles.games, OFF == V, select = c(GID, OFF, DEF, V, H, PTSV, PTSH))

home.fumbles.by.game <- ddply(home.fumbles, "GID", 
                                 function(df) c(home.fumbles = nrow(df)))

str(visitor.fumbles.by.game)

fumbles.by.game <- merge(fumbles.by.game, games, id = GID)

fumbles.by.game <- subset(fumbles.by.game, 
                          select = c(GID, home.fumbles, visitor.fumbles, PTSV, PTSH))

str(fumbles.by.game)

diffs <- function(df) {
  fumble.diff <- df$home.fumbles - df$visitor.fumbles
  pts.diff  <-  df$PTSH - df$PTSV
  c(fumble.diff =  fumble.diff, pts.diff = pts.diff)
}




nfl.stats <- with(team.sample, round(cor.stats(PA, PY), 4))
nfl.stats

passing.scaled <- with(team.sample, data.frame(cbind(pa = PA, scale(team.sample$PA), 
                                          py = PY, scale(team.sample$PY))))

team.sample.m <- melt(subset(team.sample, select = c(PA, PY)))

nfl.stats <- cast(team.sample.m, variable ~ ., function(x) round(c(mean = mean(x), sd = sd(x))))

round(with(team.sample, cor(PA, PY)), 4)

file <- paste(root.dir, "figures/nfl/passing_attempts_vs_yds.eps", sep = "/");
ggsave(file, width = 4, height = 2.5)

sink(paste(notes.dir, "r.tex", sep = "/"))
  xtable(passing.scaled)
  # xtable(as.matrix(nfl.stats), digits = 0)
sink()

games.s <- games[sample(nrow(games), 10),]

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
defense.scores <- merge(defense, scores, by = c("TEAM", "GID"))

turnovers <- ddply(defense.scores, c("TEAM", "GID"), 
                   function(df) with(df, c( PTS = PTS[1], TO = sum(INT, FORC))))

to.diff <- ddply(turnovers, "GID", function(df) c(to = diff(df$TO), pts = diff(df$PTS)))
to.diff.s <- to.diff[sample(nrow(to.diff), 40),]

plot <- ggplot(to.diff.s, aes(x = to, y = pts)) + 
  geom_point() +
  labs(x = "Turnovers", y = "Points") +
  ggtitle("Turnover vs. Point Differential")
print(plot)

file <- paste(figures.dir, "nfl/to_vs_pts.eps", sep = "/");
ggsave(file, width = 4, height = 2.5)


mean.and.sd <- function(x) {
  c(m = mean(x), s = sd(x))
}

cor.stats <- function(x, y) {
  data.frame(t(c(x = mean.and.sd(x), y = mean.and.sd(y), c = round(cor(x, y), 4))))
}

str(to.diff.s)

pts.scaled <- scale(to.diff.s$pts)
to.scaled <- scale(to.diff.s$to)

nrow(to.diff.s.scaled)

to.diff.s.scaled <- cbind(to.diff.s, to.scaled, pts.scaled)

to.diff.s.scaled <- subset(to.diff.s.scaled, select = -GID)

str(to.diff.s.scaled)

to.diff.s.m <- melt(to.diff.s, id = "GID")
to.stats <- cast(to.diff.s.m, variable ~ ., function(x) c(m = mean(x), s = sd(x)))

sink(paste(notes.dir, "r.tex", sep = "/"))
  xtable(to.stats)
sink()

round(cor(to.diff.s$to, to.diff.s$pts), 4)
stats <- with(to.diff.s, cor.stats(to, pts))
str(stats)

