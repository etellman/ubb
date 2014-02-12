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

# passing
with.weather <- merge(team, games, by = "GID")
team.sample <- with.weather[sample(nrow(with.weather), 30),]
with(with.weather, cor(PU, PTS))

plot <- ggplot(team.sample, aes(x = PU, y = PTS)) + 
  geom_point() +
  labs(x = "Punts", y = "Points") +
  ggtitle("Punts vs. Points")
print(plot)

file <- paste(figures.dir, "nfl/punts_vs_points.eps", sep = "/");
ggsave(file, width = 4, height = 2.5)


passing.scaled <- with(team.sample, data.frame(GID, pa = PA, py = PY, 
                                               pa.z = scale(team.sample$PA), 
                                               py.z = scale(team.sample$PY)))

passing.scaled.m <- melt(subset(passing.scaled, select = c(GID, pa, py)), id = "GID")
passing.summary <- cast(passing.scaled.m, variable ~ ., 
                        function(x) round(c(mean = mean(x), sd = sd(x))))


round(with(passing.scaled, cor(pa, py)), 4)

sink(paste(notes.dir, "r.tex", sep = "/"))
  xtable(passing.summary)
sink()

# turnovers

to.diff.s.m <- melt(to.diff.s, id = "GID")
to.stats <- cast(to.diff.s.m, variable ~ ., function(x) c(m = mean(x), s = sd(x)))


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
mean.and.sd <- function(x) {
  c(m = mean(x), s = sd(x))
}

cor.stats <- function(x, y) {
  data.frame(t(c(x = mean.and.sd(x), y = mean.and.sd(y), c = round(cor(x, y), 4))))
}

to.diff.s <- to.diff[sample(nrow(to.diff), 600),]
cor(to.diff.s)

cor(to.diff)

plot <- ggplot(to.diff.s, aes(x = to, y = pts)) + 
  geom_point() +
  labs(x = "Turnovers", y = "Points") +
  ggtitle("Turnover vs. Point Differential")
print(plot)

file <- paste(figures.dir, "nfl/to_vs_pts_all.eps", sep = "/");
ggsave(file, width = 4, height = 2.5)

pts.scaled <- scale(to.diff.s$pts)
to.scaled <- scale(to.diff.s$to)

to.pts.scaled <- data.frame(to = to.diff.s$to, pts = to.diff.s$pts, 
                            to.z = to.scaled, pts.z = pts.scaled)

sink(paste(notes.dir, "r.tex", sep = "/"))
  xtable(to.pts.scaled)
  xtable(cast(to.diff.s.m, variable ~ ., function(x) c(m = mean(x), s = sd(x))))
sink()

to.diff.s.m <- melt(to.diff.s, id = "GID")
cast(to.diff.s.m, variable ~ ., function(x) c(m = mean(x), s = sd(x)))


str(to.stats)

