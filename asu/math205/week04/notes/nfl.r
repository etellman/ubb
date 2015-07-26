math205.dir <- '~/Documents/U/ubb/asu/math205'

week.dir <- paste(math205.dir, 'week04', sep = '/')
figure.dir <- paste(week.dir, 'notes', 'figures', 'nfl', sep = '/')

data.dir <- paste(math205.dir, 'data', sep = '/')
nfl.dir <- paste(data.dir, 'nfl', sep = '/')
setwd(nfl.dir)

# NFL
games <- read.delim("GAMES.csv", as.is = c(2, 3, 4), header = TRUE, sep = ',', 
                    strip.white = TRUE)
team <- read.delim("TEAM.csv", header = TRUE, sep = ',', strip.white = TRUE)

teams.and.games <- merge(team, games, by = "GID")

# passing
game.sample <- teams.and.games[sample(nrow(teams.and.games), 30),]
passing.scaled <- with(game.sample, data.frame(GID, pa = PA, py = PY, 
                                               pa.z = z.scale(game.sample$PA), 
                                               py.z = z.scale(game.sample$PY)))
with(passing.scaled, round(cor(pa.z, py.z), 4))

plot <- ggplot(passing.scaled, aes(x = pa.z, y = py.z)) + 
  geom_point() +
  theme_ubb + 
  labs(x = "Attempts", y = "Yards") 
print(plot)

save.plot(plot, figure.dir, "attempts_vs_yards_scaled.pdf")

# punts
game.sample <- teams.and.games[sample(nrow(teams.and.games), 30),]
with(game.sample, cor(PU, PTS))

plot <- ggplot(game.sample, aes(x = PU, y = PTS)) + 
  geom_point() +
  theme_ubb + 
  labs(x = "Punts", y = "Points") 
print(plot)

with(game.sample, round(cor(PU, PTS), 4))
save.plot(plot, figure.dir, "punts_vs_points.pdf")


