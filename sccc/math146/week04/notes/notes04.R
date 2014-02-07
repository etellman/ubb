root.dir <- "~/Documents/U/ubb/sccc/math146/week04/notes"

# NFL
rush <- read.delim("RUSH.csv", header = TRUE, sep = ',', strip.white = TRUE)
players <- read.delim("PLAYERS.csv", as.is = c(2, 3, 4), header = TRUE, sep = ',', 
                      strip.white = TRUE)
games <- read.delim("GAMES.csv", as.is = c(2, 3, 4), header = TRUE, sep = ',', 
                    strip.white = TRUE)
core <- read.delim("CORE.csv", header = TRUE, sep = ',', strip.white = TRUE)
team <- read.delim("TEAM.csv", header = TRUE, sep = ',', strip.white = TRUE)

str(team)

team.sample <- team[sample(nrow(team), 500),]
team.small.sample <- team[sample(nrow(team), 20),]

plot <- ggplot(team.small.sample, aes(x = PA, y = PY)) + 
  geom_point() +
  labs(x = "Attempts", y = "Yards") +
  ggtitle("Pass Attempts vs. Passing Yards")
print(plot)

with(team.small.sample, round(cor(PA, PY), 4))

file <- paste(root.dir, "figures/nfl/passing_attempts_vs_yds.eps", sep = "/");
ggsave(file, width = 4, height = 2.5)

sink(paste(root.dir, "r.tex", sep = "/"))
  xtable(t(subset(team.small.sample[11:20,], select = c(PA, PY))))
sink()

?xtable
