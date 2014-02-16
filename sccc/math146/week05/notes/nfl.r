root.dir <- "~/Documents/U/ubb/sccc/math146"
notes.dir <- paste(root.dir, "week05/notes", sep = "/")
figures.dir <- paste(notes.dir, "figures/nfl", sep = "/")

# NFL
setwd(paste(root.dir, "data/nfl", sep = "/"))
games <- read.delim("GAMES.csv", as.is = c(2, 3, 4), header = TRUE, sep = ',', 
                    strip.white = TRUE)
core <- read.delim("CORE.csv", header = TRUE, sep = ',', strip.white = TRUE)
team <- read.delim("TEAM.csv", header = TRUE, sep = ',', strip.white = TRUE)

team$HPTS <- team$X1QP + team$X2QP
str(team)

team.sample <- team[sample(nrow(team), 500),]

plot <- ggplot(team, aes(x = HPTS, PTS)) + 
  geom_point(alpha = 0.4) +
  stat_smooth(method = "lm") +
  labs(x = "Halftime", y = "Final") +
  ggtitle("Halftime vs. Final")
print(plot)

file <- paste(figures.dir, "ht_vs_final.pdf", sep = "/");
ggsave(file, width = 4, height = 2.5)

with(team, cor(HPTS, PTS))
with(team, sd(PTS))
with(team, data.frame(sd(HPTS), mean(HPTS)))

# plot halftime points vs. median final
median.pts <- ddply(team, "HPTS", summarize, 
                    PTS = median(PTS), count = length(PTS))
median.pts$games <- daply(team, "HPTS", nrow)

plot <- ggplot(median.pts, aes(x = HPTS, y = PTS)) + 
  geom_point(aes(size = games)) +
  stat_smooth(method = "lm") +
  labs(x = "Half Time", y = "Final") +
  ggtitle("Halftime vs. Median Final Score")
print(plot)

file <- paste(figures.dir, "ht_vs_median_final.pdf", sep = "/");
ggsave(file, width = 4, height = 2.5)

