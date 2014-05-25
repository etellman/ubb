root.dir <- '~/Documents/U/ubb/sccc/math146'

notes.dir <- paste(root.dir, "week15/notes", sep = "/")
data.dir <- paste(root.dir, "data/nfl", sep = "/")
setwd(data.dir)

rush <- read.csv("RUSH.csv"  )
pass <- read.csv("PASS.csv"  )
games <- read.csv("GAMES.csv")
core <- read.csv("CORE.csv"  )
team <- read.csv("TEAM.csv"  )

rush <- rename(rush, c(YDS = 'yds'))

plot <- ggplot(data = subset(rush, yds > -10 & yds < 20), aes(x = yds)) + 
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  labs(x = "Statistic") +
  theme(axis.title.y = element_blank())
print(plot)

sd(rush$yds)
mean(rush$yds)

samplesPerTrial <- 20
trials <- 10

samples <- lapply(rep(samplesPerTrial, trials), function(n) sample(rush$yds, n))

sm <- sapply(samples, function(x) mean(x))
sd <- sd(rush$yds)/sqrt(samplesPerTrial)

min <- mean(rush$yds) - 1 * sd
max <- mean(rush$yds) + 1 * sd
print(c(min, max))

sm
xtable(data.frame(sm))
sm[sm < mean(rush$yds) - 2 * sd | sm > mean(rush$yds) + 2 * sd] 

pnorm(3) - pnorm(-3)

mean(rush$yds) - 2 * sd

(3.7 - 7)/1.41
pnorm((3.7 - 7)/1.41)

rush.and.games <- merge(rush, core, by = "pid")
rush.and.games <- merge(rush.and.games, games, by = "gid")

head(rush.and.games)

rush.and.games <- head(subset(rush.and.games, select = c(gid, seas, yds)))

rush2000 <- subset(rush.and.games, seas == 2000)
rush2012 <- subset(rush.and.games, seas == 2012)

head(subset(rush.and.games, seas == 2000))

mean(rush2012$yds)

sd(rush2000$yds)/sqrt(1000)

nrow(rush.and.games)

rush2012.sample <- sample(rush2012$yds, 5000)

m2012 <- mean(rush2012.sample)
m2000 <- mean(rush2000$yds)

nrow(rush2012)

# passing
subset(pass.and.games, select = c(off))

pass.and.games <- merge(pass, core, by = "pid")
pass.and.games <- merge(pass.and.games, games, by = "gid")

pass2000 <- subset(pass.and.games, seas == 2000)
pass2012 <- subset(pass.and.games, seas == 2012)

sea.def.2012 <- subset(pass2012, def == 'SEA')
ne.off.2012 <- subset(pass2012, off == 'NE')

mean(pass.and.games$yds)
mean(pass2012$yds)
mean(ne.off.2012$yds)

sd(pass2012$yds)/sqrt(100)

nrow(sea.def.2012)

pass2012.sample <- sample(pass2012$yds, 5000)

m2012 <- mean(pass2012.sample)
m2000 <- mean(pass2000$yds)

nrow(pass2012)

# team

team.and.game <- merge(team, games, by = 'GID')
team.2012 <- subset(team.and.game, SEAS == 2012)

head(ddply(team.2012, .(TNAME), transform, tname = TNAME))

ddply(team.and.game, .(TNAME), summarize, mean = mean(PTS))

pnorm(2.433)

s <- 0.001/sqrt(8)
3 * s

272 + 2 * 60/sqrt(840)

(1 - 0.975)/2

(125.8 - 115)/6
1 - pnorm(1.8)
