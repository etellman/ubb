root.dir <- '~/Documents/U/ubb/sccc/math146'
data.dir <- paste(root.dir, 'data', 'nfl', sep = '/')
exam.dir <- paste(root.dir, 'week08', sep = '/')
figures.dir <- paste(exam.dir, 'figures', sep = '/')

# NFL
setwd(data.dir)

team <- read.delim('TEAM.csv', header = TRUE, sep = ',', strip.white = TRUE)
games <- read.delim('GAMES.csv', header = TRUE, sep = ',', strip.white = TRUE)

team.sample <- team[sample(nrow(team), 20),]

sea.2012 <- subset(merge(team, games, by = 'GID'), TNAME == 'SEA' & SEAS == 2012)
plot <- ggplot(data = sea.2012, aes(x = PTS)) + 
  geom_histogram(fill = 'lightblue', color = 'black', binwidth = 5) +
  labs(x = 'Final Score', y = 'Games')
print(plot)


sink(paste(exam.dir, 'r.tex', sep = '/'))
  sort(team.sample$PTS)
sink()

