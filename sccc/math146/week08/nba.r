root.dir <- '~/Documents/U/ubb/sccc/math146'
nba.dir <- paste(root.dir, 'data', 'nba', sep = '/')

exam.dir <- paste(root.dir, 'week08', sep = '/')
exam.data.dir <- paste(exam.dir, 'data', sep = '/')
figures.dir <- paste(exam.dir, 'figures', sep = '/')

setwd(nba.dir)

# Durant vs. James in points scored
durant <- read.delim('durant_2013-14.csv', header = TRUE, sep = ',', strip.white = TRUE)
james <- read.delim('james_2013-14.csv', header = TRUE, sep = ',', strip.white = TRUE)

durant <- transform(durant, Player = 'Durant')
james <- transform(james, Player = 'James')

durant <- c( 13, 17, 19, 24, 26, 28, 29, 31, 32, 32, 32, 37, 41, 42, 42)
james  <- c(13, 15, 17, 19, 25, 25, 25, 26, 29, 30, 32, 33, 35, 36, 36)

mvps <- melt(cbind(James = James, Durant = Durant))
mvps <- rename(mvps, c('X2' = 'player'))
mvps <- rename(mvps, c('value' = 'points'))

sink(paste(exam.dir, 'r.tex', sep = '/'))
  xtable(ddply(mvps, .(player), function(df) summary(df$points)))
sink()

str(as.data.frame(summary(durant)))

plot <- ggplot(data = mvps, aes(x = player, y = points)) + 
  geom_boxplot(fill = 'lightblue', color = 'black') +
  labs(x = 'Player', y = 'Points')

print(plot)

file <- paste(figures.dir, 'james_vs_durant.pdf', sep = '/')
ggsave(file, plot, width = 4, height = 2.5)

# save for later to reconstruct the answer key
data.file <- paste(exam.data.dir, 'points_sample.csv', sep = '/')
write.csv(nba.sample, file = data.file)

ddply(nba.sample, 'ID', summarize, summary = t(summary(PTS)), mean = mean(PTS))
ddply(nba, 'ID', summarize, summary = t(summary(PTS)), mean = mean(PTS))

james$plus.minus

# compare GmSc
plot <- ggplot(data = mvps, aes(x = ID, y = GmSc)) + 
  geom_boxplot(fill = 'lightblue', color = 'black') +
  labs(x = 'Player', y = 'Points')

durant.close <- subset(durant, abs(pt.diff) < 5)
james.close <- subset(james, abs(pt.diff) < 5)

durant.close.sample <- durant.close[sample(nrow(durant.close), 10), ]
james.close.sample <- james.close[sample(nrow(james.close), 10), ]

close = rbind(durant.close, james.close)

close.sample = rbind(durant.close.sample, james.close.sample)

plot <- ggplot(data = durant.close, aes(x = PTS, y = pt.diff)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = 'Game Score', y = 'Final Score')

print(plot)

with(durant.close, cor(PTS, pt.diff))
with(james.close, cor(PTS, pt.diff))

?melt

close.sample.melted <- melt(close.sample, id.vars = "ID", 
                            measure.vars = c("GmSc", "plus.minus"))

subset(durant.close.sample, select = c("GmSc", "plus.minus"))

cast(close.sample.melted, ID ~ ...)

sink(paste(exam.dir, 'r.tex', sep = '/'))
  xtable(subset(durant.close, select = c("PTS", "pt.diff")))
  xtable(subset(james.close.sample, select = c("PTS", "pt.diff")))
sink()

# histogram of points scored
players <- read.delim('players_2013-14.csv', header = TRUE, sep = ',', strip.white = TRUE)

starters <- subset(players, GS > 30)
str(starters)

mvps <- subset(starters, Player %in% c("LeBron James", "Kevin Durant"))
other.starters <- subset(starters, 
                         !(Player %in% c("LeBron James", "Kevin Durant")))

# starters.sample <- rbind(starters.sample, mvps)
# starters.sample <- rbind(starters.sample, klay)

starters.sample <- other.starters[sample(nrow(other.starters), 22),]
starters.sample <- rbind(starters.sample, mvps)

plot <- ggplot(data = starters.sample, aes(x = PTS)) +
  geom_histogram(fill = 'lightblue', color = 'black', binwidth = 5) +
  labs(x = 'Points', y = 'Games')

print(plot)

file <- paste(figures.dir, 'point_histogram.pdf', sep = '/')
ggsave(file, plot, width = 5, height = 3)

nrow(starters.sample)
starters.sample$Player

subset(starters, select = c('Player', 'PTS'))

data.file <- paste(exam.data.dir, 'starters_sample.csv', sep = '/')
write.csv(starters.sample, file = data.file)

starters.sample

starters.sample <- starters.sample[order(starters.sample$Rk),]

sink(paste(exam.dir, 'r.tex', sep = '/'))
  xtable(subset(starters.sample, select = c("Player", "PTS")))
sink()

# Game Score
starters <- subset(players, GS > 50)
starters$FGP
three.pt.shooters <- subset(starters, !is.na(X3PP) & X3PP > 0.2 & X3PP < 0.5)


str(three.pt.shooters)
three.pt.shooters <- three.pt.shooters[order(-three.pt.shooters$X3PP),c("Player", "X3PP")]


three.pt.shooters[1:10,]

three.pt.shooters <- starters.sample[order(starters.sample$PTS),]

pgs <- subset(starters, Pos == 'PG')

with(three.pt.shooters, data.frame(mean = mean(X3PP), sd = sd(X3PP)))

plot <- ggplot(data = three.pt.shooters, aes(x = X3PP)) +
  geom_histogram(fill = 'lightblue', color = 'black', binwidth = 0.05) +
  labs(x = 'Points', y = 'Games')

print(plot)

(.42 - .36)/.05

pnorm(.42, .36, .05)
qnorm(.20, 36, 5)
qnorm(.20)

# histogram of points scored
player.totals <- read.delim('player_totals_2013-14.csv', header = TRUE, 
                        sep = ',', strip.white = TRUE)

player.totals <- subset(player.totals, Pos %in% c('PF', 'PG', 'SF', 'SG', 'C'))

shooting <- ddply(player.totals, .(Player), summarize, 
  position = first(Pos), fg.made = sum(FG), fg.missed = sum(FGA) - sum(FG))

shooting.condition <- ddply(shooting, .(position), summarize, 
      made.pct = 100 * sum(fg.made) / sum(fg.made, fg.missed),
      missed.pct = 100 * sum(fg.missed) / sum(fg.made, fg.missed)
)

shooting.by.position <- ddply(shooting, .(position), summarize, 
                              made = sum(fg.made),
                              missed = sum(fg.missed))

shooting.by.position

shooting.melted <- melt(shooting, id = c('Player', 'position'))
shooting.marginal <- cast(shooting.melted, position ~ variable, margins = T, fun = sum)

plot <- ggplot(data = shooting.by.position, 
  aes(x = reorder(position, -made), y = made / 1000)) +
  geom_bar(stat = 'identity', color = 'black', fill = 'lightblue') +
  labs(x = 'Position', y = 'Made (thousands)')

file <- paste(figures.dir, 'shots_made.pdf', sep = '/')
ggsave(file, plot, width = 4, height = 2.5)

# draft

draft <- read.delim('draft_2007.csv', header = TRUE,  sep = ',', strip.white = TRUE)
draft$MP <- draft$MP / 1000

cor(draft$Pk, draft$MP, )

summary(with(draft, lm(Pk ~ MP)))
mean(draft$Pk)
     
mean(draft$MP)

plot <- ggplot(data = draft, aes(x = Pk, y = MP)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  stat_smooth(data = subset(draft, Pk != 56 & Pk != 48), method = "lm") +
  labs(x = 'Pick', y = 'Minutes (thousands)')

plot

file <- paste(figures.dir, 'draft_with_regression.pdf', sep = '/')
ggsave(file, plot, width = 4, height = 2.5)

draft$MP
dnp <- subset(draft, is.na(MP))
draft[which(is.na(draft$MP)),'MP']  <- 0

draft.melted <- melt(draft)

draft.stats <- ddply(subset(draft.melted, variable %in% c('Pk', 'MP')), 
  .(variable), summarize, mean = mean(value), sd = sd(value))
draft.stats

sink(paste(exam.dir, 'r.tex', sep = '/'))
  xtable(draft.stats, digits = 1)
sink()

subset(draft, select = c('Pk', 'Player', 'MP'))

with(subset(draft, Pk != 56 & Pk != 48), lm(Pk ~ MP))
