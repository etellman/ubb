math205.dir <- '~/Documents/U/ubb/asu/math205'
data.dir <- paste(math205.dir, 'data', 'nfl', sep = '/')
setwd(data.dir)

week.dir <- paste(math205.dir, 'week02', 'notes', sep = '/')
figure.dir <- paste(week.dir, 'figures', 'nfl', sep = '/')

pass <- read.delim('PASS.csv', header = T, sep = ',', strip.white = T)
rush <- read.delim('RUSH.csv', header = T, sep = ',', strip.white = T)
players <- read.delim('PLAYERS.csv', as.is = c(2, 3, 4), header = T, sep = ',', 
                      strip.white = T)
games <- read.delim('GAMES.csv', as.is = c(2, 3, 4), header = T, sep = ',', 
                    strip.white = T)
core <- read.delim('CORE.csv', header = T, sep = ',', strip.white = T)

# narrow down to Seattle
sea.games <- subset(core, OFF == 'SEA' | DEF == 'SEA')
sea.pass <- merge(pass, subset(sea.games, OFF == 'SEA'), by = 'PID')
sea.rush <- merge(rush, subset(sea.games, OFF == 'SEA'), by = 'PID')

# only completions, assuming no gain is incomplete pass
sea.completions <- subset(sea.pass, YDS != 0)

# small samples of Seattle games
sea.pass.sample <- sea.pass[sample(nrow(sea.pass), 15), c("PSR", "TRG", "YDS")]
sea.completions.sample <- subset(sea.pass.sample, YDS != 0)

# save for later 
sample.file <- paste(week.dir, 'sea.pass.sample.csv', sep = '/')
# write.csv(sea.pass.sample, file = sample.file, row.names = F)
sea.pass.sample <- read.delim(file = sample.file, header = T, sep = ',', strip.white = T)

# histogram
plot <- ggplot(sea.pass.sample, aes(x = YDS)) + 
  geom_histogram(binwidth = 10, color = "black", fill = "lightblue") +
  labs(x = "Yards", y = "Plays") +
  theme_grey()
  # scale_x_continuous(limits = c(-10, 20)) +
  # ggtitle("Completions") +
print(plot)

save.plot(plot, figure.dir, 'passes_histogram.pdf')

plot <- ggplot(sea.pass.sample, aes(x = YDS)) + 
  geom_boxplot(color = "black", fill = "lightblue", outlier.shape = 21, outlier.size = 1.5) +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") +
  labs(x = "Yards") +
  theme(axis.title.y = element_blank()) 

print(plot)

play.type <- vector()
play.type[1:nrow(pass)] <- "pass"

pass <- cbind(pass, play.type);
plays <- merge(pass, rush, all = TRUE)

nrow(subset(plays, play.type != "rush"))

# box plot
plot <- ggplot(plays, aes(x = play.type, y = YDS)) + 
  geom_boxplot(color = "black", fill = "lightblue", outlier.shape = 21, outlier.size = 1.5) +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") +
  labs(x = "", y = "Yards") +
  coord_flip() +
  scale_y_continuous(limits = c(-15, 30)) +
  ggtitle("Plays (min -15, max 30)")
print(plot)

ggsave("rushes_and_passes.eps", width = 4, height = 2.5)


nfl.summary <- function(df) {
  c(Cases = sum(df$count), DP = dp.count(df), "Percent" = death.percent(df))
}

df <- ddply(plays, c(Type = "play.type"), 
            function(df) round(c(summary(df$YDS), sd = sd(df$YDS)), 2))

# complete some basic statistics
round(sd(sea.completions$YDS), 2)
x <- summary(subset(sea.rush, select = c("YDS", "BC", "DIR")))
x

# import to notes
sink("~/Documents/U/ubb/sccc/math146/week02/notes/r.tex")
  xtable(x)
sink()

# histogram
plot <- ggplot(rush, aes(x = YDS)) + 
  geom_histogram(binwidth = 2, color = "black", fill = "lightblue") +
  labs(x = "Yards", y = "Plays") +
  scale_x_continuous(limits = c(-10, 30)) +
  ggtitle("Rushes (all games, max 30)")
print(plot)

ggsave("rushes_histogram.eps", width = 4, height = 2.5)

df <- ddply(rush, c("BC"), 
      function(df) round(c(total = sum(df$YDS), sd = sd(df$YDS)), 2))

df <- df[order(-df$total),]
top.20 <- df[1:20,]
top.20

top.20.plays <- merge(top.20, rush, by = "BC")

df.summary <- ddply(top.20.plays, c("BC"), 
      function(df) round(c(total = sum(df$YDS), sd = sd(df$YDS), summary = summary(df$YDS)), 2))

df.summary <- df.summary[order(-df.summary$total),]

sink("~/Documents/U/ubb/sccc/math146/week02/notes/r.tex")
  xtable(as.matrix(df.summary))
sink()


# box plot
plot <- ggplot(top.20.plays, aes(x = reorder(BC, total), y = YDS)) + 
  geom_boxplot(color = "black", fill = "lightblue", outlier.shape = 21, outlier.size = 1.20) +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") +
  labs(x = "Ball Carrier", y = "Yards") +
  coord_flip() +
  scale_y_continuous(limits = c(-8, 15)) +
  ggtitle("Top 20 rushers by total yards")
print(plot)

ggsave("top_rushers.eps", width = 4, height = 6)

str(players)
subset(players, PLAYER == "LM-1000", select = "PNAME")
subset(players, PLAYER == "AM-2850", select = "PNAME")
subset(players, PLAYER == "AF-0900", select = "PNAME")
