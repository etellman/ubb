
root.dir <- "~/Documents/U/ubb/sccc/math146"
notes.dir <- paste(root.dir, "week07/notes", sep = "/")
figures.dir <- paste(notes.dir, "figures", sep = "/")

data.dir <- paste(root.dir, 'data/bps/PC-Text/ch07', sep = '/')
setwd(data.dir)

# exercise 5
ex5 <- read.delim('ex07-05.dat', header = TRUE, sep = ',', strip.white = TRUE)

str(ex5)

plot <- ggplot(data = ex5, aes(x = Weight)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 5) +
  labs(x = "Weight", y = "Diamonds") 

print(plot)
ggsave(paste(figures.dir, 'ex05.pdf', sep = '/'), plot)

sink(paste(notes.dir, 'r.tex', sep = '/'))
  xtable(summary(ex5))
sink()

# exercise 8
ex8 <- read.delim('ex07-08.dat', header = TRUE, sep = ',', strip.white = TRUE)

plot <- ggplot(data = ex8, aes(x = Nppm)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 100) +
  labs(x = "Nitrogen", y = "Number of Diamonds")

print(plot)
ggsave(paste(figures.dir, 'ex08.pdf', sep = '/'), plot)


ex8$type[1:nrow(ex8)] <- 'with.outlier'

ex8

ex8.no.outlier <- subset(ex8, Nppm < 1000)
ex8.no.outlier$type[1:nrow(ex8.no.outlier)] <- 'no.outlier'

ex8 <- rbind(ex8, ex8.no.outlier)

ex8.summary <- ddply(ex8, 'type', function(df) summary = summary(df$Nppm))
ddply(ex8, 'type', function(df) sd = sd(df$Nppm))

sink(paste(notes.dir, 'r.tex', sep = '/'))
  xtable(t(ex8.summary))
sink()


sd(ex8.no.outlier$Nppm)

# exercise 9
aleppo <- read.delim('ex07-09a.dat', header = TRUE, sep = ',', strip.white = TRUE)
torrey <- read.delim('ex07-09b.dat', header = TRUE, sep = ',', strip.white = TRUE)

aleppo$type[1:nrow(aleppo)] <- 'aleppo'
torrey$type[1:nrow(torrey)] <- 'torrey'

ex9 <- rbind(aleppo, torrey)

plot <- ggplot(data = ex9, aes(x = type, y = Length)) + 
  geom_boxplot(fill = 'lightblue', color = 'black') +
  labs(x = 'Tree Type', y = 'Needle Length')

print(plot)
ggsave(paste(figures.dir, 'ex09.pdf', sep = '/'), plot)

ex9.summary <- ddply(ex9, 'type', function(df) summary = summary(df$Length))

sink(paste(notes.dir, 'r.tex', sep = '/'))
  xtable(t(ex9.summary))
sink()

# exercise 13
plot <- ggplot(data = aleppo, aes(x = Length)) + 
  geom_histogram(fill = 'lightblue', color = 'black', binwidth = 2) +
  labs(x = 'Needle Length', y = 'Number of Trees')

print(plot)
ggsave(paste(figures.dir, 'ex13_aleppo.pdf', sep = '/'), plot)

# exercise 17
1 - pnorm(13, 9.6, 2)
pnorm(12, 9.6, 2) - pnorm(8, 9.6, 2)

# exercise 18
1 - pnorm(90, 75, 8.3)
qnorm(c(0.25, 0.75), 75, 8.3)

# exercise 19
soap <- read.delim('ex07-19.dat', header = TRUE, sep = '\t', strip.white = TRUE)

plot <- ggplot(data = soap, aes(x = Day, y = Weight)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = 'Day', y = 'Weight')

print(plot)
ggsave(paste(figures.dir, 'ex19.pdf', sep = '/'), plot)

with(soap, cor(Day, Weight))
with(soap, lm(Weight ~ Day))

# exercise 23
monkeys <- read.delim('ex07-23.dat', header = TRUE, sep = '\t', strip.white = TRUE)

plot <- ggplot(data = monkeys, aes(x = Mass, y = Energy, shape = Group)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = 'Mass', y = 'Energy')

print(plot)
ggsave(paste(figures.dir, 'ex23.pdf', sep = '/'), plot)

monkey.stats <- ddply(monkeys, .(Group), summarize, mean = mean(Mass), 
                      sd = sd(Mass), r = cor(Mass, Energy))

monkeys
with(subset(monkeys, Group == 'Lean'), lm(Energy ~ Mass))
with(subset(monkeys, Group == 'Obese'), lm(Energy ~ Mass))

sink(paste(notes.dir, 'r.tex', sep = '/'))
  xtable(monkey.stats)
sink()

# exercise 24
smoking <- read.delim('ex07-24.dat', header = TRUE, sep = '\t', strip.white = TRUE)
smoking

plot <- ggplot(data = smoking, aes(x = Year, y = Smokers)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = 'Year', y = 'Smokers')

print(plot)
ggsave(paste(figures.dir, 'ex24.pdf', sep = '/'), plot)

smoking.stats <- with(smoking, data.frame(mean(Smokers), mean(Year), 
                                          sd(Smokers), sd(Year), 
                                          cor(Year, Smokers)))

smoking
with(smoking, lm(Smokers ~ Year))

1066 - .52 * 1970

sink(paste(notes.dir, 'r.tex', sep = '/'))
  xtable(t(smoking.stats))
sink()

# exercise 25
ex25 <- read.delim('ex07-25.dat', header = TRUE, sep = '\t', strip.white = TRUE)
ex25

plot <- ggplot(data = ex25, aes(x = Cones, y = Offspr)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = 'Cones', y = 'Offspring')

print(plot)
ggsave(paste(figures.dir, 'ex25.pdf', sep = '/'), plot)

ex25.stats <- with(ex25, data.frame(mean(Cones), mean(Offspr), 
                                    sd(Cones), sd(Offspr), cor(Cones, Offspr)))

ex25.stats
with(ex25, lm(Offspr ~ Cones))

sink(paste(notes.dir, 'r.tex', sep = '/'))
  xtable(t(ex25.stats), digits = 2)
sink()

# exercise 34
ex34 <- read.delim('ex07-34.dat', header = TRUE, sep = '\t', strip.white = TRUE)
ex34

marginal <- ddply(ex34, .(Reason), summarize, value = sum(Count) / sum(ex34$Count))
marginal <- cast(marginal, Reason ~ ., margins = T, fun = sum)

american  <-  sum(subset(ex34, Origin == 'Amer', select = 'Count'))
asian  <-  sum(subset(ex34, Origin == 'Asian', select = 'Count'))

ex34[ex34$Origin == 'Amer',"total"]  <-  american
ex34[ex34$Origin == 'Asian',"total"]  <-  asian

ex34

conditional <- ddply(ex34, .(Reason, Origin), summarize, value = Count / total)
conditional.c <- cast(conditional, Reason ~ Origin)
conditional.c

sink(paste(notes.dir, 'r.tex', sep = '/'))
  xtable(conditional.c, digits = 2)
sink()
