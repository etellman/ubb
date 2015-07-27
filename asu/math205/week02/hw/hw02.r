math205.dir <- '~/Documents/U/ubb/asu/math205'
data.dir <- paste(math205.dir, 'data', 'bps', 'PC-Text', 'ch02', sep = '/')
setwd(data.dir)

week.dir <- paste(math205.dir, 'week02', sep = '/')
figure.dir <- paste(week.dir, 'hw', 'figures', sep = '/')
 
# exercise 29
ex29 <- read.delim('ta02-01.dat', header = T, sep = '\t')

plot <- ggplot(ex29, aes(x = reorder(Variety, Length), y = Length)) + 
  geom_boxplot(color = 'black', fill = 'grey', outlier.shape = 21, outlier.size = 1.5) +
  stat_summary(fun.y = 'mean', geom = 'point', shape = 23, size = 2, fill = 'white') +
  theme_ubb + theme(axis.title.y = element_blank()) +
  scale_x_discrete(labels = c('yellow' = 'Yellow', 'red' = 'Red', 'bihai' = 'Bihai')) +
  labs(y = 'Length (cm)') +
  coord_flip() 
print(plot)

save.plot(plot, figure.dir, 'ex29.pdf')

df <- ddply(ex29, "Variety", function(df) summary(df$Length))
sink("~/Documents/U/ubb/sccc/math146/week02/hw/r.tex")
xtable(df, digits = 2)
sink()

# histogram
plot <- ggplot(subset(ex29, Variety == "yellow"), aes(x = Length)) + 
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  theme_ubb + theme(axis.title.y = element_blank()) +
  labs(x = "Length") +
print(plot)

# exercise 32
ex32 <- read.delim("../ch01/ex01-38.dat", header = T, sep = '\t')
ddply(ex32, "Sex", summarize, mean = mean(Study), sd = sd(Study))

# exercise 37
ex37 <- read.delim("../ch01/ta01-01.dat", header = T, sep = '\t')
big.states <- subset(ex37, State %in% c("California", "NewYork", "Texas", "Florida"))

sink("~/Documents/U/ubb/sccc/math146/week02/hw/r.tex")
xtable(big.states, digits = 1)
sink()

mean(ex37$PctFor)
mean(big.states$PctFor)

# exercise 43
ex43 <- read.delim("ta02-02.dat", header = T, sep = '\t')
ex43.summary <- as.data.frame(as.matrix(summary(ex43$Salary)))

ex43.summary
ex43 <- cbind(ex43, rank = rank(-ex43$Salary))

quantile(ex43$Salary, type = 6)

sum(subset(ex43, rank <= 3, select = "Salary")) / sum(ex43$Salary)

sink("~/Documents/U/ubb/sccc/math146/week02/hw/r.tex")
xtable(ex43.summary, digits = 0)
sink()

plot <- ggplot(subset(ex43), aes(x = Salary / 10^6)) + 
  geom_histogram(binwidth = 5, fill = 'grey', color = 'black') +
  theme_ubb + theme(axis.title.y = element_blank()) +
  labs(x = 'Salary (millions)')

print(plot)
save.plot(plot, figure.dir, 'ex43.pdf')

# exercise 44
ex44 <- read.delim("ex02-44.dat", header = T, sep = '\t')
ex44.summary <- as.data.frame(as.matrix(summary(ex44$Return)))
ex44.summary

xtable(ex44.summary, digits = 2)

plot <- ggplot(ex44, aes(x = Return)) + 
  geom_histogram(binwidth = 10, fill = "grey", color = "black") +
  scale_x_continuous(breaks = seq(-20, 40, 20), labels = percent) + 
  theme_ubb + theme(axis.title = element_blank())
print(plot)

save.plot(plot, figure.dir, 'ex44.pdf')

# exercise 45
ex45 <- read.delim("ta02-03.dat", header = T, sep = '\t')

ex45.summary <- ddply(ex45, "Odor", summarize, 
                      Median = median(Euros), Mean = mean(Euros))
ex45.summary

sink("~/Documents/U/ubb/sccc/math146/week02/hw/r.tex")
xtable(ex45.summary, digits = 1)
sink()

plot <- ggplot(ex45, aes(x = reorder(Odor, Euros), y = Euros)) + 
  geom_boxplot(color = 'black', fill = 'grey', outlier.shape = 21, outlier.size = 1.5) +
  stat_summary(fun.y = 'mean', geom = 'point', shape = 23, size = 2, fill = 'white') +
  theme_ubb + theme(axis.title.y = element_blank()) +
  scale_x_discrete(labels = c('noodor' = 'No Odor', 'lemon' = 'Lemon', 
                              'lavender' = 'Lavender')) +
  coord_flip() +
  labs(y = 'Sales (Euros)')
print(plot)

save.plot(plot, figure.dir, 'ex45.pdf')

# exercise 50
ex50 <- read.delim("../ch01/ta01-06.dat", header = T, sep = '\t')
ex50 <- cbind(ex50, rank = rank(-ex50$CO2))

ex50.summary <- ddply(ex50, c(), function(df) summary(df$CO2))
ex50.summary

sink("~/Documents/U/ubb/sccc/math146/week02/hw/r.tex")
xtable( t(ex50.summary) , digits = 0)
sink()

plot <- ggplot(ex50, aes(x = 1, y = CO2)) + 
  geom_boxplot(color = "black", fill = "grey", outlier.shape = 21, outlier.size = 1.5) +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") +
  labs(x = "Country", y = "CO2") +
  ggtitle("CO2 Emissions")
print(plot)

plot <- ggplot(subset(ex50), aes(x = CO2)) + 
  geom_histogram(binwidth = 2, fill = "grey", color = "black") +
  theme_ubb + theme(axis.title.y = element_blank()) +
  labs(x = "CO2 Emissions (tons)")
print(plot)

save.plot(plot, figure.dir, 'ex50.pdf')

