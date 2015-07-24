math205.dir <- '~/Documents/U/ubb/asu/math205'
data.dir <- paste(math205.dir, 'data', 'bps', 'PC-Text', 'ch01', sep = '/')
setwd(data.dir)

week.dir <- paste(math205.dir, 'week01', sep = '/')
figure.dir <- paste(week.dir, 'hw', 'figures', sep = '/')

# exercise 25
ex25 <- read.delim('ex01-25.dat', header = T, sep = '\t')
ex25 <- rbind(ex25, data.frame(Color = 'Other', Popular = 100 - sum(ex25$Popular)))

percent <- function(x) paste(x, '%', sep = '')

plot <- ggplot(ex25, aes(x = reorder(Color, -Popular), y = Popular)) + 
  geom_bar(stat = 'identity', fill = 'grey', color = 'black') +
  theme(axis.title = element_blank()) +
  scale_y_continuous(labels = percent)

?scale_y_discrete

print(plot)
save.plot(plot, figure.dir, 'ex25.pdf')

# exercise 26
ex26 <- read.delim("ex01-26.dat", header = T, sep = ',')

plot <- ggplot(ex26, aes(x = reorder(Age, -Pct), y = Pct)) + 
  geom_bar(stat = "identity", fill="grey", color = "black") +
  theme(axis.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_y_continuous(labels = percent) 

print(plot)
save.plot(plot, figure.dir, 'ex26.pdf')

# exercise 29
ex29 <- read.delim("ex01-29.dat", header = T, sep = '\t')

# alphabetical bars
plot <- ggplot(ex29, aes(x = Type, y = Pct)) + 
  geom_bar(stat = 'identity', fill='grey', color = 'black') +
  theme(axis.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_y_continuous(labels = percent) 

print(plot)
save.plot(plot, figure.dir, 'ex29a.pdf')

plot <- ggplot(ex29, aes(x = reorder(Type, -Pct), y = Pct)) + 
  geom_bar(stat = 'identity', fill='grey', color = 'black') +
  theme(axis.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_y_continuous(labels = percent) 

print(plot)
save.plot(plot, figure.dir, 'ex29b.pdf')

# exercise 34
ex34 <- read.delim("ta01-04.dat", header = T, sep = '\t')

plot <- ggplot(ex34, aes(x = Ratio)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") +
  scale_x_continuous(breaks = 0:6) + 
  theme_ubb +
  theme(axis.title.y = element_blank()) +
  labs(x = "Omega 3 to 6 Ratio") 

print(plot)
save.plot(plot, figure.dir, 'ex34.pdf')

# exercise 35
ex35 <- read.delim("ta01-05.dat", header = T, sep = '\t')

plot <- ggplot(ex35, aes(x = MDs)) + 
  geom_histogram(binwidth = 50, color = "black", fill = "grey") +
  theme_ubb +
  theme(axis.title.y = element_blank()) +
  labs(x = "Doctors per 100,000 People") 

print(plot)
save.plot(plot, figure.dir, 'ex35.pdf')

# exercise 37
ex37 <- read.delim("ex01-37.dat", header = T, sep = '\t')

plot <- ggplot(ex37, aes(x = sole)) + 
  geom_histogram(binwidth = 500, color = 'black', fill = 'grey') +
  theme_ubb +
  labs(x = 'Fish', y = 'Years') 

print(plot)
save.plot(plot, figure.dir, 'ex37.pdf')

# exercise 39
plot <- ggplot(ex37, aes(x = year, y = sole)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1970, 2000, 5)) + 
  theme_ubb +
  theme(axis.title.x = element_blank()) +
  labs(y = "Recruitment") 

print(plot)
save.plot(plot, figure.dir, 'ex39.pdf')

# exercise 40
ex40 <- rbind(
    data.frame(pot = 'Never'       , drivers = 452 , accidents = 59),
    data.frame(pot = '1-10 Times'  , drivers = 229 , accidents = 36),
    data.frame(pot = '11-50 Times' , drivers = 70  , accidents = 15),
    data.frame(pot = '51+ Times'   , drivers = 156 , accidents = 50)
)

rate <- ddply(ex40, 'pot', summarize, rate = round(accidents/drivers, 2))

plot <- ggplot(rate, aes(x = reorder(pot, rate), y = rate)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  theme_ubb +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
  labs(y = "Accidents per Driver") 

print(plot)
save.plot(plot, figure.dir, 'ex40.pdf')

# exercise 45
ex45 <- read.delim("ex01-45.dat", header = T, sep = '\t')
summary(ex45)

plot <- ggplot(ex45, aes(x = bites)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "grey") +
  scale_y_continuous(breaks = seq(0, 8, 2)) + 
  theme_ubb +
  labs(x = "Bites", y = "Years") 

print(plot)
save.plot(plot, figure.dir, 'ex45a.pdf')

plot <- ggplot(ex45, aes(x = year, y = bites)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1970, 2010, 5)) + 
  theme_ubb +
  theme(axis.title.x = element_blank()) +
  labs(y = "Bites per Year") 

ex45
print(plot)
save.plot(plot, figure.dir, 'ex45b.pdf')

m <- round(mean(ex45$bites), 1)
length(subset(ex45$year, ex45$bites > m))
