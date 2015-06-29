math205.dir <- '~/Documents/U/ubb/asu/math205'
data.dir <- paste(math205.dir, 'data', 'bps', 'PC-Text', 'ch01', sep = '/')
setwd(data.dir)

week.dir <- paste(math205.dir, 'week01', sep = '/')
figure.dir <- paste(week.dir, 'hw', 'figures', sep = '/')

# exercise 25
ex25 <- read.delim("ex01-25.dat", header = T, sep = '\t')
other <- 100 - sum(ex25$Popular)
colors <- c(as.vector(ex25$Color), "Other")

ex25$Color <- factor(colors)
ex25 <- rbind(ex25, data.frame(Color = "Other", Popular = 5))

plot <- ggplot(ex25, aes(x = reorder(Color, -Popular), y = Popular)) + 
  geom_bar(stat = "identity", fill="lightblue", color = "black") +
  labs(x = "Color", y = "Percentage")

print(plot)
save.plot(plot, figure.dir, 'ex25.pdf')

# exercise 26
ex26 <- read.delim("ex01-26.dat", header = T, sep = '\t')
plot <- ggplot(ex26, aes(x = reorder(Age, Pct), y = Pct)) + 
  geom_bar(stat = "identity", fill="lightblue", color = "black") +
  labs(x = "Age", y = "Percentage") +
  coord_flip() 

print(plot)
save.plot(plot, figure.dir, 'ex26.pdf')

# exercise 29
ex29 <- read.delim("ex01-29.dat", header = T, sep = '\t')

plot <- ggplot(ex29, aes(x = Type, y = Pct)) + 
  geom_bar(stat = "identity", fill="lightblue", color = "black") +
  labs(x = "Type", y = "Percentage") +
  coord_flip()

print(plot)
save.plot(plot, figure.dir, 'ex29a.pdf')

plot <- ggplot(ex29, aes(x = reorder(Type, Pct), y = Pct)) + 
  geom_bar(stat = "identity", fill="lightblue", color = "black") +
  labs(x = "Type", y = "Percentage") +
  coord_flip()

print(plot)
save.plot(plot, figure.dir, 'ex29b.pdf')

# exercise 34
ex34 <- read.delim("ta01-04.dat", header = T, sep = '\t')

plot <- ggplot(ex34, aes(x = Ratio)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  labs(x = "Ratio", y = "Count") 

print(plot)
save.plot(plot, figure.dir, 'ex34.pdf')

# exercise 35
ex35 <- read.delim("ta01-05.dat", header = T, sep = '\t')

plot <- ggplot(ex35, aes(x = MDs)) + 
  geom_histogram(binwidth = 50, color = "black", fill = "lightblue") +
  labs(x = "Doctors per 100,000 People", y = "Count") 

print(plot)
save.plot(plot, figure.dir, 'ex35.pdf')

# exercise 37
ex37 <- read.delim("ex01-37.dat", header = T, sep = '\t')

plot <- ggplot(ex37, aes(x = sole)) + 
  geom_histogram(binwidth = 500, color = "black", fill = "lightblue") +
  labs(x = "Fish", y = "Number of Years") 

print(plot)
save.plot(plot, figure.dir, 'ex37.pdf')

# exercise 39
plot <- ggplot(ex37, aes(x = year, y = sole)) + 
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Recruitment") 

print(plot)
save.plot(plot, figure.dir, 'ex39.pdf')


# exercise 40
ex40.pot = factor(c("Never", "1-10 Times", "11-50 Times", "51+ Times"))
ex40.drivers = c(452, 229, 70, 156)
ex40.accidents = c(59, 36, 15, 50)

ex40 <- data.frame(pot = ex40.pot, drivers = ex40.drivers, accidents = ex40.accidents)

rates <- ddply(ex40, "pot", function(df) df$accidents / df$drivers)
rates <- merge(ex40, rates)

rates <- rates[order(rates$V1),]

# sink("~/Documents/U/ubb/sccc/math146/week01/hw/r.tex")
#   xtable(rates, digits = 2)
# sink()

plot <- ggplot(ex40, aes(x = reorder(pot, accidents / drivers), y = accidents / drivers)) + 
  geom_bar(stat = "identity", fill="lightblue", color = "black") +
  labs(x = "Pot Use", y = "Accidents per Driver") 

print(plot)
save.plot(plot, figure.dir, 'ex40.pdf')

# exercise 45
ex45 <- read.delim("ex01-45.dat", header = T, sep = '\t')
summary(ex45)

plot <- ggplot(ex45, aes(x = bites)) + 
  geom_histogram(binwidth = 2, color = "black", fill = "lightblue") +
  labs(x = "Bites", y = "Number of Years") 

print(plot)
save.plot(plot, figure.dir, 'ex45a.pdf')

plot <- ggplot(ex45, aes(x = year, y = bites)) + 
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Number of Bites") 

print(plot)
save.plot(plot, figure.dir, 'ex45b.pdf')
