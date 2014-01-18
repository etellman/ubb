library(ggplot2) 
library(reshape)
library(xtable)

# exercise 25
ex25 <- read.delim("ex01-25.dat", header = TRUE, sep = '\t')
other <- 100 - sum(ex25$Popular)

colors <- c(as.vector(ex25$Color), "Other")
colors

ex25$Color <- factor(colors)
ex25 <- rbind(ex25, data.frame(Color = "Other", Popular = 5))

str(ex25)

plot <- ggplot(ex25, aes(x = reorder(Color, -Popular), y = Popular)) + 
  geom_bar(stat = "identity", fill="lightblue", color = "black") +
  labs(x = "Color", y = "Percentage") +
  ggtitle("Car and Truck Colors")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week01/hw/figures/ex25.eps", width = 4, height = 2.5)

# exercise 26
ex26 <- read.delim("ex01-26.dat", header = TRUE, sep = '\t')
plot <- ggplot(ex26, aes(x = reorder(Age, Pct), y = Pct)) + 
  geom_bar(stat = "identity", fill="lightblue", color = "black") +
  labs(x = "Age", y = "Percentage") +
  coord_flip() +
  ggtitle("Online Music Purchasers")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week01/hw/figures/ex26.eps", width = 4, height = 2.5)

# exercise 29
ex29 <- read.delim("ex01-29.dat", header = TRUE, sep = '\t')

plot <- ggplot(ex29, aes(x = Type, y = Pct)) + 
  geom_bar(stat = "identity", fill="lightblue", color = "black") +
  labs(x = "Type", y = "Percentage") +
  coord_flip() +
  ggtitle("Spam")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week01/hw/figures/ex29a.eps", width = 4, height = 2.5)

plot <- ggplot(ex29, aes(x = reorder(Type, Pct), y = Pct)) + 
  geom_bar(stat = "identity", fill="lightblue", color = "black") +
  labs(x = "Type", y = "Percentage") +
  coord_flip() +
  ggtitle("Spam")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week01/hw/figures/ex29b.eps", width = 4, height = 2.5)

# exercise 34
ex34 <- read.delim("ta01-04.dat", header = TRUE, sep = '\t')

plot <- ggplot(ex34, aes(x = Ratio)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  labs(x = "Ratio", y = "Count") +
  ggtitle("Fatty Acids")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week01/hw/figures/ex34.eps", width = 4, height = 2.5)

# exercise 35
ex35 <- read.delim("ta01-05.dat", header = TRUE, sep = '\t')

plot <- ggplot(ex35, aes(x = MDs)) + 
  geom_histogram(binwidth = 50, color = "black", fill = "lightblue") +
  labs(x = "Doctors per 100,000 People", y = "Count") +
  ggtitle("Doctors")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week01/hw/figures/ex35.eps", width = 4, height = 2.5)

# exercise 37
ex37 <- read.delim("ex01-37.dat", header = TRUE, sep = '\t')
str(ex37)

plot <- ggplot(ex37, aes(x = sole)) + 
  geom_histogram(binwidth = 500, color = "black", fill = "lightblue") +
  labs(x = "Fish", y = "Number of Years") +
  ggtitle("Rock Sole")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week01/hw/figures/ex37.eps", width = 4, height = 2.5)

# exercise 39
ex39 <- read.delim("ex01-37.dat", header = TRUE, sep = '\t')
str(ex39)

plot <- ggplot(ex39, aes(x = year, y = sole)) + 
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Recruitment") +
  ggtitle("Rock Sole")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week01/hw/figures/ex39.eps", width = 4, height = 2.5)

# exercise 40
ex40.pot = factor(c("Never", "1-10 Times", "11-50 Times", "51+ Times"))
ex40.drivers = c(452, 229, 70, 156)
ex40.accidents = c(59, 36, 15, 50)

ex40 <- data.frame(pot = ex40.pot, drivers = ex40.drivers, accidents = ex40.accidents)
str(ex40)

plot <- ggplot(ex40, aes(x = reorder(pot, accidents / drivers), y = accidents / drivers)) + 
  geom_bar(stat = "identity", fill="lightblue", color = "black") +
  labs(x = "Pot Use", y = "Accidents per Driver") +
  ggtitle("Accidents and Pot")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week01/hw/figures/ex40.eps", width = 4, height = 2.5)

# exercise 45
ex45 <- read.delim("ex01-45.dat", header = TRUE, sep = '\t')
str(ex45)

summary(ex45)
nrow(subset(ex45, year >= 1986))

plot <- ggplot(ex45, aes(x = bites)) + 
  geom_histogram(binwidth = 2, color = "black", fill = "lightblue") +
  labs(x = "Bites", y = "Number of Years") +
  ggtitle("Alligator Attacks")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week01/hw/figures/ex45a.eps", width = 4, height = 2.5)

plot <- ggplot(ex45, aes(x = year, y = bites)) + 
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Number of Bites") +
  ggtitle("Alligator Attacks Over Time")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week01/hw/figures/ex45b.eps", width = 4, height = 2.5)
