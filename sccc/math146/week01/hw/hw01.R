library(ggplot2) 
library(reshape)
library(xtable)

# exercise 26
ex26 <- read.delim("data/ex01-26.dat", header = TRUE, sep = '\t')
plot <- ggplot(ex26, aes(x = reorder(cause, -deaths), y = deaths)) + 
  geom_bar(stat = "identity", fill="lightblue", color = "black") +
  labs(x = "Cause", y = "Deaths")

print(plot)
ggsave("ex26.eps", width = 5, height = 3)

# exercise 28
ex28 <- read.delim("data/ex01-28.dat", header = TRUE, sep = '\t')
plot <- ggplot(ex28, aes(x = age, y = percent)) + 
  geom_bar(stat = "identity", fill="lightblue", color = "black") +
  labs(x = "Age", y = "Movie Attendance")

ggsave("ex28.eps", width = 5, height = 3)
print(plot)

# exercise 30
ex30 <- read.delim("data/ex01-30.dat", header = TRUE, sep = '\t')
ex30$servings <- as.factor(ex30$servings)
plot <- ggplot(ex30, aes(x = servings, y = count)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Number of Subjects", y = "Servings per Day")
print(plot)

ggsave("ex30.eps", width = 5, height = 3)

cumsum(ex30$count)
sum(ex30$count)/2

# exercise 34
ex34 <- read.delim("data/ta01-04.dat", header = TRUE, sep = '\t')
ex34 <- ex34[order(-ex34$MDs),]
summary(ex34)

plot <- ggplot(ex34, aes(x = MDs)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth=50) +
  labs(x = "MDs per 100,000 people", y = "Number of States")
print(plot)
ggsave("ex34.eps", width = 5, height = 3)

plot <- ggplot(ex34[1:25,], aes(y = reorder(state, MDs), x = MDs)) + 
  geom_point() +
  labs(y = "State", x = "MDs per 100,000 people")
print(plot)

# exercise 37
ex37 <- read.delim("data/ex01-37.dat", header = TRUE, sep = '\t')
str(ex37)

plot <- ggplot(data = ex37, aes(x = study, fill = sex)) + 
  geom_histogram(color = "blue", binwidth = 50, position = "stack") +
  labs(x = "Study Hours", y = "Number of Students") 

print(plot)
ggsave("ex37.eps", width = 5, height = 3)

# exercise 39
ex39 <- read.delim("data/ex01-39.dat", header = TRUE, sep = '\t')
str(ex39)
ex39$rate <- apply(ex39, 1, function(row) as.integer(row["accidents"]) / as.integer(row["drivers"]))
ex39$pot <- factor(x = ex39$pot, levels = c("never", "1to10", "11to50", "51plus"))
ex39$pot
plot <- ggplot(data = ex39, aes(x = pot, y = rate)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Marijuana Use", y = "Accidents per Driver") 

print(plot)
ggsave("ex39.eps", width = 5, height = 3)

# exercise 41
ex41 <- read.delim("data/ex01-41.dat", header = TRUE, sep = '\t')
ex41long <- melt(ex41, id = "Year")
str(ex41long)

plot <- ggplot(ex41long, aes(x = Year, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_grey()
  labs(x = "Year", y = "Defects") 
print(plot)

ggsave("ex41.eps", width = 5, height = 3)

# exercise 44
ex44 <- read.delim("data/ex01-44.dat", header = TRUE, sep = '\t')
nrow(subset(ex44, attacks >= 10, select = c(attacks)))

plot <- ggplot(ex44, aes(x = year, y = attacks)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Year", y = "Attacks") 
print(plot)

ggsave("ex44Time.eps", width = 5, height = 3)

plot <- ggplot(ex44, aes(x = attacks)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 5) +
  labs(x = "Attacks", y = "Number of Years") 
print(plot)

ggsave("ex44Histogram.eps", width = 5, height = 3)

