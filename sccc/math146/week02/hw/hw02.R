root.dir <- "~/Documents/U/ubb/sccc/math146"
hw.dir <- paste(root.dir, "week02/hw", sep = "/")
figures.dir <- paste(hw.dir, "figures", sep = "/")

data.dir <- paste(root.dir, "data/bps/PC-Text/ch02", sep = "/")
setwd(data.dir)
 
# exercise 29
ex29 <- read.delim("ta02-01.dat", header = TRUE, sep = '\t')
str(ex29)

plot <- ggplot(ex29, aes(x = reorder(Variety, Length), y = Length)) + 
  geom_boxplot(color = "black", fill = "lightblue", outlier.shape = 21, outlier.size = 1.5) +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") +
  labs(x = "Variety", y = "Length") +
  ggtitle("Flower Lengths")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week02/hw/figures/ex29.eps", width = 4, height = 2.5)

df <- ddply(ex29, "Variety", function(df) summary(df$Length))
sink("~/Documents/U/ubb/sccc/math146/week02/hw/r.tex")
xtable(df, digits = 2)
sink()

# histogram
plot <- ggplot(subset(ex29, Variety == "yellow"), aes(x = Length)) + 
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(x = "Length", y = "Count") +
  ggtitle("Flower Lengths")
print(plot)

# exercise 32
ex32 <- read.delim("../ch01/ex01-38.dat", header = TRUE, sep = '\t')
str(ex32)

mean.and.sd <- function(df) {
  round(c(mean = mean(df$Study), sd = sd(df$Study)), digits = 2)
}

ddply(ex32, "Sex", mean.and.sd)
ddply(subset(ex32, Study < 300), "Sex", mean.and.sd)

# exercise 37
ex37 <- read.delim("../ch01/ta01-01.dat", header = TRUE, sep = '\t')
str(ex37)

big.states <- subset(ex37, State %in% c("California", "NewYork", "Texas", "Florida"))

sink("~/Documents/U/ubb/sccc/math146/week02/hw/r.tex")
xtable(big.states, digits = 1)
sink()

mean(ex37$PctFor)
mean(big.states$PctFor)

# exercise 43
ex43 <- read.delim("ta02-02.dat", header = TRUE, sep = '\t')
ex43.summary <- as.data.frame(as.matrix(summary(ex43$Salary)))

ex43 <- cbind(ex43, rank = rank(-ex43$Salary))

sum(subset(ex43, rank <= 3, select = "Salary")) / sum(ex43$Salary)

sink("~/Documents/U/ubb/sccc/math146/week02/hw/r.tex")
xtable(ex43.summary, digits = 0)
sink()

plot <- ggplot(subset(ex43), aes(x = Salary / 10^6)) + 
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(x = "Salary (millions)", y = "Count") +
  ggtitle("Red Sox Salaries")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week02/hw/figures/ex43.eps", width = 4, height = 2.5)

# exercise 44
ex44 <- read.delim("ex02-44.dat", header = TRUE, sep = '\t')
str(ex44)

ex44.summary <- as.data.frame(as.matrix(summary(ex44$Return)))
ex44.summary

hw.dir
sink(paste(hw.dir, "r.tex", sep = "/"))
  xtable(ex44.summary, digits = 2)
sink()

plot <- ggplot(ex44, aes(x = Return)) + 
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(x = "Return", y = "Years") +
  ggtitle("Exercise 44")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week02/hw/figures/ex44.eps", width = 4, height = 2.5)

# exercise 45
ex45 <- read.delim("ta02-03.dat", header = TRUE, sep = '\t')
str(ex45)

ex45.summary <- ddply(ex45, "Odor", function(df) summary(df$Euros))
ex45.summary <- subset(ex45.summary, select = c("Odor", "Median", "Mean"))

sink("~/Documents/U/ubb/sccc/math146/week02/hw/r.tex")
xtable(ex45.summary, digits = 0)
sink()

plot <- ggplot(ex45, aes(x = reorder(Odor, Euros), y = Euros)) + 
  geom_boxplot(color = "black", fill = "lightblue", outlier.shape = 21, outlier.size = 1.5) +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") +
  labs(x = "Odor", y = "Euros") +
  ggtitle("Restaurant Odors")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week02/hw/figures/ex45.eps", width = 4, height = 2.5)

# exercise 50
ex50 <- read.delim("../ch01/ta01-06.dat", header = TRUE, sep = '\t')
ex50 <- cbind(ex50, rank = rank(-ex50$CO2))

subset(ex50, rank <= 3)

ex50.summary <- ddply(ex50, c(), function(df) summary(df$CO2))

sink("~/Documents/U/ubb/sccc/math146/week02/hw/r.tex")
xtable( t(ex50.summary) , digits = 0)
sink()

plot <- ggplot(ex50, aes(x = 1, y = CO2)) + 
  geom_boxplot(color = "black", fill = "lightblue", outlier.shape = 21, outlier.size = 1.5) +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") +
  labs(x = "Country", y = "CO2") +
  ggtitle("CO2 Emissions")
print(plot)

plot <- ggplot(subset(ex50), aes(x = CO2)) + 
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(x = "CO2", y = "Countries") +
  ggtitle("CO2 Emissions")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week02/hw/figures/ex50.eps", width = 4, height = 2.5)

summary(c(4,2,8,3,9))
