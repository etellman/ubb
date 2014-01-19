library(ggplot2) 
library(reshape)
library(xtable)
library(plyr)

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

# exercise 31
ex32 <- read.delim("../ch01/ex01-38.dat", header = TRUE, sep = '\t')
str(ex32)

mean.and.sd <- function(df) {
  round(c(mean = mean(df$Study), sd = sd(df$Study)), digits = 2)
}

ddply(ex32, "Sex", mean.and.sd)
ddply(subset(ex32, Study < 300), "Sex", mean.and.sd)
