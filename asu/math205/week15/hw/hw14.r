
root.dir <- "~/Documents/U/ubb/sccc/math146"

hw.dir <- paste(root.dir, "week16/hw", sep = "/")
data.dir <- paste(root.dir, "data/bps/PC-Text/ch15", sep = "/")
setwd(data.dir)
￼
# exercise 36
ex36 <- read.csv("ex15-36.dat", sep = '\t')
str(ex36)

plot <- ggplot(data = ex36, aes(x = Year, y = Change)) + 
  geom_line() +
  labs(x = "Year", y = "Change") 
print(plot)

plot <- ggplot(data = ex36, aes(x = Change)) + 
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(x = "Year") +
  theme(axis.title.y = element_blank())
print(plot)

file <- paste(hw.dir, "ex50.pdf", sep = "/")
ggsave(filename = file, plot = plot, width = 4, height = 2.5)

# exercise 41
ex41 = c(6.47, 7.51, 10.1, 13.63, 9.91)
mean(ex41)

s <- 2/sqrt(5)

z <- (mean(ex41) - 8)/s

2 * (1 - pnorm(z))
