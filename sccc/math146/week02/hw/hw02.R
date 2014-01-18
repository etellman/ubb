library(ggplot2) 
library(reshape)
library(xtable)
library(plyr)

# exercise 26
ex31 <- read.delim("ex02-31.dat", header = TRUE, sep = '\t')
plot <- ggplot(ex31, aes(x = reorder(cause, -deaths), y = deaths)) + 
  geom_bar(stat = "identity", fill="lightblue", color = "black") +
  labs(x = "Cause", y = "Deaths")

print(plot)
ggsave("ex31.eps", width = 5, height = 3)


