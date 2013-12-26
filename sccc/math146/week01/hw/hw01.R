library(ggplot2) 
library(reshape)
library(xtable)

# library(gcookbook)

# exercise 26
ex26 <- read.delim("data/ex01-26.dat", header = TRUE, sep = '\t')
plot <- ggplot(ex26, aes(x = reorder(cause, -deaths), y = deaths)) + 
  geom_bar(stat = "identity") +
  labs(x = "Cause", y = "Deaths")
  scale_fill_grey()

print(plot)
ggsave("ex26.eps")

