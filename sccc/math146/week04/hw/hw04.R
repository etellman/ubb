
# exercise 26
ex26 <- read.delim("ex04-26.dat", header = TRUE, sep = '\t')
str(ex26)

plot <- ggplot(ex26, aes(x = Women, y = Men)) + 
  geom_point() +
  labs(x = "Women", y = "Men") +
  # scale_x_continuous(limits = c(-10, 30)) +
  ggtitle("Exercise 26: Date Heights")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week04/hw/figures/ex26.eps", width = 4, height = 2.5)

with(ex26, round(cor(Women, Men), 4))

# exercise 27
ex27 <- read.delim("ex04-27.dat", header = TRUE, sep = '\t')
str(ex27)

plot <- ggplot(ex27, aes(x = Price, y = Deforest)) + 
  geom_point() +
  labs(x = "Coffee Price", y = "Forest Lost") +
  # scale_x_continuous(limits = c(-10, 30)) +
  ggtitle("Exercise 27: Coffee and Deforestation")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week04/hw/figures/ex27.eps", width = 4, height = 2.5)

with(ex27, round(cor(Price, Deforest), 4))

# exercise 28
ex28 <- read.delim("ex04-28.dat", header = TRUE, sep = '\t')
str(ex28)

plot <- ggplot(ex28, aes(x = Newbirds, y = Pctret)) + 
  geom_point() +
  labs(x = "Percent Returning", y = "New Birds") +
  ggtitle("Exercise 28: Birds")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week04/hw/figures/ex28.eps", width = 4, height = 2.5)

with(ex28, round(cor(Pctret, Newbirds), 4))
