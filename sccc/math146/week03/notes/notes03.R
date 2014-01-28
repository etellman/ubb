
# exercise 50
ex50 <- read.delim("ex03-50.dat", header = TRUE, sep = '\t')
str(ex50)
ex50

ex50.s <- round(summary(ex50$Rain))
sd(ex50$Rain)

round(qnorm(c(0.25, 0.5, 0.75), ex50.s["Mean"], sd(ex50$Rain)))
ex50.s

plot <- ggplot(ex50, aes(x = Rain)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 20) +
  labs(x = "Rain", y = "Years") +
  ggtitle("Rain with Bin Width 20")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week03/hw/figures/ex50_histogram_20.eps", 
       width = 4, height = 2.5)

plot <- ggplot(ex50, aes(x = 1, y = Rain)) + 
  geom_boxplot(color = "black", fill = "lightblue", outlier.shape = 21, outlier.size = 1.5) +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") +
  labs(x = "", y = "Rain") +
  coord_flip() +
  ggtitle("Rain Box Plot")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week03/hw/figures/ex50_box.eps", 
       width = 4, height = 2.5)

sink("~/Documents/U/ubb/sccc/math146/week03/hw/r.tex")
xtable(as.matrix(ex50.s))
sink()

ex50.s["Median"] - ex50.s["1st Qu."]
ex50.s["Median"] - ex50.s["3rd Qu."]

ex50.s["Median"] - ex50.s["Min."]
ex50.s["Median"] - ex50.s["Max."]

