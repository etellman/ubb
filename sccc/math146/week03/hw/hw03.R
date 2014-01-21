
# exercise 25
ex25.range <- function(mean, sd) {
  c(mean - 2 * sd, mean + 2 * sd)
}

ex25.range(373, 67)

# exercise 27
pnorm(70, 100, 15)

# exercise 28
plot <- ggplot(data.frame(x = c(-3, 3)), aes(x = x)) +
  stat_function(fun = dnorm) +
  stat_function(fun = limit.domain(dnorm, 1.77, 3), geom = "area", fill = "lightblue") +
  ggtitle("Exercise 28 (c)")
print(plot)

qgsave("~/Documents/U/ubb/sccc/math146/week03/hw/figures/ex28c.eps", width = 4, height = 2.5)

# exercise 29
qnorm(0.1, 100, 50)
pnorm(.8416212)

