
# exercise 25
ex25.range <- function(mean, sd) {
  c(mean - 2 * sd, mean + 2 * sd)
}

ex25.range(373, 67)

# exercise 27
pnorm(70, 100, 15)

# exercise 28
plot <- ggplot(data.frame(x = c(-25, 525)), aes(x = x)) +
  stat_function(fun = function(x) dnorm(x, 250, 176)) +
  stat_function(fun = limit.domain(function(x) dnorm(x, 250, 176), -25, 25), geom = "area", fill = "lightblue") +
  stat_function(fun = limit.domain(function(x) dnorm(x, 250, 176), 475, 525), geom = "area", fill = "lightblue") +
  ggtitle("Exercise 28 (c)")
print(plot)

qgsave("~/Documents/U/ubb/sccc/math146/week03/hw/figures/ex28c.eps", width = 4, height = 2.5)

# exercise 30
qnorm(0.1, 100, 50)
pnorm(5, 5.4, 0.54)
pnorm((5 - 5.4)/0.54)

z <- (750 - 499)/110
round(z, digits = 4)

round(1 - pnorm(750, 499, 110), digits = 4)
round(1 - pnorm(2.2818), digits = 4)

pnorm(500, 250, 175.78)
round(qnorm(c(0.1, 0.9), 250, 175.78), digits = 2)

round((750 - 533)/116, digits = 4)

(475-250)/1.28
