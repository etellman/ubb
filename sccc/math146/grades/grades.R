# returns the number of points for a list of problems
points <- function(problems) {
  5 * length(problems)
}

# returns the percentage given a number of points wrong and total number of points
percentage <- function(score, points) {
  round(100 * (points - score) / points)
}

possible <- points(c(23:26, 29:35, 39:41, 44:45))
percentage(3, possible)

grades <- read.delim("grades.csv", header = TRUE, sep = ',')

sd(grades$hw1)

plot <- ggplot(grades, aes(x = hw1)) + 
  geom_histogram(binwidth = 2, color = "black", fill = "lightblue") +
  labs(x = "Grade", y = "Students") +
  ggtitle("Homework Grades")
print(plot)
