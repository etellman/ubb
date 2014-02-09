# returns the number of points for a list of problems
points <- function(problems) {
  5 * length(problems)
}

# returns the percentage given a number of points wrong and total number of points
percentage <- function(score, points) {
  actual <- points - score
  c(actual = actual, percentage = round(100 * (actual) / points))
}

possible <- points(c(25:27, 29:30, 37:39, 32, 41:43, 45, 50:51))
percentage(5, possible)

setwd("~/Documents/U/ubb/sccc/math146/grades")
grades <- read.delim("grades.csv", header = TRUE, strip.white = T, sep = ',')

grades.m <- melt(grades, id = "id")
str(grades.m)

round(summary(grades.m$value, na.rm = T))

plot <- ggplot(grades.m, aes(x = value)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(x = "Grade", y = "Students") +
  ggtitle("Homework Grades")
print(plot)
