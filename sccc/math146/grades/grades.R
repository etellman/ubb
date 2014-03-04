# returns the number of points for a list of problems
points <- function(problems) {
  5 * length(problems)
}

# returns the percentage given a number of points wrong and total number of points
percentage <- function(score, points) {
  actual <- points - score
  c(actual = actual, percentage = round(100 * (actual) / points))
}

possible <- points(c(27:28, 30:34, 37:38, 42, 44:45, 47:48, 51, 53))
percentage(7, possible)

grades.dir <- "~/Documents/U/ubb/sccc/math146/grades"
grades.file <- paste(grades.dir, 'grades.csv', sep = '/')
grades <- read.delim(grades.file, header = TRUE, strip.white = T, sep = ',')

grades.m <- melt(grades, id = "id")
str(grades.m)

round(summary(grades.m$value, na.rm = T))

plot <- ggplot(grades.m, aes(x = value)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(x = "Grade", y = "Students") +
  ggtitle("Homework Grades")
print(plot)
