grades.dir <- '~/Documents/U/ubb/sccc/math146/grades'

# returns the number of points for a list of problems
points <- function(problems) {
  5 * length(problems)
}

# returns the percentage given a number of points wrong and total number of points
percentage <- function(score, points) {
  actual <- points - score
  c(actual = actual, percentage = round(100 * (actual) / points))
}

possible <- points(c(25, 27:28, 32:36, 38, 41:43, 47))
percentage(3, possible)

grades.file <- paste(grades.dir, 'grades.csv', sep = '/')
grades <- read.delim(grades.file, header = TRUE, strip.white = T, sep = ',')

grades.m <- melt(grades, id = "id")

sd(grades.m$value, na.rm = T)

plot <- ggplot(grades.m, aes(x = value)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(x = "Grade", y = "Students")
print(plot)

