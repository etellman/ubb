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

possible <- points(c(19:24, 27, 29:31))
percentage(6, possible)

grades.file <- paste(grades.dir, 'grades.csv', sep = '/')
grades <- read.delim(grades.file, header = TRUE, strip.white = T, sep = ',')

grades.m <- melt(grades, id = "id")

exam1.summary <- summary(subset(grades, select = 'exam1'), na.rm = T)
exam1.summary

data.frame(hw5$id, round(scale(hw5$value), 2))

plot <- ggplot(grades, aes(x = exam1)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(x = "Grade", y = "Students")
print(plot)

