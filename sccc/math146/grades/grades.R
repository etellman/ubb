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

grades.dir <- '~/Documents/U/ubb/sccc/math146/grades'
grades.file <- paste(grades.dir, 'grades.csv', sep = '/')
grades <- read.delim(grades.file, header = TRUE, strip.white = T, sep = ',')

grades.m <- melt(grades, id = "id")

grades.m

round(summary(grades.m$value, na.rm = T))

hw5 <- subset(grades.m, variable == 'hw5', select = c('id', 'value'))
hw5$id

data.frame(hw5$id, round(scale(hw5$value), 2))

plot <- ggplot(grades.m, aes(x = value)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(x = "Grade", y = "Students")
print(plot)
