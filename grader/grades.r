grades.dir <- '~/Documents/U/ubb/algebra/grades'

# returns the number of points for a list of problems
points <- function(problems) {
  5 * length(problems)
}

# returns the percentage given a number of points wrong and total number of points
percentage <- function(score, points) {
  actual <- points + score
  c(actual = actual, percentage = round(100 * (actual) / points))
}

possible <- points(c(1:15, 19:34, 40:44, 51:56, 58, 59, 62))
percentage(-24, possible)

possible

grades.file <- paste(grades.dir, 'grades.csv', sep = '/')
grades <- read.delim(grades.file, header = TRUE, strip.white = T, sep = ',')

grades.m <- melt(grades, id = "student")
median(grades.m$value, na.rm = T)
round(mean(grades.m$value, na.rm = T))

hw <- grades.m[grep("hw", grades.m$variable),]
exam01 <- grades.m[grep("exam", grades.m$variable),]

plot <- ggplot(grades.m, aes(x = value)) + 
  geom_histogram(binwidth = 5, origin = 40.1, color = "black", fill = "lightblue") +
  labs(x = "Grade", y = "Students")
print(plot)


