grades.dir <- '~/Documents/U/ubb/sccc/calculus/grades'

# returns the number of points for a list of problems
points <- function(problems) {
  5 * length(problems)
}

# returns the percentage given a number of points wrong and total number of points
percentage <- function(score, points) {
  actual <- points - score
  c(actual = actual, percentage = round(100 * (actual) / points))
}

possible <- points(c(3:10, 13:14, 17:18, 21:22, 25:27, 29, 31:33, 41,43, 45, 49, 58))
percentage(23, possible)

grades.file <- paste(grades.dir, 'grades.csv', sep = '/')
grades <- read.delim(grades.file, header = TRUE, strip.white = T, sep = ',')

grades.m <- melt(grades, id = "id")
median(grades.m$value, na.rm = T)
round(mean(grades.m$value, na.rm = T))

hw <- grades.m[grep("hw", grades.m$variable),]
exam01 <- grades.m[grep("exam", grades.m$variable),]

exam01.mean <- mean(exam01$value, na.rm = T)
hw.mean <- mean(hw$value, na.rm = T)

0.8 * exam01.mean + 0.2 * hw.mean

plot <- ggplot(grades.m, aes(x = value)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(x = "Grade", y = "Students")
print(plot)

