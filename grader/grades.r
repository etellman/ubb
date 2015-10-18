grades.dir <- '~/Documents/U/ubb/asu/math205/grades'
setwd(grades.dir)

# returns the percentage given a number of points wrong and total number of points
percentage <- function(score, points) {
  actual <- points + score
  c(actual = actual, percentage = round(100 * (actual) / points))
}

possible <- 5 * length(c(34, 36:38, 40, 42:46, 48:53, 55, 57))
percentage(-12, possible)

# overall course grades

# assigns a letter grade for a percentage
#
# percentage - course percentage
letter.grade <- function(percentage) {
  if (percentage >= 90) {
    'A'
  } else if (percentage >= 80) {
    'B'
  } else if (percentage >= 70) {
    'C'
  } else if (percentage >= 60) {
    'D'
  } else {
    'F'
  }
}

grades.file <- paste(grades.dir, 'grades.csv', sep = '/')
grades <- read.delim(grades.file, header = TRUE, strip.white = T, sep = ',', comment = '#')

grades.m <- melt(grades, id = "id")
grades.summary <- ddply(grades.m, .(id), summarize, 
      id = first(id),
      exam01 = value[variable == 'exam01'],
      exam02 = value[variable == 'exam02'],
      homework = round(mean(value[grep('hw', variable)])),
      overall = round(sum(0.2 * homework + 0.4 * exam01 + 0.4 * exam02)),
      grade = letter.grade(overall)
)

# grades ordered by overall score, descending
grades.summary[order(-grades.summary$overall),]

plot <- ggplot(grades.summary, aes(x = overall)) + 
  geom_histogram(binwidth = 10, origin = 40.1, color = "black", fill = "lightblue") +
  labs(x = "Grade", y = "Students")
print(plot)

