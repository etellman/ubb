grades.dir <- '~/Documents/U/ubb/asu/math205/grades'
setwd(grades.dir)

# returns the percentage given a number of points wrong and total number of points
percentage <- function(score, points) {
  actual <- points + score
  c(actual = actual, percentage = round(100 * (actual) / points))
}

possible <- 5 * length(c(23:26, 29:35, 39:41, 44:45))
percentage(-3, possible)

grades.file <- paste(grades.dir, 'grades.csv', sep = '/')
grades <- read.delim(grades.file, header = TRUE, strip.white = T, sep = ',', comment = '#')

str(grades)

grades.m <- melt(grades, id = "student")
median(grades.m$value, na.rm = T)
round(mean(grades.m$value, na.rm = T))

hw <- grades.m[grep("hw", grades.m$variable),]
exam01 <- grades.m[grep("exam", grades.m$variable),]

plot <- ggplot(grades.m, aes(x = value)) + 
  geom_histogram(binwidth = 10, origin = 40.1, color = "black", fill = "lightblue") +
  labs(x = "Grade", y = "Students")
print(plot)

77/80
16*5
