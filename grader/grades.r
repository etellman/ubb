grades.dir <- '~/Documents/U/ubb/asu/math205/grades'
setwd(grades.dir)

# returns the percentage given a number of points wrong and total number of points
percentage <- function(score, points) {
  actual <- points + score
  c(actual = actual, percentage = round(100 * (actual) / points))
}

possible <- 5 * length(c(25, 27:28, 32:36, 38, 41:43, 47))
percentage(-2, possible)

grades.file <- paste(grades.dir, 'grades.csv', sep = '/')
grades <- read.delim(grades.file, header = TRUE, strip.white = T, sep = ',', comment = '#')

df <- ddply(grades, .(id), summarize, 
      id = id, 
      hw = mean(c(hw01, hw02, hw03, hw04, hw05), na.rm = T),
      exam01 = exam01,
      overall = round(sum(0.2 * hw, 0.8 * exam01))
)

mean(df$overall)
mean(mean(c(100, 100, 97, 100)), 100)

grades.m <- melt(grades, id = "id")
median(grades.m$value, na.rm = T)
round(mean(grades.m$value, na.rm = T))

hw <- grades.m[grep("hw", grades.m$variable),]
exam01 <- grades.m[grep("exam", grades.m$variable),]

hw <- dcast(hw, id ~ variable)

ddply(hw.m, .(id), fun = function(df) max(value))


plot <- ggplot(grades.m, aes(x = value)) + 
  geom_histogram(binwidth = 10, origin = 40.1, color = "black", fill = "lightblue") +
  labs(x = "Grade", y = "Students")
print(plot)

77/80
16*5
