root.dir <- '~/Documents/U/ubb/sccc/math146'
nfl.dir <- paste(root.dir, 'data', 'nfl', sep = '/')
nba.dir <- paste(root.dir, 'data', 'nfl', sep = '/')
exam.dir <- paste(root.dir, 'week08', sep = '/')
figures.dir <- paste(exam.dir, 'figures', sep = '/')

# NFL
setwd(exam.dir)

team <- read.delim('TEAM.csv', header = TRUE, sep = ',', strip.white = TRUE)
games <- read.delim('GAMES.csv', header = TRUE, sep = ',', strip.white = TRUE)

team.sample <- team[sample(nrow(team), 20),]

sea.2012 <- subset(merge(team, games, by = 'GID'), TNAME == 'SEA' & SEAS == 2012)
plot <- ggplot(data = sea.2012, aes(x = PTS)) + 
  geom_histogram(fill = 'lightblue', color = 'black', binwidth = 5) +
  labs(x = 'Final Score', y = 'Games')
print(plot)


sink(paste(exam.dir, 'r.tex', sep = '/'))
  sort(team.sample$PTS)
sink()

# # scatter plots
# exams <- rbind(
#   data.frame(student = 'A', midterm = 20, final = 20),
#   data.frame(student = 'B', midterm = 70, final = 70),
#   data.frame(student = 'C', midterm = 40, final = 100),
#   data.frame(student = 'D', midterm = 70, final = 30),
#   data.frame(student = 'E', midterm = 90, final = 10),
#   data.frame(student = 'F', midterm = 45, final = 48),
#   data.frame(student = 'G', midterm = 30, final = 55),
#   data.frame(student = 'H', midterm = 30, final = 70),
#   data.frame(student = 'I', midterm = 90, final = 20),
#   data.frame(student = 'J', midterm = 60, final = 25)
# )

exams <- rbind(
  data.frame(student = 'A', midterm = 65, final = 80),
  data.frame(student = 'B', midterm = 51, final = 55),
  data.frame(student = 'C', midterm = 75, final = 70),
  data.frame(student = 'D', midterm = 66, final = 95),
  data.frame(student = 'E', midterm = 40, final = 40),
  data.frame(student = 'F', midterm = 61, final = 84),
  data.frame(student = 'G', midterm = 72, final = 80),
  data.frame(student = 'H', midterm = 85, final = 91),
  data.frame(student = 'I', midterm = 35, final = 53),
  data.frame(student = 'J', midterm = 64, final = 71),
  data.frame(student = 'K', midterm = 53, final = 71),
  data.frame(student = 'L', midterm = 47, final = 62)
)

more.than.60 <- subset(exams, midterm > 60, select = 'final')
mean(more.than.60$final)

exams.melted <- melt(exams, id = 'student')

midterms <- subset(exams.melted, variable == 'midterm')
median(midterms$value)

exams.cast <- cast(exams.melted, student + variable ~ ., )
exams.box <- rename(exams.cast, c(variable = 'exam', '(all)' = 'score'))


exams.box

plot <- ggplot(exams.box, aes(x = exam, y = score)) +
  geom_boxplot() + 
  labs(x = 'Exam', y = 'Score')
print(plot)

ggsave(paste(figures.dir, 'exams_box.pdf', sep = '/'), height = 3, width = 5)

plot <- ggplot(exams, aes(x = midterm, y = final)) +
  geom_point() + 
  labs(x = 'Midterm', y = 'Final')

print(plot)
ggsave(paste(figures.dir, 'exams_scatter.pdf', sep = '/'), height = 4, width = 6)

with(exams, data.frame(cor(midterm, final), mean(midterm), mean(final)))


