root.dir <- "~/Documents/U/ubb/sccc/math146"
notes.dir <- paste(root.dir, "week04/notes", sep = "/")
figures.dir <- paste(notes.dir, "figures", sep = "/")

x = c(91, 81, 86, 83, 85, 85, 85, 84, 91, 91, 91, 85, 85, 87, 90)
y = c(85, 83, 85, 84, 85, 84, 89, 83, 82, 82, 82, 85, 85, 85, 85)

df <- data.frame(x, y, scale(x), scale(y))
cor(df$x, df$y)

plot <- ggplot(df, aes(x = x, y = y)) + 
  geom_point() +
  labs(x = "x", y = "y") +
  ggtitle("Correlation")
print(plot)

sink(paste(notes.dir, "r.tex", sep = "/"))
  xtable(df)
sink()
