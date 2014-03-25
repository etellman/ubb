
root.dir <- '~/Documents/U/ubb/sccc/math146'
notes.dir <- paste(root.dir, 'week10/notes', sep = '/')
figures.dir <- paste(notes.dir, 'figures', sep = '/')

sum(dbinom(6:10, 10, 17/70))

53/70

?dbinom

placebo = c(0,4,5,4,0,5, 0,0,2,0,2,3, 2,0,5,3,2,0, 3,5,0,0,4,0,
            0,5,0,5,5,4, 4,2,0,4,0,5, 5,0,1,3,5,3, 0,5,3,5,0,5,
            5,3,5,0,0,0, 4,0,4,0,5 )

plot <- ggplot(data = data.frame(placebo), aes(x = placebo)) +
  geom_histogram(breaks = 0:6, binwidth = 1, fill = 'lightblue', color = 'black') +
  labs(x = "Headaches Relieved")
print(plot)

ggsave(paste(figures.dir, 'placebo.pdf'), plot, width = 5, height = 3)
