root.dir <- '~/Documents/U/ubb/sccc/math146'
notes.dir <- paste(root.dir, "week13/notes", sep = "/")
setwd(notes.dir)

animals <- rbind(
  data.frame(color = 'white', species = 'dog', count = 30),
  data.frame(color = 'black', species = 'dog', count = 70),
  data.frame(color = 'white', species = 'cat', count = 25),
  data.frame(color = 'black', species = 'cat', count = 25)
)

cast(animals, species ~ color, margins = T, fun.aggregate = sum)

by.species.and.color <- ddply(animals, .(species, color), summarise, 
  species = species, color = color, value = count / 150)

print(by.species.and.color)

by.color <- ddply(animals, 'color', summarise, species = species, 
  color = color, value = count / sum(count))

?cast

cast(by.species.and.color, species ~ color, margins = 'grand_col', 
  fun.aggregate = sum)

sink('r.tex')
  xtable(cast(by.species.and.color, species ~ color), digits = 4, 
         grand_row = T)
sink()
