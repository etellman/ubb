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

by.color <- ddply(animals, .(color), summarise,  
  color = color, species = species, value = count / sum(count))

by.species <- ddply(animals, .(species), summarise,  
  species = species, color = color, value = count / sum(count))

by.species

by.color <- ddply(animals, 'color', summarise, species = species, 
  color = color, value = count / sum(count))

percentages <- cast(by.species.and.color, species ~ color, 
                    margins = T, fun.aggregate = sum)

percentages

sink('r.tex')
  xtable(by.species, digits = 4)
  xtable(by.color, digits = 4)
sink()

0.6333 * 0.7368 

0.7 * 250000

0.3 * 0.6 * 0.9 * 25000000 + 0.7 * 250000

1 - 0.7 - 0.3 * 0.6 * 0.9 
