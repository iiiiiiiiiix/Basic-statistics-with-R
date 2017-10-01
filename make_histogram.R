make_histogram <- function(data, variable)
{
  d <- data[,c(1,variable)]
  values <- as.vector(as.matrix(d[,2]))
  species <- data$species
  hist <- ggplot(d, aes(values, fill = species, colour = species)) +
    geom_histogram(alpha = 0.7, bins = 14) + 
    ggtitle(colnames(d)[2])
  print(hist)
}