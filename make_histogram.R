make_histogram <- function(data, variable)
{
  d <- data[,c(1,variable)]
  values <- as.vector(as.matrix(d[,2]))
  hist <- ggplot(d, aes(values, fill = data$species, colour = data$species)) +
    geom_histogram(alpha = 0.7, bins = 14) + ggtitle(colnames(d)[2])
  print(hist)
}