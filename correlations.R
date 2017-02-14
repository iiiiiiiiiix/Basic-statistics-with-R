correlations <- function(data)
{
  correlation_matrix <- round(cor(data, method = "pearson"),2)
  diag(correlation_matrix) <- 0
  correlation_matrix[lower.tri(correlation_matrix)] <- 0
  fm <- as.data.frame(as.table(correlation_matrix))
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  return(fm[order(abs(fm$Correlation),decreasing=T),])
}