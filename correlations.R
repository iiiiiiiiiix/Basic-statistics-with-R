correlations <- function(data)
{
  correlation_matrix <- round(cor(data, method = "pearson"), 3)
  diag(correlation_matrix) <- 0
  correlation_matrix[lower.tri(correlation_matrix)] <- 0
  fm <- as.data.frame(as.table(correlation_matrix))
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  sorted <- fm[order(abs(fm$Correlation),decreasing=T),]
  i = 1
  while(sorted[i,3] != 0){
    i = i+1
  }
  return(sorted[1:i-1,])
}
