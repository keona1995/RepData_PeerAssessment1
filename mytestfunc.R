fillNAsWithMeans <- function(df1, df2) {
  i <- 1
  while(i <= length(df1[,1]))
  {
    if(is.na(df[i,1])) {
      m <-  i%%288
      if(m==0)
        m <- 288
      df1[i,1] <- df2[m,1]
    }
    i <- i+1
  }
  df1
}