addInequalColumn = function(x,y,z,w)
{
  i <- 1
  while(i<length(x$sgPartido)+1)
  {
    j <- 1
    while(j < length(y$sgPartido)+1)
    {
      if(x$sgPartido[i] == y$sgPartido[j])
      {
        z[[w]][i] = y$vlrLiquido[j]
        break
      }
      z[[w]][i] = 0
      j<-j+1
    }
    i<-i+1
  }
  return(z)
}
