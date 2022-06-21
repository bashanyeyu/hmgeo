Rescale = function(x, type=1) {
  # type=1 Positive indicators , type=2 Negative indicators 
  rng = range(x, na.rm = TRUE)
  if (type == 1) {
    (x - rng[1]) / (rng[2] - rng[1]) 
  } else {
    (rng[2] - x) / (rng[2] - rng[1])
  }
}
# Define the entropy function 
Entropy = function(x) {
  entropy=array(data = NA, dim = ncol(x),dimnames = NULL)
  
  j=1
  while (j<=ncol(x)) {
    value=0
    i=1
    while (i<=nrow(x)) {
      if (x[i,j]==0) {
        (value=value) 
      } else {
        (value=value+x[i,j]*log(x[i,j]))
      }
      i=i+1
    }
    entropy[j]=value*(-1/log(nrow(x))) 
    j=j+1
  }
  
  return(entropy)
}
Entropy_Weight = function(X, index) {
  #  Realize the calculation of each index with entropy weight method ( Column ） The weight of each data line and the score of each data line 
  # X For index data ,  One line represents a sample ,  Each column corresponds to an indicator 
  # index Indicating vector , Indicates whether the columns are positive indicators or negative indicators ,1 Indicates a positive indicator ,2 A negative indicator 
  # s Return to rows （ sample ） score ,w Return the weight of each column 
  
  pos = which(index == 1)
  neg = which(index != 1)
  
  #  Data normalization 
  X[,pos] = lapply(X[,pos], Rescale, type=1)
  X[,neg] = lapply(X[,neg], Rescale, type=2)
  #  Computation first j Two indicators , The first i The proportion of samples in this index p(i,j)           
  P = data.frame(lapply(X, function(x) x / sum(x)))
  
  e = Entropy(P)
  d = 1 - e         #  Computing information entropy redundancy 
  w = d / sum(d)   #  Calculate the weight vector 
  
  #  Calculate the sample score 
  s = as.vector(100 * as.matrix(X) %*% w)
  
  
  list(w=w,s=s)
  
}
