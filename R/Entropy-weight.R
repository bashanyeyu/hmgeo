entropyW=function(data){
  warning("we use 'log10 and nan=0'")
  data=as.data.frame(data)
  sumcol=apply(data,2,sum)
  presidents=data.frame(matrix(NA,nrow(data),ncol(data)))
   for (n in 1:length(sumcol)) {
     presidents[,n]=data[,n]/sumcol[n]
   }
  k=1/log10(nrow(data))
  log10sc=presidents*log10(presidents)
  log10sc[is.na(log10sc)] =0

  info=apply(log10sc,2,sum)*(-k)
  res=(1-info)/sum(1-info)
  res
}


