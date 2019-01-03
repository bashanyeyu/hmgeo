

advanced_indus=function(x){

x=as.data.frame(x)
library(hmgeo)



angles=data.frame(matrix(0, ncol = 1, nrow = nrow(x)))
colnames(angles)='Advanced'
rownames(angles)=rownames(x)
all1=c(1,0,0)
all2=c(0,1,0)
all3=c(0,0,1)
for(i in 1:nrow(x)){


  angles[i,]=angle(x[i,1:3],all1)+angle(x[i,1:3],all2)+angle(x[i,1:3],all3)

}

}
