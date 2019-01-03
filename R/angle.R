
angle=function(x,y){
  numerator=sum(x*y)
  denominator=sqrt(sum(x*x))*sqrt(sum(y*y))
  return( acos(numerator/denominator))

}

angle(xx,yy)
