rm (list = ls())

install.packages("genalg")
library("genalg")
#evalFunc = function(x = c())
#{
#  r = abs(x[1]*x[1] + x[2]*x[2] + x[3])
 # return(r)
#}


evalFunc = function(a = c())
{
  x = a[1]
  y = a[2]
  z = a[3]
  r = c(0,0,0)
  r[1] = x+y+2*z
  r[2] = x+2*y+z
  r[3] = 2*x+3*y-3*z
  #rx = abs(r[1]-8 + r[2]-10 + r[3]-14)
  x = abs(r[1]-8) + abs(r[2]-10) + abs(r[3]-14)
  #x = abs(r[1]) + abs(r[2]) + abs(r[3])
  #cc = abs(r[1] + r[2] + r[3])
  return(x)
}
iter = 1000
GAmodel = rbga (
  stringMin = c(-10,-10,-10),
  stringMax = c(10,10,10),
  popSize = 50,
  iters = iter,
  mutationChance = 0.001,
  elitism =T,
  evalFunc = evalFunc
)
print(GAmodel)

evalFunc(GAmodel$population[1,])

GAmodel$population[1,]