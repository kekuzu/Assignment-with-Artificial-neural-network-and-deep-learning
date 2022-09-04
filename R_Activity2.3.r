install.packages("neuralnet")
library("neuralnet")
??neuralnet

AND = c(0,0,0,1)
truthtable = expand.grid(c(0,1), c(0,1))
AND.data <- data.frame(truthtable, AND)
print(AND.data)

net <- neuralnet(AND~Var1+Var2,AND.data,hidden=0,rep=1)
print(net)
plot(net)

var1 = c(0,1,0,1)
var2 = c(0,0,1,1)

########## AND #############
OR = c(0,1,1,1)
truthtable = expand.grid(c(0,1), c(0,1))
OR.data <- data.frame(truthtable, OR)
print(OR.data)

net <- neuralnet(OR~Var1+Var2,OR.data,hidden=0,rep=1)


var1 = c(0,1,0,1)
var2 = c(0,0,1,1)

########## OR #############
NOR = c(1,0,0,0)
truthtable = expand.grid(c(0,1), c(0,1))
NOR.data <- data.frame(truthtable, NOR)
print(NOR.data)

net <- neuralnet(NOR~Var1+Var2,NOR.data,hidden=0,rep=1)


var1 = c(0,1,0,1)
var2 = c(0,0,1,1)

########## NOR #############
NAND = c(1,1,1,0)
truthtable = expand.grid(c(0,1), c(0,1))
NAND.data <- data.frame(truthtable, NAND)
print(NAND.data)

net <- neuralnet(NAND~Var1+Var2,NAND.data,hidden=0,rep=1)


var1 = c(0,1,0,1)
var2 = c(0,0,1,1)

########## NAND #############
XOR = c(0,1,1,0)
truthtable = expand.grid(c(0,1), c(0,1))
XOR.data <- data.frame(truthtable, XOR)
print(XOR.data)

net <- neuralnet(XOR~Var1+Var2,XOR.data,hidden=0,rep=1)


var1 = c(0,1,0,1)
var2 = c(0,0,1,1)

########## XOR #############

print(net)
plot(net)

saveRDS(net,'./model.rds')
model <-readRDS('./model.rds')

datatest = data.frame(var1,var2)
pred <-predict(net,datatest)
pred

logistic = function(x)
{
  1/(1 + exp(-x))
}
n = (net$result.matrix[4]+
     net$result.matrix[5]*var1 +
     net$result.matrix[6]*var2)
logistic(n)



??saveRDS
saveRDS(net, file = "/Users/kkeexx/R/Activity_AND_OR.net")
