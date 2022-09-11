library("neuralnet")
radius = 1:4
height = 1:4
volume = pi*radius*radius*height 
surface = 2*pi*radius*height 
datatrain = data.frame(radius,height,
                       volume,
                       surface)
datatrain

model <- neuralnet( volume+surface~radius+height, 
                    datatrain,
                    hidden=10, ##<--Change here 
                    rep = 1,
                    linear.output = TRUE)
print(model) 
plot(model) 
print(model$net.result)

########################################## #ACTIVITY 5-2 1:N PERCEPTRON
#ANALOG TO DIGITAL CONVERTER WITH #PERCEPTRON ##########################################
library("neuralnet")
volt = c(1,2,3,4)
b0 = c(1,0,0,0)
b1 = c(0,1,0,0)
b2 = c(0,0,1,0)
b3 = c(0,0,0,1)
datatrain = data.frame(volt,b0,b1,b2,b3) 
datatrain

model <- neuralnet( b0+b1+b2+b3~volt,
                    datatrain,
                    hidden=6, ##<--Change here
                    rep = 1,
                    linear.output = TRUE)
print(model) 
plot(model) 
print(model$net.result)

testvolt = c(1.2,2.2,3.2,3.9,3) #แรงดัน
datatest = data.frame(testvolt)
predictresult = predict(model,datatest)

predictresult



#################Z-scrore scaling
funcZscore = function(X) {
  u = mean(X)
  sd = sd(X)
  z = (X-u)/sd
  return(z)
}

funcMaxMin = function(x)
{
  minx = min(x)
  maxx = max(x) 
  range = maxx-minx 
  z = (x - minx) / range 
  return(z)
}

funcNormlization = function(x)
{
  sumx = sum(x)
  z = x/-sumx
  return(z)
}

x1 = runif(10,-10,10) 
plot(x1,type='l', col='red') 
x2 = funcZscore (x1) 
lines(x2,col='blue')
x3 = (x1/3)+5
lines(x3,col='green')
x3 = (x1/3)-5
lines(x3,col='green')
x4 = funcMaxMin(x1)
lines(x4,col='yellow')
x5 = funcNormlization(x1)
lines(x5,col='black')

#Shift
#x3 = (x1/3)+5
#lines(x3,col='green')
#ดูค่าเฉลี่ย
mean(x1)
mean(x3)
mean(x2)
mean(x4)

#Ratio การคูณหรือหาร

#Zero-Sum Normalization
#ผลรวมของสมาชกิ มคี า่ เป็ น 0 (บางทใี่ ช ้ 1)


