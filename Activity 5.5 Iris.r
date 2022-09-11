#LOAD DATA 
data(iris)

#PRINT SUMMARY
summary(iris)

#PLOT 
plot(iris,
bg = c("red","green","blue"),pch=21)

#20 sample data from 150 data to check data details.
#สุมหยิบ data เช่นสุ่มกยิบจาก 10000 ,มาสัก  100
s = sample(1:150,20)
siris = iris[s,]
siris


#เปลี่ยนข้อมูลตัวหนังสือเป็นเลข
datatrain = data.frame(iris)
datatrain$Species = c(rep(1,50),rep(2,50),rep(3,50))
print(datatrain)


#traindata
model = neuralnet(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                  datatrain,
                  hidden = 5,
                  rep = 1,
                  linear.output = TRUE)


print(model) 

print(model$net.result)
plot(model) 
