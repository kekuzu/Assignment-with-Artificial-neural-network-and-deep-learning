#CREATE DATA TRAIN
library("neuralnet")
#radius = 1:20
#height = 1:20
radius = runif(50,1,12)
height = runif(50,1,12)
volume = pi*radius*radius*height
datatrain = data.frame(radius,height,volume) 
datatrain

# TRAIN THE DATA
model <- neuralnet( volume~radius+height, datatrain,
                    hidden=50, ##<--Change here 
                    rep = 1, ##loop 1 time
                    linear.output = TRUE,
                    stepmax = 1E6)
print(model) 
plot(model) 
print(model$net.result)


#-------------------------------------------------------#
# SAVE THE MODEL TO A BINARY FILE
#RDS is to group object
saveRDS(model,file = "/Users/kkeexx/R/nnmodel.dat")
#-------------------------------------------------------#

# PREPARING TEST DATA
#generate random data
radius = runif(12,1,12)
#height = runif(12,1,12)
height = radius
datatest = data.frame(radius,height) 
datatest
#-------------------------------------------------------#

# LOAD THE MODEL
modeltest = readRDS(file = "/Users/kkeexx/R/nnmodel.dat")
pred <- predict(modeltest, datatest) 
pred
#take data to col 3 to see
datatest[,3] = pred
datatest[,4] = pi*radius*radius*height
datatest