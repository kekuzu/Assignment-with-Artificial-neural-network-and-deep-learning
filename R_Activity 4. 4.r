#CREATE DATA TRAIN
library("neuralnet")

mydata = read.csv('/Users/kkeexx/R/HW_R/rating_final.csv', header = T)
mydata
service_rating = mydata[1:100,3]
food_rating = mydata[1:100,3]
rating = mydata[1:100,3]
CustomerWillTip = service_rating+food_rating+rating
datatrain = data.frame(service_rating,rating,food_rating) 
datatrain
##Train the model based on output from input
model <- neuralnet (CustomerWillTip~service_rating+food_rating+rating,datatrain,
                    
                    hidden=50, ##<--Change here 
                    rep = 1, ##loop 1 time
                    linear.output = TRUE,
                    stepmax = 1E6)

print(model) 

print(model$net.result)
plot(model) 
#-------------------------------------------------------#
# SAVE THE MODEL TO A BINARY FILE
#RDS is to group object
saveRDS(model,file = "/Users/kkeexx/R/nnmodel_rest.dat")

#-------------------------------------------------------#

# PREPARING TEST DATA
service_rating = mydata[50:100]
food_rating = mydata[50:100]
rating = mydata[50:100]
datatest = data.frame(service_rating,rating,food_rating) 
datatrain

#-------------------------------------------------------#
# LOAD THE MODEL
modeltest = readRDS(file = "/Users/kkeexx/R/nnmodel_rest.dat")
pred <- predict(modeltest, datatest) 
pred
#take data to col 3 to see
datatest[,4] = pred
datatest[,5] = service_rating*rating*food_rating/1000
datatest
