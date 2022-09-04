###############################################
#ACTIVITY 2.1-2.2 INTRODUCTION TO PERCEPTRON
#NOTE: RANDOM GENERATE WEIGHT
###############################################
###############################################
step_func = function(value, threshold)
{
	if(value>=threshold)
		result = 1
	else
		result = 0
	result	
}


###############################################
perceptron = function(x)
{	
	err = 1
	iter = 0
	maxi = length(x$x1)
	while(err>0.01 && iter<5000)
	{
		w1 = runif(1)	#RANDOM VALUE		
		w2 = runif(1)	#RANDOM VALUE

		err = 0
		for(i in 1:maxi)
		{
			#print(x[i,1])
			x1 = x[i,1]
			x2 = x[i,2]
			sum = x1*w1 + x2*w2
			#print(sum)	

			yh = step_func(sum,100)

			yt = x[i,3]
			if(yh==yt)
			{
				#print("pass")	
			}else
			{
				#print("not pass")
				err = err+1
			}
		}
		err = err/maxi
		#print(err)
		iter = iter+1
		#print(iter)
	}
	#print(iter)
	#print(c(w1,w2))
	listReturn = list("weight" = c(w1,w2), "error" = err)
	return(listReturn)
} 

###############################################
test_perceptron = function(x,w1,w2)
{
	err = 1
	iter = 0
	maxi = length(x$x1)
	err = 0
	for(i in 1:maxi)
	{
		x1 = x[i,1]
		x2 = x[i,2]
		sum = x1*w1 + x2*w2
		yh = step_func(sum,100)
		yt = x[i,3]
		if(yh==yt)
		{
			print("pass")	
		}else
		{
			print("not pass")
			err = err+1
		}
	}
	print(err/maxi)
} 


###############################################
#MAIN PROGRAM##################################
###############################################

print("----START-----")
#LOAD DATA
ga = read.csv("/Users/kkeexx/R/Metrial02/group_a.txt", header=T, sep="\t")
gb = read.csv("/Users/kkeexx/R/Metrial02/group_b.txt", header=T, sep="\t")

#PLOT DATA
plot.new()
plot(ga[,1:2],col="red")
lines(gb[,1:2],type="p",col="blue")

#CHECK RANGE OF DATA
summary(ga)
summary(gb)

print("-----TRAIN-----")
gc = rbind(ga[1:4,] , gb[1:4,])	#SMALL
#gc = rbind(ga,gb) 				#LARGE
z = perceptron(gc)
print(z)

print("-----TEST------")
gc = rbind(ga,gb) 				#LARGE
test_perceptron(gc,z$weight[1],z$weight[2])
print("----END-----")

x = c(12,13,14,15,15,16)
length(x)

10:20-3
a = c(11,22,33,44,55)
is(a)

x = c(1,2,3,4,5,6)
y = c(6,5,4,3,2,1)
cor(x,y)
