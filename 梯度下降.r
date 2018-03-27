rm(list = is(all=TRUE)

launch = read.csv("d:\\challenger.csv")

Y = launch[1]

X = launch[2]

X[2]=1

X = as.matrix(X)

X = X[,c(2,1)]

Y = as.matrix(Y)

m = length(Y)

solve(t(X)%*%X)%*%t(X)%*%Y

grad = function(X,Y,theta){
  
  gradient = (1/m)*(t(X)%*%((X%*%t(theta))-Y))
  
  return(t(gradient))
  
}

grad.descent = function(X,maxit){
  theta = matrix(c(0,0),nrow=1) #initialize the parameter
  
  alpha = .0001 #ser learning rate
  
  for (i in 1:maxit){
    theta = theta - alpha * grad(X,Y,theta)
    
    #print(theta)
    
    #print((X%*% t(theta))-Y)
  }
  
  return(theta)
}

print(grad.descent(X,30000000))
