

######################################################################
#####################  Creating Test Grids  ##########################
######################################################################

# Define test.grid.2D
x1.grid <- rep(seq(0.2,0.8,0.2),4)
x2.grid <- rep(seq(0.2,0.8,0.2),each=4)
test.grid.2D <- data.frame("x1"=x1.grid,"x2"=x2.grid)

# Define test.grid.3D
x1.grid <- rep(c(0.3,0.5,0.7),9)
x2.grid <- rep(rep(c(0.3,0.5,0.7),each=3),3)
x3.grid <- rep(c(0.3,0.5,0.7),each=9)
test.grid.3D <- data.frame("x1"=x1.grid,"x2"=x2.grid,"x3"=x3.grid)


######################################################################
#####################  Creating Datasets  ############################
######################################################################



# m2:  y = x1
m2.y <- test.grid.2D$x1 

# m3:  y = exp(x1)
m3.y <- exp(test.grid.2D$x1) 


# m5:  y = exp(x1) + sin(pi x2)
m5.y <- exp(test.grid.2D$x1) + sin(pi*test.grid.2D$x2) 

# m7:  y = x1 + x2 + x3
m7.y <- test.grid.3D$x1 + test.grid.3D$x2 + test.grid.3D$x3 

# m8:  y = exp(x1) + exp(x2) + exp(x3)
m8.y <- exp(test.grid.3D$x1) + exp(test.grid.3D$x2) + exp(test.grid.3D$x3) 

# m10:  y = x1*x3 + x2*x3
m10.y <- test.grid.3D$x1*test.grid.3D$x3 + test.grid.3D$x2*test.grid.3D$x3 

# m11:  y = exp(x1*x3) + exp(x2*x3)
m11.y <- exp(test.grid.3D$x1*test.grid.3D$x3) + exp(test.grid.3D$x2*test.grid.3D$x3) 

# m13:  y = x1*x2
m13.y <- test.grid.2D$x1*test.grid.2D$x2 

# m14:  y = x1*x2*x3
m14.y <- test.grid.3D$x1*test.grid.3D$x2*test.grid.3D$x3 

# m15:  y = exp(5*(x1+x2))/(1+exp(5*(x1+x2))) - 1
m15.y <- exp(5*(test.grid.2D$x1+test.grid.2D$x2))/(1+exp(5*(test.grid.2D$x1+test.grid.2D$x2))) - 1 

# m17:  y = 0.5*(1+sin(2*pi*(x1+x2)))
m17.y <- 0.5*(1+sin(2*pi*(test.grid.2D$x1+test.grid.2D$x2))) 

# m18:  y = 0.5*(1+sin(2*pi*(x1+x2+x3)))
m18.y <- 0.5*(1+sin(2*pi*(test.grid.3D$x1+test.grid.3D$x2+test.grid.3D$x3))) 

# m19:  y = (64*(x1*x2)^3)*(1-x1*x2)^3
m19.y <- (64*(test.grid.2D$x1*test.grid.2D$x2)^3)*(1-test.grid.2D$x1*test.grid.2D$x2)^3 

# # m20:  y = (64*(x1*x2*x3)^3)*(1-x1*x2*x3)^3
m20.y <- (64*(test.grid.3D$x1*test.grid.3D$x2*test.grid.3D$x3)^3)*(1-test.grid.3D$x1*test.grid.3D$x2*test.grid.3D$x3)^3 




########################################################################
#####################  Defining the D Matrices  ########################
########################################################################

Dbar.2D <- matrix(1/16,16,16)
Di. <- matrix(1,4,4)%x%diag(4)/4
D.j <- diag(4)%x%matrix(1,4,4)/4
Dbar.3D <- matrix(1/27,27,27)
Di.. <- matrix(1,9,9)%x%diag(3)/9
D.j. <- matrix(1,3,3)%x%diag(3)%x%matrix(1,3,3)/9
D..k <- diag(3)%x%matrix(1,9,9)/9
Di.k <- diag(3)%x%matrix(1,3,3)%x%diag(3)/3
D.jk <- diag(9)%x%matrix(1,3,3)/3

D.2D <- diag(16) - Di. - D.j + Dbar.2D
D.3D <- diag(27) - Di.. - D.j. - D..k + 2*Dbar.3D
#D.part <- Di.k + D.jk - Di.. - D.j. - 2*D..k
D.part <- diag(27) - Di.k - D.jk + D..k


size.m2 <- sum( (D.2D %*% m2.y)^2 )
size.m3 <- sum( (D.2D %*% m3.y)^2 )
size.m5 <- sum( (D.2D %*% m5.y)^2 )

size.m7 <- sum( (D.3D %*% m7.y)^2 )
size.m8 <- sum( (D.3D %*% m8.y)^2 )

size.m10 <- sum( (D.part %*% m10.y)^2 )
size.m11 <- sum( (D.part %*% m11.y)^2 )

size.m13 <- sum( (D.2D %*% m13.y)^2 )
size.m14 <- sum( (D.part %*% m14.y)^2 )

size.m15 <- sum( (D.2D %*% m15.y)^2 )

size.m17 <- sum( (D.2D %*% m17.y)^2 )
size.m18 <- sum( (D.part %*% m18.y)^2 )

size.m19 <- sum( (D.2D %*% m19.y)^2 )
size.m20 <- sum( (D.part %*% m20.y)^2 )

sizes = c(size.m2,size.m3,size.m5,size.m7,size.m8,size.m10,size.m11,
          size.m13,size.m14,size.m15,size.m17,size.m18,size.m19,size.m20)

