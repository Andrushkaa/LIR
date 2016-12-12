


M <- mesh(testVectorX, testVectorY)

X <- M$x
Y <- M$y
Z <- CalculateFunction(length(testVector),testVector)

zMatrix <- matrix(Z,nrow = dim(X),ncol = dim(X))

surf3D(x = X, y = Y,z = zMatrix, 
       col = ramp.col(col = c("red", "red", "orange"), n = 100),
       colkey = FALSE, shade = 0.5, expand = 1.2, box = FALSE, 
       phi = 35, lighting = TRUE, ltheta = 560)
plot(X,zMatrix,"l",col="red")
CalculateError(length(testVector))