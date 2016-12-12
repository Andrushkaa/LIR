normal1 <- function(x)
{
  c( numDeriv::grad(boundH1,boundH1(x))[2]/norm(as.matrix(numDeriv::grad(boundH1,boundH1(x)))), 
     -numDeriv::grad(boundH1,boundH1(x))[1]/norm(as.matrix(numDeriv::grad(boundH1,boundH1(x)))) )  
}


normal2 <- function(x)
{
  c( numDeriv::grad(boundH2,boundH2(x))[2]/norm(as.matrix(numDeriv::grad(boundH2,boundH2(x)))), 
     -numDeriv::grad(boundH2,boundH2(x))[1]/norm(as.matrix(numDeriv::grad(boundH2,boundH2(x)))) )  
}


normal3 <- function(x)
{
  c( numDeriv::grad(boundH3,boundH3(x))[2]/norm(as.matrix(numDeriv::grad(boundH3,boundH3(x)))), 
     -numDeriv::grad(boundH3,boundH3(x))[1]/norm(as.matrix(numDeriv::grad(boundH3,boundH3(x)))) )  
}

H11 <- function (x,y)
{
  if (x == y){
    dot(numDeriv::genD(boundH1,boundH1(x))$f0,normal1(x))/2*norm(as.matrix(numDeriv::grad(boundH1,boundH1(x))))
    }else{
    x = boundH1(x);
    y = boundH1(y);
  (dot((x-y),normal1(x)))/(norm(as.matrix(x-y)))**2
  }
}

H12 <- function(x,y)
{
  if(x == y){
    log(1/(2.7 *norm(as.matrix(numDeriv::grad(boundH2,boundH2(x))))))/2
  }else {
    x = boundH2(x);
    y = boundH2(y);
    log(1/norm(as.matrix(x-y)))
    }
}

H13 <- function (x,y)
{
  if (x == y){
    dot(numDeriv::genD(boundH1,boundH1(x))$f0,normal1(x))/2*norm(as.matrix(numDeriv::grad(boundH1,boundH1(x))))
  }else{
    x = boundH3(x);
    y = boundH3(y);
    (dot((x-y),normal3(x)))/(norm(as.matrix(x-y)))**2
  }
}

H21 <- function(x,y)
{
  if (x == y){
    dot(numDeriv::genD(boundH1,boundH1(x))$f0,normal1(x))/2*norm(as.matrix(numDeriv::grad(boundH1,boundH1(x))))
  }else{
    x = boundH1(x);
    y = boundH1(y);
    (dot((x-y),normal1(x)))*(dot((x-y),normal1(y)))/(norm(as.matrix(x-y)))**2
  }
}

H22 <- function(x,y)
{
  if (x == y){
    dot(numDeriv::genD(boundH2,boundH2(x))$f0,normal2(x))/2*norm(as.matrix(numDeriv::grad(boundH2,boundH2(x))))
  }else{
    x = boundH2(x);
    y = boundH2(y);
    (dot((x-y),normal2(y)))/(norm(as.matrix(x-y)))**2
  }
}

H23 <- function(x,y)
{
  if (x == y){
    dot(numDeriv::genD(boundH3,boundH3(x))$f0,normal3(x))/2*norm(as.matrix(numDeriv::grad(boundH3,boundH3(x))))}else{
    x = boundH3(x);
    y = boundH3(y);
    (dot((x-y),normal3(x)))*(dot((x-y),normal3(y)))/(norm(as.matrix(x-y)))**2
  }
}
boundH1 <- function(x)
{
  c(cos(x[1])+2,sin(x[1])+2)
}

boundH2 <- function(x)
{
  c(cos(x[1]),sin(x[1]))
}

boundH3 <- function(x)
{
  c(4*cos(x[1]),4*sin(x[1]))
}