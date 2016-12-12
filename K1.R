funcH1 <- function(x)
{
  c(cos(x[1])+2,sin(x[1])+2)
}

funcH2 <- function(x)
{
  c(cos(x[1]),sin(x[1]))
}

funcH3 <- function(x)
{
  c(4*cos(x[1]),4*sin(x[1]))
}

vertex1 <- function(x)
{
  c( numDeriv::grad(funcH1,funcH1(x))[2]/norm(as.matrix(numDeriv::grad(funcH1,funcH1(x)))), 
     -numDeriv::grad(funcH1,funcH1(x))[1]/norm(as.matrix(numDeriv::grad(funcH1,funcH1(x)))) )  
}


vertex2 <- function(x)
{
  c( numDeriv::grad(funcH2,funcH2(x))[2]/norm(as.matrix(numDeriv::grad(funcH2,funcH2(x)))), 
     -numDeriv::grad(funcH2,funcH2(x))[1]/norm(as.matrix(numDeriv::grad(funcH2,funcH2(x)))) )  
}


vertex3 <- function(x)
{
  c( numDeriv::grad(funcH3,funcH3(x))[2]/norm(as.matrix(numDeriv::grad(funcH3,funcH3(x)))), 
     -numDeriv::grad(funcH3,funcH3(x))[1]/norm(as.matrix(numDeriv::grad(funcH3,funcH3(x)))) )  
}

H11 <- function (x,y)
{
  if (x == y){
    dot(numDeriv::genD(funcH1,funcH1(x))$f0,vertex1(x))/2*norm(as.matrix(numDeriv::grad(funcH1,funcH1(x))))
    }else{
    x = funcH1(x);
    y = funcH1(y);
  (dot((x-y),vertex1(x)))/(norm(as.matrix(x-y)))**2
  }
}

H12 <- function(x,y)
{
  if(x == y){
    log(1/(2.7 *norm(as.matrix(numDeriv::grad(funcH2,funcH2(x))))))/2
  }else {
    x = funcH2(x);
    y = funcH2(y);
    log(1/norm(as.matrix(x-y)))
    }
}

H13 <- function (x,y)
{
  if (x == y){
    dot(numDeriv::genD(funcH1,funcH1(x))$f0,vertex1(x))/2*norm(as.matrix(numDeriv::grad(funcH1,funcH1(x))))
  }else{
    x = funcH3(x);
    y = funcH3(y);
    (dot((x-y),vertex3(x)))/(norm(as.matrix(x-y)))**2
  }
}

H21 <- function(x,y)
{
  if (x == y){
    dot(numDeriv::genD(funcH1,funcH1(x))$f0,vertex1(x))/2*norm(as.matrix(numDeriv::grad(funcH1,funcH1(x))))
  }else{
    x = funcH1(x);
    y = funcH1(y);
    (dot((x-y),vertex1(x)))*(dot((x-y),vertex1(y)))/(norm(as.matrix(x-y)))**2
  }
}

H22 <- function(x,y)
{
  if (x == y){
    dot(numDeriv::genD(funcH2,funcH2(x))$f0,vertex2(x))/2*norm(as.matrix(numDeriv::grad(funcH2,funcH2(x))))
  }else{
    x = funcH2(x);
    y = funcH2(y);
    (dot((x-y),vertex2(y)))/(norm(as.matrix(x-y)))**2
  }
}

H23 <- function(x,y)
{
  if (x == y){
    dot(numDeriv::genD(funcH3,funcH3(x))$f0,vertex3(x))/2*norm(as.matrix(numDeriv::grad(funcH3,funcH3(x))))}else{
    x = funcH3(x);
    y = funcH3(y);
    (dot((x-y),vertex3(x)))*(dot((x-y),vertex3(y)))/(norm(as.matrix(x-y)))**2
  }
}
