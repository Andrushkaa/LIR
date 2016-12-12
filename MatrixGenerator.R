
R <- 4
FundamentalFunc <- function(x,y)
{
  
  (1/2*pi) * log(1/norm(as.matrix(x-y))) + NTilda(x,y);
  
}
NTilda <- function(x,y)
{
  - (1/2*pi) * log(norm(as.matrix(y * x / (R * R * R) - x / (x * x) * (x / R))))
}

inc <- function(x,val)
{
  eval.parent(substitute(x <- x + val))
  if (x > 2*pi) {
    eval.parent(substitute(x <- 0))  
  }
}


FillMatrix <- function (m)
{
  nrow<-4*m 
  ncol<-4*m
  total <- matrix(0,nrow=nrow,ncol=ncol)
  funcVect = vector()
  
  t1 = 0
  t2 = 0
  
  endLoop = 2*m
  
  for(i in 1:(2*endLoop))
  {
    funcVect[i] = 0;
  }
  
  for (i in 1:endLoop){
    t2 = 0;
    for(j in 1:endLoop){
      
      total[i,j] <- (1/(2*m))*(H11(t1,t2))
      total[i,j + endLoop] <- (1/(2*m))*H12(t1,t2)
    
      total[i + endLoop,j] <- (1/(2*m))*H21(t1,t2)
      total[i + endLoop,j + endLoop] <- (1/(2*m))*H22(t1,t2)
    
      inc(t2,pi/m)
    }

    t2 = 0;
    for(j in 1:endLoop){
      
      funcVect[i] <- funcVect[i] + (1/2*m) * H13(t1,t2)
      funcVect[i + endLoop] <- funcVect[i+ endLoop] + (1/2*m) * H23(t1,t2)
    
      inc(t2,pi/m)
    }
    
    funcVect[i] <- funcVect[i] + boundCondition2(t1)
    funcVect[i+endLoop] <- funcVect[i+endLoop] + boundCondition3(t1)
    
    total[i,i] = total[i,i] - 1/2
    total[i + endLoop,i + endLoop] = total[i + endLoop,i + endLoop] - 1/2
    
    inc(t1,pi/m)
  }
  
  solve(total, funcVect)
}

CalculateFunction <- function(m,x)
{
  coff = FillMatrix(m);
  result = vector()
  
  
  for (k in 1:length(x))
  {
  endLoop = 2*m;  
  
  firstQuadr = 0;
  secondQuadr = 0;
  thirdQuadr = 0;
  
  t1 = 0
  for(i in 1:(2*m))
  {
    thirdQuadr = thirdQuadr + boundCondition1(funcH1(t1))*H11(funcH1(x[k]),funcH1(t1))
    inc(t1,pi/m)
  }  
  
  
  t1 = 0
  for(i in (2*m):(4*m))
  {
    secondQuadr = secondQuadr + coff[i]*H12(funcH1(x[k]),funcH1(t1))
    inc(t1,pi/m)
  }
  
  t1 = 0
  for(i in 1:(2*m))
  {
    firstQuadr = firstQuadr + coff[i]*H11(funcH1(x[k]),funcH1(t1))
    inc(t1,pi/m)
  }
  
  result[k] =  (1/2*m)*(firstQuadr + secondQuadr - thirdQuadr)
  }
  
  result
}
boundCondition1 <- function(t)
{
  1;
}

boundCondition2 <- function(t)
{
  t;
}

boundCondition3 <- function(t)
{
  0;
}
