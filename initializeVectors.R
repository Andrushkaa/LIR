
testVector = vector()
testVectorX = vector()
testVectorY = vector()

a = 0
b = 2*pi
k = 0
n = 32

for(j in seq(a, b, by=pi/n))
{
  testVector[k] = c(j,0)
  k = k + 1
}

k = 0;
for(j in seq(a, b, by=pi/n))
{
  testVectorX[k] = testVectorY[k] =j
  k = k + 1
}


testVector