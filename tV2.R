
testVectorv2 = vector()
testVectorXv2 = vector()
testVectorYv2 = vector()

a = 0
b = 2*pi
k = 0
n = 5

for(j in seq(a, b, by=pi/n))
{
  testVectorv2[k] = c(j,0)
  k = k + 1
}

k = 0;
for(j in seq(a, b, by=pi/n))
{
  testVectorXv2[k] = testVectorYv2[k] =j
  k = k + 1
}


testVectorv2