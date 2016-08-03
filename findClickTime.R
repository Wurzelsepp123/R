minDist=99999
for(i in 1:nrow(part1corrected))
  {
    Dist=sqrt((ClickPoint[1,1]-(part1corrected[i,2]-2*1920))^2+(ClickPoint[1,2]-part1corrected[i,3])^2)
    if( Dist<minDist)
    {
      minDist=Dist
      matchedPoint=part1corrected[i,]
    }
}
   
print(matchedPoint)
print(minDist)



