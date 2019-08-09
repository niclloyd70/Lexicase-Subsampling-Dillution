setwd("C:/Users/nicol/Desktop/BEACON/final/Dilution populations 1/populations/")

popsize = c(5,10,20,50,100)
Disc_TC = c(5,10,20)
NonDisc_TC = c(0,5,10,20,50,100,500)

for (repID in 1:100) {
  print(repID)
  for (Pop in popsize) 
  {
   for (x in Disc_TC) 
    {
      table=data.frame()
      for (a in 1:Pop) 
      { 
        Testcase = sample(x = c(1,0), size = x, replace = TRUE, prob = c(.5, .5))
        Number1 = sum(Testcase)
        table = rbind(c(Number1, Testcase),table)
      }
    
      for(b in NonDisc_TC) 
      { 
        TC_size = sum(x, b)
        copy.table=table
      
        if(b > 0)
        {
          for (c in 1:b)
          {  
            copy.table = cbind(copy.table,rep(0,Pop))
          }
        }
      colnames(copy.table)=c("Fitness",paste("Test Case", 1:TC_size))
    
      write.csv(copy.table,paste0(repID, "_",Pop, "_", x, "_", b, ".csv"))
      
      #print(copy.table)
      }
    } 
  } 
}    