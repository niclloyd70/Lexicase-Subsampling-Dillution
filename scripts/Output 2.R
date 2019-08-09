popsize = c(5,10,20,50)
pop_ID = 1:Pop
Disc_TC = c(5,10, 20)
NonDisc_TC = c(0,5,10,20,50,100,500)
Population = rnorm(popsize,5,5)
Selection_Scheme = c("lexicase", "cohort_0_2", "tournament_7", "roulette","downsampled_0_1", "downsampled_0_2")

table=data.frame()

for (repID in 1:99) 
{ 
  for (Disc in Disc_TC) 
  { 
    for (Pop in popsize)  
    { 
      for(NonDisc in NonDisc_TC) 
      { 
        for (Selscheme in Selection_Scheme) 
        {
          if (!(Pop == 5 & Selscheme == "tournament_7")) 
          {
            filename = paste0("C:/Users/nicol/Desktop/BEACON/final/output3/output/", Selscheme, "__", repID, "_", Pop, "_", Disc, "_", NonDisc, ".csv")
            print(filename)
            Read = read.csv(filename, header = FALSE)
            table = rbind(table, c(Selscheme, repID, Disc, NonDisc, Pop, as.numeric(Read[1,]), rep(NA, max(popsize) - Pop)), stringsAsFactors = FALSE)
            #print(c(Selscheme, repID, Disc, NonDisc, Pop, as.numeric(Read[1,]), rep(NA, max(popsize) - Pop)))
          }
        }   
      }
    } 
  } 
}
colnames(table) = c("Selection_Scheme", "Rep_ID", "Discriminatory", "Non_Discriminatory", "Popsize", 1:max(popsize))

#print(table)

write.csv(table, "Dilution_Analysis.csv")
