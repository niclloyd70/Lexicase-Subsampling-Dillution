rm(list = ls())
library(dplyr)
library(ggplot2)
library(tidyr)
popsize = c(5,10,20,50)
NonDisc_TC = c(5,10,20,50,100,500)
Selection_Scheme = c("lexicase", "cohort_0_2", "tournament_7", "roulette","downsampled_0_1", "downsampled_0_2")
D_A = read.csv("Dilution_Analysis.csv")
#D_A = filter(D_A, Rep_ID == 17)
Re_D_A = tidyr::gather(D_A, key = "Org_ID", value = "Prob", paste0("X",1:max(popsize)))
#Re_D_A is the reorganized Dilution Analysis
Re_D_A$Prob = as.numeric(Re_D_A$Prob)

#ggplot(Re_D_A, aes(x = Prob, fill = Selection_Scheme), alpha = .2) + geom_histogram()+
#facet_grid(rows = vars(Discriminatory), cols = vars(Non_Discriminatory))



# for (Population in popsize) 
# {
#   popfilter = filter(Re_D_A, Popsize == Population)
#   for (f in c(5,10,20)) 
#   {
#     RDA_filtered = filter(popfilter, Discriminatory == f)
#     #print(paste(Population, f, nrow(RDA_filtered)))
#     ggp = ggplot(RDA_filtered, aes(x = Prob, fill = Selection_Scheme)) + geom_histogram()+
#     facet_grid(rows = vars(Selection_Scheme), cols = vars(Non_Discriminatory))+
#     ggtitle(paste0("Discriminatory ", f, " Population Size ", Population))
#     print(ggp)
#   }
# }

Test_Data= data.frame()

for (Population in popsize) 
{
  if (Population > 5)
  {
    popfilter = filter(Re_D_A, Popsize == Population)
    for (f in c(5,10,20)) 
    {
      RDA_filtered = filter(popfilter, Discriminatory == f)
      for (SlSchm in Selection_Scheme) 
      {
        SlSchm_Filter = filter(RDA_filtered, Selection_Scheme == SlSchm)
        NonDisc_Filter_0 = filter(SlSchm_Filter, Non_Discriminatory == 0)
        for (ND in NonDisc_TC) 
        { 
          NonDisc_Filter_ND = filter(SlSchm_Filter, Non_Discriminatory == ND )
           WCT = wilcox.test(NonDisc_Filter_0$Prob, NonDisc_Filter_ND$Prob)
               print(SlSchm)
               print(ND)
               print(f)
               print(Population)
               # print(NonDisc_Filter_0)
               # print(NonDisc_Filter_ND)
               print(WCT$p.value)
              Test_Data = rbind(Test_Data, c(SlSchm, ND, f, Population, WCT$p.value), stringsAsFactors= FALSE)
          
        }
          colnames(Test_Data) = c("Selection_Scheme", "Non_Discriminatory", "Discriminatory", "Population", "Wilcox_Test")
          Test_Data$Wilcox_Test = as.numeric(Test_Data$Wilcox_Test)
          Filter_Down =  Test_Data[
          Test_Data$Selection_Scheme == SlSchm &
          Test_Data$Discriminatory == f &
          Test_Data$Population == Population, ]
          print(p.adjust(Filter_Down$Wilcox_Test, method = "holm"))
          print(Filter_Down$Wilcox_Test)
          Filter_Down$wilcox_Test = p.adjust(Filter_Down$Wilcox_Test, method = "holm")
      }
    }
  }
}


colnames(Test_Data) = c("Selection_Scheme", "Non_Discriminatory", "Discriminatory", "Population", "Wilcox_Test")

Test_Data$Non_Discriminatory = as.numeric(Test_Data$Non_Discriminatory)
Test_Data$Discriminatory = as.numeric(Test_Data$Discriminatory)
Test_Data$Population = as.numeric(Test_Data$Population)
Test_Data$Wilcox_Test = as.numeric(Test_Data$Wilcox_Test)
SigDif = filter(Test_Data, Wilcox_Test <= 0.05 )
                
ggplot(Test_Data, aes(x = factor(Discriminatory, levels = c(5,10,20)), y = factor(Non_Discriminatory, levels = c(5,10,20,50,100,500)))) + 
geom_tile(aes(fill = Wilcox_Test)) +
facet_grid(rows = vars(Selection_Scheme), cols = vars(Population))


plotfilt = filter(Test_Data, is.element(Selection_Scheme, c("cohort_0_2", "downsampled_0_1", "downsampled_0_2")))

ggplot(plotfilt, aes(x = factor(Discriminatory, levels = c(5,10,20)), y = factor(Non_Discriminatory, levels = c(5,10,20,50,100,500)))) + 
geom_tile(aes(fill = Wilcox_Test)) +
facet_grid(rows = vars(Selection_Scheme), cols = vars(Population))


plotfilt_SigDif = filter(SigDif, is.element(Selection_Scheme, c("cohort_0_2", "downsampled_0_1", "downsampled_0_2")))

ggplot(plotfilt_SigDif, aes(x = factor(Discriminatory, levels = c(5,10,20)), y = factor(Non_Discriminatory, levels = c(5,10,20,50,100,500)))) + 
geom_tile(aes(fill = Wilcox_Test)) +
facet_grid(rows = vars(Selection_Scheme), cols = vars(Population))



                  