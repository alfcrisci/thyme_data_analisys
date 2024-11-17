# install R       https://cran.r-project.org/bin/windows/base/
# install Rtools  https://cran.r-project.org/bin/windows/Rtools/rtools44/rtools.html 
# install Rstudio https://posit.co/download/rstudio-desktop/

#######################################################################################################
# installazione pacchetto

install.packages("devtools")

install.packages(c("tidyverse","ggplot2","ggord","knitr","gridExtra","cowplot","openxlsx","heplots"))
install.packages(c("cluster","factoextra","flexclust","hopkins","ClusterR","clusterSim","ordr","ordr.extr","MASS"))
install.packages(c("FactoMineR","CCA","klaR"))
install.packages(c("caret","matrixTests","multiColl","spls","performance","smacof","paran","moments"))
install.packages(c("plyr","dplyr","BBmisc","flextable","psych","skimr","gtsummary","ggpubr","ggcorrplot","kableExtra"))
install.packages("skimr")
install.packages(c("vegan","ecodist","mFD"))
#######################################################################################################
# useful function

cal_z_score <- function(x){
  (x - mean(x)) / sd(x)
}


set.seed(123)

cat("\014") 
