library(tidyverse)
library(ggplot2)
library(ggord)
library(knitr)
library(gridExtra)
library(cowplot)
library(openxlsx)
library(heplots) # Visualizing Hypothesis Tests in Multivariate Linear Models
library(ggstatsplot)
########################################################################

library(cluster)
library(factoextra)
library(flexclust)
library(hopkins)
library(ClusterR)
library(clusterSim)
library(ordr)
library(ordr.extra)
library(MASS)
library(FactoMineR)         # Multivariate Exploratory Data Analysis and Data Mining
library(CCA)                # Canonical correlation analysis
library(klaR)               # Classification and Visualization
library(caret)
library(matrixTests)
library(multiColl)
library(spls)               # Sparse Partial Least Squares (SPLS) Regression and Classification
library(performance)        # Assessment of Regression Models Performance
library(smacof)             #  Multidimensional Scaling
library(paran)              # Horn's Test of Principal Components/Factors
library(moments)            # skewness, kurtosis and related tests

########################################################################
library(plyr); library(dplyr)
library(BBmisc)
library(flextable)          # beautifying tables
library(psych)              # psychological research: descr. stats, FA, PCA etc.
library(skimr)              # summary stats
library(gtsummary)          # publication ready summary tables
library(ggpubr)             # publication ready data visualization in R
library(ggcorrplot)
library(kableExtra)
########################################################################
#######################################################################################################
# useful function

cal_z_score <- function(x){
  (x - mean(x)) / sd(x)
}


set.seed(123)

cat("\014") 
