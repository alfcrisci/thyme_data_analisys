################################################

library(openxlsx)
library(ggstatsplot)
library(ggstats)
library(ggord)

##############################################################
# imposto un valore sul generatore dei numeri casuali per asssicurarmi la riproducibilità


set.seed(123)

#############################################################

setwd("C:\\aaa_lavori\\lav_timo_morfo")

source("aux_pca.R")

#############################################################

timo_morfo_df<-read.xlsx("timo_morfo.xlsx","dati")

#####################################################################################
# [1] "ID"                "sample"           
# [3] "leaf_surface"      "leaf_length"      
# [5] "blade_leaf_length" "blade_leaf_width" 
# [7] "tn_abaxial_leaf"   "t_narea"          
# [9] "flower_length"     "calix_length"     
# [11] "corolla_length"
#####################################################################################


timo_morfo_pca_df=timo_morfo_df[,-2]


###############################################################################################
# PCA analisys

X=timo_morfo_pca_df[,-1]
Y=timo_morfo_pca_df[,1]
ord=PCA(X)
ggord(ord, Y,txt=F,arrow=F,obslab=T,ellipse=F)


###############################################################################################
# cluster analisys

df=na.omit(X[,-1])

silhouette_score <- function(k){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)

plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

res.hk <-hkmeans(df, 4,hc.metric =  'manhattan')

fviz_dend(res.hk,
          cex = 0.5,
          palette = "jco", 
          rect = TRUE, 
          rect_border = "jco", 
          rect_fill = TRUE,
          main="Cluster morfologici Thymus spp sez. serpillum",
          sub="cluster")



fviz_cluster(res.hk, data = df)



res.hk <-hkmeans(df, 4,hc.metric =  'manhattan',hc.method="centroid" )
fviz_cluster(res.hk, data = df)


###############################################################################################


g_leaf_surface <- ggbetweenstats(timo_morfo_df, 
                    ID, 
                    leaf_surface,pairwise.display = 'none')

##########################################################
