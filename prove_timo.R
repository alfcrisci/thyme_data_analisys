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
# "Leaf_surface"     
# "Leaf_length"       "Blade_leaf_length" "Blade_leaf_width" 
# "T_N_Abaxial_leaf"  "T_narea"           "flower_length"    
# "Calix_length"      "Corolla_length"   
#####################################################################################


timo_morfo_pca_df=timo_morfo_df[,-2]



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








p <- ggbetweenstats(timo_morfo_df, 
                    ID, 
                    Leaf_surface,pairwise.display = 'none')

##########################################################




mod1 <- lm(Fertility ~ ., data = swiss)
ggcoef_model(mod1)

models <- list(
  "Full model" = mod1,
  "Simplified model" = mod2,
  "With interaction" = mod3
)

ggcoef_compare(models, type = "faceted")