# if not installed unccoment & run next 2 line
  

# install.packages(c("umap", "dendextend","mgcv","reticulate"))
# install.packages("estimatr")
 

################################################

  library(reticulate)
  library(openxlsx)
  library(ggstatsplot)
  library(ggstats)
  library(ggord)
  library(dendextend)
  library(umap)
  library(mgcv)
  library(effects)
  library(estimatr)

  ##############################################################
  # imposto un valore sul generatore dei numeri casuali per asssicurarmi la riproducibilità
  
  
  set.seed(123)
  


#############################################################

setwd("") # Qui indicare la directory dove mettere i dati e dove sono il gruppo dei file dezippati.

source("aux_pca.R")

#############################################################

  #############################################################
  
  timo_morfo_bil_df<-read.xlsx("timo_morfo.xlsx","dati_bilanciati")
  timo_morfo_med_df<-read.xlsx("timo_morfo.xlsx","dati_mediati")
  
  view(timo_morfo_bil_df)
  view(timo_morfo_med_df)
  
  names(timo_morfo_bil_df)
  
  #######################################################################################
  # [1] "ID"                "sample"            "leaf_surface"     
  # [4] "leaf_length"       "blade_leaf_length" "blade_leaf_width" 
  # [7] "tn_abaxial_leaf"   "t_narea"           "flower_length"    
  # [10] "calix_length"      "corolla_length"  
  #######################################################################################
  
  timo_qual_df_nov<-read.xlsx("timo_qual.xlsx","nov2023qual")
  timo_qual_df_feb<-read.xlsx("timo_qual.xlsx","feb2024qual")
  timo_qual_df_mag<-read.xlsx("timo_qual.xlsx","mag2024qual")
  timo_qual_df_giu<-read.xlsx("timo_qual.xlsx","giu2024qual")
  names(timo_qual_df_giu)
  ##########################################################
  # [1] "ID"               "a_pinene"         "a_thujene"       
  # [4] "camphene"         "b_pinene"         "sabinene"        
  # [7] "myrcene"          "a_phellandrene"   "a_terpinene"     
  # [10] "limonene"         "cineolo"          "trans_b_ocimene" 
  # [13] "g_terpinene"      "cis_b_ocimene"    "p_cymene"        
  # [16] "camphor"          "linalool"         "linalylacetate"  
  # [19] "four_ol_terpinen" "b_caryophillene"  "a_terpineol"     
  # [22] "borneol"          "a_citral"         "geranylacetate"  
  # [25] "nerol"            "geraniol"         "thymol"          
  # [28] "eugenol"          "carvacrol" 
  ###################################################################################
  
  timo_quant_FW_df_nov<-read.xlsx("timo_quant.xlsx","novFW2023quant")
  timo_quant_FW_df_feb<-read.xlsx("timo_quant.xlsx","febFW2024quant")
  timo_quant_FW_df_mag<-read.xlsx("timo_quant.xlsx","magFW2024quant")
  timo_quant_DW_df_mag<-read.xlsx("timo_quant.xlsx","magDW2024quant")
  timo_quant_DW_df_giu<-read.xlsx("timo_quant.xlsx","giuDW2024quant")
  
#####################################################################################
# checking names , length and order of name of variables in data.frames
  
  sort(names(timo_quant_FW_df_nov))==sort(names(timo_qual_df_giu))
  length(timo_quant_FW_df_nov)==length(timo_quant_FW_df_feb)
  
#####################################################################################
###################################################################################### 
  dim(timo_morfo_med_df) 
  dim(timo_morfo_bil_df)
  dim(timo_qual_df_giu)
  dim(timo_quant_DW_df_giu)
  giu2024qual=timo_qual_df_giu
  giuDW2024quant=timo_quant_DW_df_giu
  
  qual2morfo=merge(giu2024qual,timo_morfo_med_df,by="ID")
  quant2morfo=merge(giuDW2024quant,timo_morfo_med_df,by="ID")
  
  write.xlsx(list(qual2morfo,quant2morfo),"matrici_analisi.xlsx")
  ##################################################################################
 
  
   G <- gam(total~s(g_terpinene)+leaf_surface,data=quant2morfo)
  
  plot(G)
  
  G <- gam(total~s(p_cymene)+leaf_surface,data=quant2morfo)
  
  plot(G)
  
  G <- gam(total~s(thymol)+leaf_surface,data=quant2morfo)
  
  plot(G)
  
  G <- gam(total~s(carvacrol)+leaf_surface,data=quant2morfo)
  
  plot(G)
  
  G <- gam(total~s(tn_abaxial_leaf)+leaf_surface,data=quant2morfo)
  
  plot(G)
  ##############################################################################
  lm_tricome <- lm(total~tn_abaxial_leaf,data=quant2morfo[1:16,])
  summary(lm_tricome)
  plot(lm_tricome,2)
  eall_tricome <- predictorEffects(lm_tricome)
  plot(eall_tricome)
  lm_tricome_robust <- lm_robust(total~tn_abaxial_leaf,data=quant2morfo)
  
  ##############################################################################
  # Kosakowska, Olga & Baczek, Katarzyna & Przybył, Jarosław & Pawełczak, Anna & Rolewska, Katarzyna & Węglarz, Zenon. (2020). 
  # Morphological and Chemical Traits as Quality Determinants of Common Thyme (Thymus vulgaris L.), on the Example of ‘Standard Winter’ Cultivar. Agronomy. 10. 909. 10.3390/agronomy10060909. 
   #https://www.researchgate.net/publication/342456313_Morphological_and_Chemical_Traits_as_Quality_Determinants_of_Common_Thyme_Thymus_vulgaris_L_on_the_Example_of_'Standard_Winter'_Cultivar
   #Thymol and carvacrol biosynthesis pathway (Mikio and Taeko, 1962). 
  
  ##################################################################################
  # 
  aa=stepAIC(lm(total~leaf_surface+leaf_length+blade_leaf_length+blade_leaf_width+tn_abaxial_leaf+t_narea+flower_length+calix_length+corolla_length,quant2morfo))
  bb=stepAIC(lm(thymol~leaf_surface+leaf_length+blade_leaf_length+blade_leaf_width+tn_abaxial_leaf+t_narea+flower_length+calix_length+corolla_length,qual2morfo))
  
  # risultati troppo deboli
  
        
#####################################################################################
# PCA morfo
  
# eliminate sample var ( is a label categorical variable not used)

timo_morfo_pca_df=timo_morfo_bil_df[,-2]

# create PCA matrix ( no missing data and numeric)

X=timo_morfo_pca_df[,-1]
Y=timo_morfo_pca_df[,1]
ord=PCA(X)

#title="PCA loading vectors morphological variables"

ggord(ord, Y,txt=F,arrow=F,obslab=T,ellipse=T)

#####################################################################
# PCA non lineare

morfo.umap <- umap::umap(X)
layout <- morfo.umap [["layout"]] 
layout <- data.frame(layout) 
final <- cbind(layout, Thy_sample=Y)

final %>%
  ggplot(aes(x = X1, 
             y = X2, 
             color =Thy_sample))+
  geom_point()+
  labs(x = "UMAP1",
       y = "UMAP2",
       subtitle = "Thyme morfological clusters")

# ref https://datavizpyr.com/how-to-make-umap-plot-in-r/
################################################################################
# PCA qual


Xqual=timo_qual_df_giu[,-1]
Yqual=timo_qual_df_giu[,1]
row.names(Xqual)=Yqual
ordqual=PCA(scale(Xqual))


ggord(ordqual, Yqual,txt=F,arrow=F,obslab=T,ellipse=F,xlims=c(-5.5,5),ylims=c(-3,4),sizelab=2)+ggtitle("PCA volatile variables")

####################################################################################################################################
# umap algorithm 

morfo.umap.qual <- umap::umap(Xqual)
layout.qual <- morfo.umap.qual [["layout"]] 
layout.qual <- data.frame(layout.qual) 
final.qual <- cbind(layout.qual, Thy_sample=Yqual)

final.qual %>%
  ggplot(aes(x = X1, 
             y = X2, 
             color =Thy_sample))+
  geom_point()+
  labs(x = "UMAP1",
       y = "UMAP2",
       subtitle = "Thyme volatile based clusters")


################################################################################
# cluster analisys



silhouette_score_morfo <- function(k){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}

silhouette_score_qual <- function(k){
  km <- kmeans(df_qual, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df_qual))
  mean(ss[, 3])
}



################################################################################################################
k <- 2:10

avg_sil_morfo <- sapply(k, silhouette_score_morfo)
plot(k, avg_sil_morfo,  type='b',
     xlab='Number of clusters', 
     ylab='Average Silhouette Scores',
     main="Cluster con parametri morfologici",
     frame=T)


avg_sil_qual <- sapply(k, silhouette_score_qual)
plot(k, avg_sil_qual,  type='b',
     xlab='Number of clusters', 
     ylab='Average Silhouette Scores',
     main="Cluster con composti volatili",
     frame=F)

################################################################################################################
df_qual=timo_qual_df_giu[,-1]

row.names(df_qual)<-timo_qual_df_giu$ID
res.hk.qual <-hkmeans(df_qual, 4,hc.metric =  'euclidean')
fviz_dend(res.hk.qual,
          cex = 0.5,
          palette = "jco", 
          rect = TRUE, 
          rect_border = "jco", 
          rect_fill = TRUE,
          main="Cluster con composti volatili",
          sub="cluster")
df=timo_morfo_med_df[,-1]
row.names(df)<-timo_morfo_med_df$ID

df=df[-2,]
df=df[-2,]



res.hk.morfo <-hkmeans(df, 4,hc.metric =  'euclidean') # manhattan 
fviz_dend(res.hk.morfo,
          cex = 0.5,
          palette = "jco", 
          rect = TRUE, 
          rect_border = "jco", 
          rect_fill = TRUE,
          main="Cluster con parametri morfologici",
          sub="cluster")

dend1 <- as.dendrogram (res.hk.morfo$hclust)
dend2 <- as.dendrogram (res.hk.qual$hclust)
# Create a list to hold dendrograms
dend_list <- dendlist(dend1, dend2)

##############################################################################################################

tanglegram(dend1, 
           dend2, 
           common_subtrees_color_lines = T, 
           highlight_distinct_edges  = TRUE, 
           highlight_branches_lwd=FALSE, 
           margin_inner=7,
           lwd=2,
           main_left="Morphological",
           main_right="Chemiotypes",
           cex_main = 2)


cor_bakers_gamma(dend1, dend2)
cor_cophenetic(dend1, dend2)

################################################################################################################

fviz_cluster(res.hk.morfo, data = df)
fviz_cluster(res.hk.qual, data = df)

################################################################################################################

###############################################################################################
# Analisi di gruppo per variabile

g_leaf_surface <- ggbetweenstats(timo_morfo_bil_df, 
                                 ID, 
                                 leaf_surface,pairwise.display = 'none')

g_leaf_surface 

g_t_narea <- ggbetweenstats(timo_morfo_bil_df, 
                                 ID, 
                                 t_narea,pairwise.display = 'none')
g_t_narea 

g_flower_length <- ggbetweenstats(timo_morfo_bil_df, 
                            ID, 
                            flower_length,pairwise.display = 'none')

g_flower_length
###############################################################################################



