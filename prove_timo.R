##################################################################################################################
# Questo codice è un esercizio di data analisys su dati GC MS #terpeni #morfologia #IBE #IBBR 

# Se i seguenti pacchetti R non sono installati prova a scommentare ( togliere cancelletto) e lanciare le successive 2 linee di codice
# install.packages(c("umap", "dendextend","mgcv","reticulate"))
# install.packages("estimatr")
# Enable the r-universe repo
# Se non è installato  ggord
# options(repos = c( fawda123 = 'https://fawda123.r-universe.dev', CRAN = 'https://cloud.r-project.org'))
# install.packages('ggord') 
#############################################################

setwd("") # Qui indicare la directory dove mettere i dati e dove sono il gruppo dei file dezippati.


##################################################################################################################
# carica le librerie necessarie

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
  library(FactominerR) # per PCA 
  library(MASS) # per stepAIC

  source("aux_pca.R") # carica tutte le altre

  ##############################################################
  # imposto un valore sul generatore dei numeri casuali per asssicurarmi la riproducibilità
  
  
  set.seed(123)
  

  #############################################################
  # Leggi i file di dati 

  timo_morfo_bil_df<-read.xlsx("timo_morfo.xlsx","dati_bilanciati")
  timo_morfo_med_df<-read.xlsx("timo_morfo.xlsx","dati_mediati")
  
  View(timo_morfo_bil_df)
  View(timo_morfo_med_df)
  
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
  
###################################################################################### 
# verifica la dimensione delle matrici di lavoro

  dim(timo_morfo_med_df) 
  dim(timo_morfo_bil_df)
  dim(timo_qual_df_giu)
  dim(timo_quant_DW_df_giu)

####################################################################################################
# unisco le matrici con i dati sui composti relativi ( qual) e assoluti in ppm ( quant) di giugno

  giu2024qual=timo_qual_df_giu
  giuDW2024quant=timo_quant_DW_df_giu
  
  qual2morfo=merge(giu2024qual,timo_morfo_med_df,by="ID")
  quant2morfo=merge(giuDW2024quant,timo_morfo_med_df,by="ID")
  
  write.xlsx(list(qual2morfo,quant2morfo),"matrici_analisi.xlsx")

#####################################################################################
# La strategia in analisi multivariata è sempre quella:

# A) Analisi delle PCA per capire le  
# B) Analisi delle UMAP ( PCA non lineari ) per capire se vi è un ordine de idati che rispecchia il lableing di campionamento o di altro raggruppamento
# C) Esplorazione non lineare o lineare con i modelli della matrice dati
# D) Clustering dei dati
# E) Confronto fra gruppi con ggstatsplot

#####################################################################################
# PCA morfologici
  
# eliminate sample var ( is a label categorical variable not used)

timo_morfo_pca_df=timo_morfo_bil_df[,-2]

# create PCA matrix ( no missing data and numeric)

X=timo_morfo_pca_df[,-1]
Y=timo_morfo_pca_df[,1]
ord=PCA(X)

# title="PCA loading vectors morphological variables"

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

# reference
# https://datavizpyr.com/how-to-make-umap-plot-in-r/
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


##################################################################################
# Questa è una fase esplorativa  dei dati utilizzando le matrici unite dei dati sui composti e quelli morfologici
# Utilizzo le matrici unite
  
  G <- gam(total~s(g_terpinene)+leaf_surface,data=quant2morfo) # esempio lineare
  summary(G) # chiedo le caratteristiche del modello
  plot(G) #  figura della curva

  G_lin <-lm(total~g_terpinene+leaf_surface,data=quant2morfo) # esempio lineare
  summary(G_lin) # chiedo le caratteristiche del modello
 
  G <- gam(total~s(p_cymene)+leaf_surface,data=quant2morfo)
  
  plot(G)
  
  G <- gam(total~s(thymol)+leaf_surface,data=quant2morfo)
  
  plot(G)
  
  G <- gam(total~s(carvacrol)+leaf_surface,data=quant2morfo)
  
  plot(G)
  
  G <- gam(total~s(tn_abaxial_leaf)+leaf_surface,data=quant2morfo)
  
  plot(G)
  ##############################################################################
  # ho capito che 
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
  # Analisi  di selezione con modello con predittando terpeni totali ( total) o timolo ( thymol)


  aa=stepAIC(lm(total~leaf_surface+leaf_length+blade_leaf_length+blade_leaf_width+tn_abaxial_leaf+t_narea+flower_length+calix_length+corolla_length,quant2morfo))
 
  bb=stepAIC(lm(thymol~leaf_surface+leaf_length+blade_leaf_length+blade_leaf_width+tn_abaxial_leaf+t_narea+flower_length+calix_length+corolla_length,qual2morfo))

  # Nelle variabili aa e bb avrete il modello finale selezionate con AIC più basso ( AKAIKE INFORMATION CRITERION)

  summary(aa)
  summary(bb)

  # risultati a mio avviso troppo deboli


################################################################################
# cluster analisys

# faccio prima l'analisi di silhouette pe capire il numero di cluster migliore
# prima creo delle funzioni per il plot successivo

# funzione caratteri morfologici

silhouette_score_morfo <- function(k){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}

# funzionecaratteri qualitativi

silhouette_score_qual <- function(k){
  km <- kmeans(df_qual, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df_qual))
  mean(ss[, 3])
}



################################################################################################################
# provo da 2 a 10 cluster

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
# visualizzo

df_qual=timo_qual_df_giu[,-1]  # tolgo le label dai dati

row.names(df_qual)<-timo_qual_df_giu$ID # metto le label com nome riga

res.hk.qual <-hkmeans(df_qual, 4,hc.metric =  'euclidean')
fviz_dend(res.hk.qual,
          cex = 0.5,
          palette = "jco", 
          rect = TRUE, 
          rect_border = "jco", 
          rect_fill = TRUE,
          main="Cluster con composti volatili",
          sub="cluster")



df=timo_morfo_med_df[,-1]           # tolgo le label dai dati
row.names(df)<-timo_morfo_med_df$ID # metto le label com nome riga

df=df[-2,]  # tolgo colonne testo che danno noia
df=df[-2,]  # tolgo colonne testo che danno noia


######################################################################################

res.hk.morfo <-hkmeans(df, 4,hc.metric =  'euclidean') # posso provare 'manhattan' al posto di 'euclidean' 

fviz_dend(res.hk.morfo,
          cex = 0.5,
          palette = "jco", 
          rect = TRUE, 
          rect_border = "jco", 
          rect_fill = TRUE,
          main="Cluster con parametri morfologici",
          sub="cluster")

###########################################################################################
# creo i due dendrogrammi 
dend_morfo <- as.dendrogram (res.hk.morfo$hclust)
dend_qual <- as.dendrogram (res.hk.qual$hclust)

# Create a list to hold dendrograms
dend_list <- dendlist(dend_morfo, dend_qual)

##############################################################################################################
# creo un diagramma incrociato

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

#########################################################àààà
# guarda le che relazioni ci sono fra i dendrogrammi 

cor_bakers_gamma(dend_morfo, dend_qual)
cor_cophenetic(dend_morfo, dend_qual)

################################################################################################################
# altro tipo di visualizzazione a ellissi

fviz_cluster(res.hk.morfo, data = df)

fviz_cluster(res.hk.qual, data = df)


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



