library(factoextra)
library(dplyr)
library(ggplot2)
library(corrplot)
library(factoextra)
library(FactoMineR)
library(cluster)
library(mclust)
library(MVN)
library(hrbrthemes)

df1 = read.csv("50_years_batting.csv")
df2 = read.csv("50_years_pitching.csv")
anyNA(df1)
anyNA(df2)
# removing observations from shortened seasons
df1 = df1 %>% 
  filter(!(Season %in% c(1994, 2020)))
df2 = df2 %>% 
  filter(!(Season %in% c(1994, 2020)))
concat_df = cbind(df1, df2) # combining data sets
# creating subsets of the data
reference_df = concat_df[c(2:3)] # reference data frame with categorical data
concat_df = concat_df[,-c(1:5,8,36:43,45)] # removing unnecessary columns for analysis
colnames(concat_df)[c(35:36, 38:42,46,53)] = c(
  "HA", 
  "RA", 
  "HRA", 
  "WA", 
  "IBA", 
  "SO P", 
  "HBPA",
  "ERA +",
  "SO/BB"
) # renaming duplicative column names
final_df = concat_df[,-c(1:2,17:21,29:30,46:53)] # keeping counting statistics only
final_df[] = lapply(final_df, as.numeric) 
pergame_df = sweep(final_df,1,unlist(df1[,5]),"/") # making features 'per game'

# per game data
correlations = cor(pergame_df)
correlations = round(correlations, digits=2)
corrplot(correlations, method="shade", shade.col=NA, tl.cex=0.5, tl.col="black")

# per game pca
pergame.normalized = scale(pergame_df)
pergame.pca = princomp(pergame.normalized)
summary(pergame.pca)

loadings = pergame.pca$loadings[, 1:2] # storing the loadings of pc1 and pc2
loadings_df = data.frame(variable = rownames(loadings), loadings)
sorted_loadings = lapply(loadings_df[, -1], function(x) {
  sorted_indices = order(abs(x), decreasing = TRUE)  
  sorted_values = x[sorted_indices]  
  sorted_variables = loadings_df$variable[sorted_indices]  
  data.frame(variable = sorted_variables, loading = sorted_values)
}) # sorting the loadings by absolute value in decreasing order 
print(sorted_loadings)

par(mfrow = c(2,1))
fviz_contrib(pergame.pca, choice = "var", axes = 1) + theme_ipsum()  + 
  theme(
    axis.text.x = element_text(size = 6),  
    axis.text.y = element_text(size = 6)   
  )  + 
  labs(x = "Features")  # visualizing the contribution of each variable to pc1
fviz_contrib(pergame.pca, choice = "var", axes = 2) + theme_ipsum()  + 
  theme(
    axis.text.x = element_text(size = 6),  
    axis.text.y = element_text(size = 6)  
  )  + 
  labs(x = "Features")

fviz_pca_var(pergame.pca, col.var = "contrib", alpha.var=0.8, repel=TRUE) + ggtitle("PCA biplot (PC1 vs. PC2)") + 
  theme(plot.title = element_text(hjust = 0.4)) + theme_ipsum()

my_pca_data = data.frame(pergame.pca$scores[,1:2])
fviz_nbclust(my_pca_data,
             FUNcluster = kmeans,
             method = "wss") + theme_ipsum()

set.seed(1993)
pca.kmeans = kmeans(my_pca_data,
                    centers=3)
fviz_pca_ind(pergame.pca,
             habillage = pca.kmeans$cluster,
             label="none",
             addEllipses=TRUE) + ggtitle("PCA plot of K-means clusters") + theme_ipsum()


# fitting gmm to pc1 and pc2
gmm_pca_data = data.frame(pergame.pca$scores[,1:2])
gmm_model = Mclust(gmm_pca_data)  
summary(gmm_model)
gmm_pca_data$Cluster = as.factor(gmm_model$classification)

cluster_colors = c("#69b3a2", "#fbc02d", "#ff7f50", "#8e24aa", "#a1887f")
ggplot(gmm_pca_data, aes(x = Comp.1, y = Comp.2, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +  
  stat_density_2d(aes(fill = Cluster), 
                  geom = "polygon", 
                  color = "white", 
                  alpha = 0.3,      
                  level = 0.95) +  
  stat_ellipse(aes(color = Cluster), 
               level = 0.95,    
               size = 1.2,        
               linetype = "solid") +  
  labs(title = "GMM Clusters",
       x = "Principal Component 1 (PC1)",
       y = "Principal Component 2 (PC2)") +
  scale_color_manual(values = cluster_colors) +  
  scale_fill_manual(values = cluster_colors) +  
  theme_ipsum() + 
  theme(legend.position = "bottom") 


reference_df = data.frame(reference_df, pca.kmeans$cluster, gmm_pca_data$Cluster) # adding cluster assignments to reference data frame
# creating k-means cluster data frame
km.cluster1 = reference_df[which(pca.kmeans$cluster == 1), ] 
km.cluster2 = reference_df[which(pca.kmeans$cluster == 2), ]
km.cluster3 = reference_df[which(pca.kmeans$cluster == 3), ]
km.cluster1 = km.cluster1[,-4]
km.cluster2 = km.cluster2[,-4]
km.cluster3 = km.cluster3[,-4]
# creating gmm cluster data frame
gmm.cluster1 = reference_df[which(gmm_pca_data$Cluster == 1), ] 
gmm.cluster2 = reference_df[which(gmm_pca_data$Cluster == 2), ] 
gmm.cluster3 = reference_df[which(gmm_pca_data$Cluster == 3), ] 
gmm.cluster4 = reference_df[which(gmm_pca_data$Cluster == 4), ] 
gmm.cluster5 = reference_df[which(gmm_pca_data$Cluster == 5), ] 
gmm.cluster1 = gmm.cluster1[,-3]
gmm.cluster2 = gmm.cluster2[,-3]
gmm.cluster3 = gmm.cluster3[,-3]
gmm.cluster4 = gmm.cluster4[,-3]
gmm.cluster5 = gmm.cluster5[,-3]


# exploring k-means results
kmcount1 = table(km.cluster1$Season) # counting how many seasons appear in each cluster
kmcount2 = table(km.cluster2$Season)
kmcount3 = table(km.cluster3$Season)

kmteam1 = table(km.cluster1$Team) # counting how many times each team appears in each cluster
kmteam2 = table(km.cluster2$Team)
kmteam3 = table(km.cluster3$Team)

print(kmcount1)
print(kmcount2)
print(kmcount3)
print(kmteam1)
print(kmteam2)
print(kmteam3)

par(mfrow = c(2,1))
# cluster 1 season frequency
kmcount1_df = as.data.frame(kmcount1)
colnames(kmcount1_df) = c("Team", "Season")
kmcount1_df$Team <- reorder(kmcount1_df$Team, kmcount1_df$Season, FUN = sum)

ggplot(kmcount1_df, aes(x = Team, y = Season)) +
  geom_bar(stat = "identity", fill = "#FF0000") +
  labs(title = "Season frequency in cluster 1",
       x = "Season",
       y = "Count") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6)
  )

# cluster 1 team frequency
kmteam1_df = as.data.frame(kmteam1)
colnames(kmteam1_df) = c("Team", "Count")
kmteam1_df$Team <- reorder(kmteam1_df$Team, kmteam1_df$Count, FUN = sum)

ggplot(kmteam1_df, aes(x = Team, y = Count)) +
  geom_bar(stat = "identity", fill = "#FF0000") +
  labs(title = "Team frequency in cluster 1",
       x = "Team",
       y = "Count") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6)
  )
