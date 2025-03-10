acp <- PCA(games_clean, scale.unit = TRUE, ncp = 3, graph = FALSE)

## Tracé des valeurs propres ----
X11()
barplot(acp$eig[,1])
fviz_eig(acp) #variance expliquée, environ 70% dim 1&2

#projection des individus

fviz_pca_ind(acp, geom.ind = "point", col.ind = "blue",
             addEllipses = TRUE, ellipse.type = "confidence",
             repel = TRUE)

#contribution des variables
fviz_pca_var(acp, col.var = "contrib", repel = TRUE)

#Dim1 semble principalement influencé par les évaluations des utilisateurs (positives et négatives).
#Dim2 est dominé par des variables économiques et de popularité, notamment le prix et le pic de joueurs simultanés.

#pas de liens clair avec average playtime


PCAshiny(X = acp) #trop lourd pour mon pc..

summary(acp)          

# dim 1 Positive (cos2 = 0.767, 45.9%) et Negative (cos2 = 0.674, 40.3%) 
#dim 2 Peak.CCU (44.1%) et Price (40.2%) majoritairement

#average playtime reste tres proche du centre et peu correlés