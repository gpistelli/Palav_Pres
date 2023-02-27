## GDA

library(FactoMineR)
library(factoextra)

PCA_vocab <- PCA(X = vocab_df, graph = F)

summary(PCA_vocab)

fviz_nbclust()

fviz_contrib(X = PCA_vocab, axes = 1, choice = "var", top = 10)
fviz_contrib(X = PCA_vocab, axes = 2, choice = "var", top = 10)
fviz_contrib(X = PCA_vocab, axes = 3, choice = "var", top = 10)
fviz_contrib(X = PCA_vocab, axes = 4, choice = "var", top = 10)

fviz_pca_biplot(PCA_vocab, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T)
fviz_pca_biplot(PCA_vocab, axes = c(3, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T)

###

vocab_df_sca <- scale(vocab_df)

fviz_nbclust(x = vocab_df_sca, FUNcluster = kmeans, method = "silhouette")

vocab_kmeans <- kmeans(x = vocab_df_sca, centers = 2)

fviz_cluster(object = vocab_kmeans, data = vocab_df_sca)

###

vocab_df_doc$NivK <- nivk_ent[vocab_df_doc$Entidade,]
vocab_df_doc$NivTransf <- transf_ent[vocab_df_doc$Entidade,]

PCA_vocab_doc <- PCA(X = vocab_df_doc, quali.sup = ncol(vocab_df_doc)-2, quanti.sup = c(ncol(vocab_df_doc)-1, ncol(vocab_df_doc)), graph = F)

summary(PCA_vocab_doc)

fviz_contrib(X = PCA_vocab_doc, choice = "var", axes = 1, top = 10)
fviz_contrib(X = PCA_vocab_doc, choice = "var", axes = 2, top = 10)
fviz_contrib(X = PCA_vocab_doc, choice = "var", axes = 3, top = 10)
fviz_contrib(X = PCA_vocab_doc, choice = "var", axes = 4, top = 10)
fviz_contrib(X = PCA_vocab_doc, choice = "var", axes = 5, top = 30)

pca_1x2_plot <- fviz_pca_biplot(PCA_vocab_doc, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T,
                geom.var = "text", select.var = list(contrib = 50), alpha.ind = 0.5, col.ind = "blue", geom.ind = "point")

pca_3x4_plot <- fviz_pca_biplot(PCA_vocab_doc, axes = c(3, 4), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T,
                geom.var = "text", select.var = list(contrib = 50), alpha.ind = 0.5, col.ind = "blue", geom.ind = "point")

pca_4x5_plot <- fviz_pca_biplot(PCA_vocab_doc, axes = c(5, 4), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T,
                                geom.var = "text", select.var = list(contrib = 50), alpha.ind = 0.5, col.ind = "blue", geom.ind = "point")

pca_1x4_plot <- fviz_pca_biplot(PCA_vocab_doc, axes = c(1, 4), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T,
                                geom.var = "text", select.var = list(contrib = 50), alpha.ind = 0.5, col.ind = "blue", geom.ind = "point")

# pca_quali.sup_plot <- ggplot(data = pca_quali.sup_coord, mapping = aes(x = Dim.1, y = Dim.3, label = Ent)) + geom_text()

pca_quali.sup_coord <- as.data.frame(PCA_vocab_doc$quali.sup$coord)
pca_quali.sup_coord$Ent <- row.names(pca_quali.sup_coord)

pca_quanti.sup_coord <- as.data.frame(PCA_vocab_doc$quanti.sup$coord)

# fviz_pca_biplot(PCA_vocab_doc, axes = c(1, 3), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T,
#                geom.var = "text", select.var = list(contrib = 50))


fviz_pca_ind(PCA_vocab_doc, habillage = ncol(vocab_df_doc))

fviz_add(ggp = fviz_add(ggp = pca_1x2_plot, df = pca_quali.sup_coord, axes = c(1, 2), geom = "text", color = "black", alpha.ind = 0.5),
         df = pca_quanti.sup_coord, axes = c(1, 2), geom = "text", color = "darkgreen", alpha.ind = 0.5)

fviz_add(ggp = fviz_add(ggp = pca_3x4_plot, df = pca_quali.sup_coord, axes = c(3, 4), geom = "text", color = "black", alpha.ind = 0.5),
         df = pca_quanti.sup_coord, axes = c(3, 4), geom = "text", color = "darkgreen", alpha.ind = 0.5, repel = T)

fviz_add(ggp = fviz_add(ggp = pca_4x5_plot, df = pca_quali.sup_coord, axes = c(5, 4), geom = "text", color = "black", alpha.ind = 0.5),
         df = pca_quanti.sup_coord, axes = c(5, 4), geom = "text", color = "darkgreen", alpha.ind = 0.5)

fviz_add(ggp = fviz_add(ggp = pca_1x4_plot, df = pca_quali.sup_coord, axes = c(1, 4), geom = "text", color = "black", alpha.ind = 0.5),
         df = pca_quanti.sup_coord, axes = c(1, 1), geom = "text", color = "darkgreen", alpha.ind = 0.5)

fviz_add(ggp = pca_1x3_plot, df = pca_quali.sup_coord, axes = c(1, 3), geom = "text", color = "black", alpha.ind = 0.5, repel = T)
fviz_add(ggp = pca_4x5_plot, df = pca_quali.sup_coord, axes = c(5, 4), geom = "text", color = "black", alpha.ind = 0.5, repel = T)

pca_quali.sup_coord <- as.data.frame(PCA_vocab_doc$quali.sup$coord)
pca_quali.sup_coord$Ent <- row.names(pca_quali.sup_coord)

ggplot(data = pca_quali.sup_coord, mapping = aes(x = Dim.1, y = Dim.3, label = Ent)) + geom_text()

PCA_vocab_doc$quali.sup$coord



plot(as.data.frame(PCA_vocab_doc$quali.sup$coord)$`Dim.1`,
     as.data.frame(PCA_vocab_doc$quali.sup$coord)$`Dim.2`,
     xlab = "Dim 1",
     ylab = "Dim 2",
     type = "n")
text(as.data.frame(PCA_vocab_doc$quali.sup$coord)$`Dim.1`,
     as.data.frame(PCA_vocab_doc$quali.sup$coord)$`Dim.2`,
     row.names(PCA_vocab_doc$quali.sup$coord)
)
clip(x1 = -4.8, x2 = 5.1, y1 = -5.6, y2 = 5)
abline(v = 0, h = 0)

plot(as.data.frame(PCA_vocab_doc$quali.sup$coord)$`Dim.1`,
     as.data.frame(PCA_vocab_doc$quali.sup$coord)$`Dim.3`,
     xlab = "Dim 1",
     ylab = "Dim 3",
     type = "n")
text(as.data.frame(PCA_vocab_doc$quali.sup$coord)$`Dim.1`,
     as.data.frame(PCA_vocab_doc$quali.sup$coord)$`Dim.3`,
     row.names(PCA_vocab_doc$quali.sup$coord)
)
clip(x1 = -4.8, x2 = 5.1, y1 = -5.6, y2 = 5)
abline(v = 0, h = 0)

fviz_pca_biplot(PCA_vocab_doc, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T,
                geom.var = "text", select.var = list(contrib = 50))
