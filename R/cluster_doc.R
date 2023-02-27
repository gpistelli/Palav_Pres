library(cluster)
library(factoextra)

###
library(FactoMineR)

PCA_vocab_doc <- PCA(X = vocab_df_doc, scale.unit = T, graph = F)

summary(PCA_vocab_doc)

fviz_pca_biplot(PCA_vocab_doc, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T,
                geom.var = "text")

###

vocab_df_doc <- scale(vocab_df_doc)

fviz_nbclust(x = vocab_df_doc, FUNcluster = kmeans, method = "silhouette")

vocab_doc_kmeans <- kmeans(x = vocab_df_doc, centers = 3, iter.max = 1000, nstart = 100)

View(vocab_doc_kmeans$centers)

fviz_cluster(object = vocab_doc_kmeans,
             data = vocab_df_doc)

knitr::kable(data.frame(Clust1 = names(sort(vocab_doc_kmeans$centers[1,], decreasing = T)[1:20]),
                        Clust2 = names(sort(vocab_doc_kmeans$centers[2,], decreasing = T)[1:20]),
                        Clust3 = names(sort(vocab_doc_kmeans$centers[3,], decreasing = T)[1:20])
                       )
            )

prop.table(table(df$Entidade, vocab_doc_kmeans$cluster), 1)*100

cbind(df$Entidade, vocab_doc_kmeans$cluster)
