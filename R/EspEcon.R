# EspEcon

library(dplyr)
library(FactoMineR)
library(factoextra)

ent_dir_paths <- c("C:/Users/gabri/Documents/R/Projects/PR_assoc/data/FIEP/19-23/summary.csv",
                   "C:/Users/gabri/Documents/R/Projects/PR_assoc/data/ACP/20-22/summary.csv",
                   "C:/Users/gabri/Documents/R/Projects/Federacoes_Ind/data/FIESP/22-25/summary.csv",
                   "C:/Users/gabri/Documents/R/Projects/Federacoes_Ind/data/FIESC/21-24/summary.csv",
                   "C:/Users/gabri/Documents/R/Projects/Federacoes_Ind/data/FIEG/19-22/summary.csv",
                   "C:/Users/gabri/Documents/R/Projects/Federacoes_Ind/data/FIEC/19-24/summary.csv",
                   "C:/Users/gabri/Documents/R/Projects/Federacoes_Ind/data/FIEAM/19-23/summary.csv")


read.csv("C:/Users/gabri/Documents/R/Projects/PR_assoc/data/FIEP/19-23/summary.csv", fileEncoding = "utf8", row.names = 1)
read.csv("C:/Users/gabri/Documents/R/Projects/PR_assoc/data/ACP/20-22/summary.csv", fileEncoding = "utf8", row.names = 1)

econ_df <- do.call(what = bind_rows, args = lapply(X = ent_dir_paths, FUN = read.csv, fileEncoding = "utf8", row.names = 1))

econ_df$N_Emp <- gsub("5 a 10 Emp", "6 a 10 Emp", econ_df$N_Emp)

econ_df$Maior_n_Func <- gsub(pattern = "10-49 Func", replacement = "20-99 Func", econ_df$Maior_n_Func) %>% 
  gsub(pattern = "50-199 Func", replacement = "100-199 Func", x = .) %>% gsub(pattern = "Até 9 Func", replacement = "Até 19 Func", x = .)

econ_df$Maior_Fat <- gsub("De 30 a 50M Fat|De 50 a 100M Fat", "Mais de 30M Fat", econ_df$Maior_Fat)

econ_df[c("Maior_n_Func", "Entidade")] <- lapply(econ_df[c("Maior_n_Func", "Entidade")], as.factor)     

econ_df$Maior_n_Func <- factor(x = econ_df$Maior_n_Func, levels = c("Até 19 Func", "20-99 Func", "100-199 Func", "Mais de 200 Func"))
econ_df$Maior_Fat <- factor(x = econ_df$Maior_Fat, levels = c("Até 240k Fat", "Até 2,4M Fat", "De 2,4 a 5M Fat", "De 5 a 10M Fat",
                                                    "De 10 a 30M Fat", "Mais de 30M Fat"))

dir_MCA_nvK <- MCA(X = econ_df[c(1:4, 17)], quali.sup = 5, graph = F)
dir_MCA_set <- MCA(X = econ_df[c("Transf", "Agr", "Comerc", "Serv", "Constr", "Entidade")], quali.sup = 6, graph = F)

fviz_mca_biplot(X = dir_MCA_nvK, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T, geom.ind = "point",
                title = "ACM 1: Nível de capital dos membros das federações da indústria selecionadas")
fviz_mca_var(X = dir_MCA_set, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T,
             title = "ACM 2: Atuação setorial dos membros das federações da indústria selecionadas")

nivk_ent <- -as.data.frame(dir_MCA_nvK$quali.sup$coord)[1]
transf_ent <- -as.data.frame(dir_MCA_set$quali.sup$coord)[2]

vocab_df_doc$NivK <- nivk_ent[vocab_df_doc$Entidade,]
vocab_df_doc$NivTransf <- transf_ent[vocab_df_doc$Entidade,]
