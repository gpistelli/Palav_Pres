library(GeomContentAnalysis)
library(dplyr)
library(wordcloud)
library(tm)
library(ptstem)

df <- read.csv("textos2.csv", fileEncoding = "utf8")
# df <- bind_rows(df, FIEAM)

entidades <- unique(df$Entidade)
entidades <- entidades[-grep("ACP", entidades)]

stopuordis <- unique(c(stopwords("pt"), "ainda", "par", "-", "–", "pod", "pel", "tambem", "outr", "fiesp", "fiec", "outr", "noss",
                "mai", "est", "entr", "iss", "ess", "sao", "paulo", "ceará", "mesm", "ter", "traz", "dest", "cearens", "nest",
                "revist", "aind", "edic", "mater", "mostr", "sobr", "ness", "nov", "nao", "ser", "tod", "ano", "acp", "alem",
                "dev", "entreg", "inici", "sempr", "dess", "apresent", "analis", "cad", "coloc", "fiep", "paran", "parana", "ate",
                "utiliz", "vem", "tud", "vez", "realiz", "pass", "algum", "precis", "–", "fieam", "amazonas", "associação", "comercial",
                "revista", "indústria", "paraná", "manaus", "indústrias", "goiás", "santa", "catarina", "fiesc", "fieg", "ama",
                "goi", "sant", "catarin", "manau", "sistem", "sej", "paulist", "goian", "paran", "hoj", "dia", "curitib",
                "edson", "élcio", "elisa", "queiroz", "batista", "”", "carlos", "cariri", "faz", "alencarin", "apesar", "maia",
                "amorim", "júnior", "minh", "diss", "além", "alg", "carlo", "aidar", "tenh", "tant", "apont", "quer", "alberto",
                "sistema", "quant", "desd", "cerc", "curitiba", "tiv", "segund", "aquil", "quer", "barão", "vicpresid", "tenh",
                "pereira", "oliveira", "neto", "telles", "cni", "federação", "sesi", "senai", "enai", "vilaça", "assim", "muit",
                "junh", "agost", "mai", "marc", "jan", "fever", "julh", "setembr", "outubr", "novembr", "dezembr", "batist", "elci",
                "net", "elis", "albert", "oliv", "bara", "carl", "paul", "skaf", "porem", "terceir", "fic", "zao", "per", "ger", "aca",
                "cas", "prim")
                 )

ent_corpus <- list()
for (i in 1:length(entidades)){
ent_corpus[[i]] <- df %>% filter(Entidade == entidades[i]) %>% .$Texto %>% tolower() %>% ptstem::ptstem(texts = ., complete = F) %>% 
  gsub(pattern = "–", replacement = "-", x = .) %>% gsub("”", "", x = .) %>% GCA_get_corpus(vec = ., stopwords_chos = stopuordis)
}

GCA_get_main_terms_alt <- function(fir_mx, prop = 0.9){
  fir_term_freq <- apply(fir_mx, 2, sum)
  fir_term_freq <- fir_term_freq[which(fir_term_freq > quantile(x = fir_term_freq, probs = prop))]
  return(names(fir_term_freq))
}

ent_corpus_doc <- df %>% .$Texto %>% tolower() %>% ptstem::ptstem(texts = ., complete = F) %>% 
    gsub(pattern = "–", replacement = "-", x = .) %>% gsub("”", "", x = .) %>% 
    lapply(X = ., FUN = GCA_get_corpus, stopwords_chos = stopuordis)

ent_corpus_doc2 <- df %>% .$Texto %>% .[c(1:3, 5:nrow(df))] %>% tolower() %>% ptstem::ptstem(texts = ., complete = F) %>% 
  gsub(pattern = "–", replacement = "-", x = .) %>% gsub("”", "", x = .) %>% 
  lapply(X = ., FUN = GCA_get_corpus, stopwords_chos = stopuordis)

main_terms <- unique(unlist(lapply(ent_corpus, GCA_get_main_terms)))
main_terms

main_terms_doc <- unique(unlist(lapply(ent_corpus_doc2, GCA_get_main_terms_alt)))
main_terms_doc

vocab_df <- lapply(X = ent_corpus, FUN = GCA_get_main_terms_prop, main_terms = main_terms) %>% do.call(what = bind_rows, args = .)
row.names(vocab_df) <- entidades

vocab_df_doc <- lapply(X = ent_corpus_doc2, FUN = GCA_get_main_terms_prop, main_terms = main_terms_doc) %>% do.call(what = bind_rows, args = .)
vocab_df_doc <- as.data.frame(vocab_df_doc)
row.names(vocab_df_doc) <- row.names(df)[-4]
vocab_df_doc$Entidade <- df$Entidade[-4]

main_terms_sel <- names(sort(apply(X = vocab_df_doc[1:241], MARGIN = 2, FUN = mean), decreasing = T)[1:50])

vocab_df_doc <- vocab_df_doc[, c(main_terms_sel, "Entidade")]

# Wordclouds

library(wordcloud)
library(dplyr)
library(gridExtra)

entidades <- unique(df$Entidade)
for (i in 1:length(entidades)){

test_a <- c(abubleble = 1, abcdefg = 0)
for (j in grep(entidades[i], df$Entidade)){
test_a <- bind_rows(test_a,
          setNames(nm = ent_corpus_doc[[j]]$dimnames$Terms, object = ent_corpus_doc[[j]]$v))
}

test_a[1:2] <- NULL
test_a[is.na(test_a)] <- 0
test_a <- apply(test_a, 2, sum)

assign(x = paste0(entidades[i], "_words"), value =  test_a)
}

for (i in 1:length(entidades)){
png(filename = paste0(entidades[i], "_wordcloud.png"))
print(wordcloud(names(get(paste0(entidades[i], "_words"))), get(paste0(entidades[i], "_words")), random.order = F, random.color = F, max.words = 30, colors = c("#00AFBB", "#E7B800", "#FC4E07")))
dev.off()
}

png

grid.arrange(wordcloud(names(get(paste0(entidades[i], "_words"))), get(paste0(entidades[i], "_words")), random.order = F, random.color = F, max.words = 30, colors = c("#00AFBB", "#E7B800", "#FC4E07")),
             wordcloud(names(FIEP_words), FIEP_words, random.order = F, random.color = F, max.words = 30, colors = c("#00AFBB", "#E7B800", "#FC4E07")))
