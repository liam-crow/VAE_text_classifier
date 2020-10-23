
# cat_clean_tags <- googlesheets4::read_sheet("1ZbnspgZDgNn_0NsHLCqtxVDMEdt4dkPNk_Dwv7xQfgA","category_clean_data")

library(dplyr)
library(stringr)
library(tm)
library(uwot)
library(Matrix)

# text_data <- readr::read_lines("~/Texts/text_acad_isi/text_acad_1990.txt")
text_data <- readr::read_lines("D:/Corpus/Texts/text_acad_isi/text_acad_1990.txt")

text_data_clean <- text_data %>% 
  str_replace_all("@@[0-9]+ Section : ", "") %>%
  str_replace_all("<p>", ".") %>%
  str_remove_all("@") %>%
  removeNumbers() %>% 
  str_replace_all("\\. \\.",".") %>% 
  str_squish() %>%
  stripWhitespace()

text_data_clean[4]

sentences <- unlist(strsplit(as.character(text_data_clean), "(?<=\\.)\\s(?=[A-Z])", perl = T)) %>% 
  removePunctuation() %>% 
  str_replace_all(" {2,}", " ") %>% 
  trimws()

sentences[7]

corpus <- VCorpus(VectorSource(sentences))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument, language = "english")  
tdm_data <- TermDocumentMatrix(corpus)

#remove sparse stuff
tdm_data_small <- removeSparseTerms(tdm_data, 0.9999)

tdm_sparse <- Matrix::sparseMatrix(
  i = tdm_data_small$i,
  j = tdm_data_small$j,
  x = tdm_data_small$v
)

cooc <- tdm_sparse %*% t(tdm_sparse)
diag(cooc) <- 0



cooc_bin <- matrix(ifelse(cooc > 0,1,0), dim(cooc))

cooc_scale <- scale(cooc)

cooc_bin[1:10,1:10]
cooc_scale[1:10,1:10]
cooc[1:10,1:10]
dim(cooc)
dim(cooc_bin)
# tdm_sparse <- Matrix::sparseMatrix(
#   i = tdm_data$i,
#   j = tdm_data$j,
#   x = tdm_data$v
# )

# tdm_sparse_dist <- wordspace::dist.matrix(
#   tdm_sparse,
#   method = 'cosine'
# )


# dim(tdm_sparse_dist)
# tdm_sparse_dist[1:10,1:10]
# 
# tdm_umap <- uwot::umap(tdm_sparse_dist, n_components = 2)
# 
# plot(tdm_umap[,1],tdm_umap[,2])


