
# remove small groups in training data
# create an 'other' group
# stem words or lemmatisation
# 
# top_cat <- c('The mind', 'Emotions, mood', 'Language', 'Plants', 'Armed hostility', 'Faith', 'Life', 'Attention, judgment, curiosity', 'Communication', 'Animals')
top_cat <- c('The mind', 'Emotions, mood', 'Language')

library(keras)
library(tensorflow)
library(tm)
library(dplyr)
cat_clean_tags <- googlesheets4::read_sheet("1ZbnspgZDgNn_0NsHLCqtxVDMEdt4dkPNk_Dwv7xQfgA","category_clean_data") %>% 
  mutate(semantic_tag_category = if_else(semantic_tag_category %in% top_cat, semantic_tag_category , NA_character_)) %>% drop_na()

# cat_clean_tags %>% distinct(semantic_tag_category) %>% pull() # there are 37 categories
# cat_clean_tags %>% distinct(words) %>% pull() %>% length()# for 5186 words
# cat_clean_tags %>% distinct(stem_words) %>% pull() %>% length()# for 4220 stem words


# get words with 1 category
tag_ids <- cat_clean_tags %>% 
  mutate(semantic_tag_category = if_else(semantic_tag_category %in% top_cat, semantic_tag_category ,NA_character_)) %>% 
  distinct(semantic_tag_category) %>% 
  mutate(tag_id = row_number())

cat_clean_tags_single <- inner_join(cat_clean_tags, tag_ids, by = 'semantic_tag_category') %>% 
  group_by(stem_words) %>% #or words
  mutate(n = n()) %>% ungroup() %>% 
  filter(n == 1)

cat_clean_tags_single %>% distinct(semantic_tag_category) %>% pull() # there are 37 categories
cat_clean_tags_single %>% distinct(words) %>% pull() %>% length()# for 3982 words
cat_clean_tags_single %>% distinct(stem_words) %>% pull() %>% length()# for 2700 words

terms_data <- tibble(stem_words = tolower(tdm_data_small$dimnames$Terms))

comb <- left_join(terms_data, cat_clean_tags_single, by = 'stem_words') %>% mutate(row_id = row_number()) %>% drop_na()

comb %>% group_by(semantic_tag_category) %>% count() %>% View()
# top_comb <- comb %>% group_by(semantic_tag_category) %>% count() %>% drop_na() %>% arrange(-n) %>% head(10) %>% pull(semantic_tag_category)

# comb %>% mutate(semantic_tag_category = if_else(semantic_tag_category %in% top_comb, semantic_tag_category ,'Other')) %>% View()

# training data set up
training_id <- comb %>% filter(!is.na(tag_id)) %>% pull(row_id)
train_x <- as.matrix(cooc_scale[training_id,])
dim(train_x)
train_x[1:10, 1:10]
train_y_raw <- comb %>% filter(!is.na(tag_id)) %>% pull(tag_id)
train_y <- to_categorical(train_y_raw, 4) #one hot
dim(train_x)

# testing data set up
testing_id  <- comb %>% filter( is.na(tag_id))%>% pull(row_id)
test_x <- cooc_scale[testing_id,]
dim(test_x)

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = 8647) %>%
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 25, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 4, activation = 'softmax')

model %>% compile(
  optimizer = 'rmsprop',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

history <- model %>% fit(
  train_x, train_y, 
  epochs = 40, 
  batch_size = 100,
  validation_split = 0.2
)
