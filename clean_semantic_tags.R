
# install.packages("googlesheets4")
library(googlesheets4)

sem_tags <- read_sheet("1ZbnspgZDgNn_0NsHLCqtxVDMEdt4dkPNk_Dwv7xQfgA","raw_data")

library(dplyr)
library(tidyr)

sem_tags_clean <- sem_tags %>% 
    unite(comb, Code, Explanation, sep = ' ', na.rm = T) %>% 
    mutate(id_name = if_else(grepl('^..:', comb), 'semantic_tag', 'words')) %>% 
    group_by(id_name) %>% 
    mutate(id = row_number()) %>% 
    pivot_wider(names_from = id_name, values_from = comb) %>% 
    mutate(
        words = gsub('[0-9]', '', words),
        words = gsub('  ', ', ', words)
    ) %>% 
    separate_rows(words, sep = ', ') %>% 
    mutate(words = trimws(words)) %>% 
    select(-id)

cat_sem_tags <- sem_tags_clean %>% 
    mutate(semantic_tag_category = gsub('[A-Z]{2}:[0-9]{2}? ', '', semantic_tag)) %>% 
    mutate(semantic_tag_category = gsub(':(.*)', '', semantic_tag_category)) %>% 
    select(semantic_tag_category, words) %>% distinct() #distinct removes about 700 responses
    
googlesheets4::write_sheet(cat_sem_tags, "1ZbnspgZDgNn_0NsHLCqtxVDMEdt4dkPNk_Dwv7xQfgA", "category_clean_data")
googlesheets4::write_sheet(sem_tags_clean, "1ZbnspgZDgNn_0NsHLCqtxVDMEdt4dkPNk_Dwv7xQfgA", "subcategory_clean_data")
