
# cat_clean_tags <- googlesheets4::read_sheet("1ZbnspgZDgNn_0NsHLCqtxVDMEdt4dkPNk_Dwv7xQfgA","category_clean_data")

library(dplyr)
library(stringr)
library(tm)

text_data <- readr::read_lines("~/Texts/text_acad_isi/text_acad_1990.txt")

text_data <- text_data %>%
  str_replace_all("@@[0-9]+ Section : ", "") %>%
  str_replace_all("<p>", " ") %>%
  str_replace_all(" {2,}", " ") %>%
  str_squish() %>%
  stripWhitespace()

text_data[4]

sentences <- unlist(strsplit(as.character(text_data), "(?<=\\.)\\s(?=[A-Z])", perl = T))

