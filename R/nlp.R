# Natural language processing

if (!require("pacman")) install.packages("pacman")

# Import ####
p_load(
  quanteda, 
  readtext, 
  here, 
  dplyr
)

df_desc <- readtext(
  here::here("working", "inputs", "descriptions"), encoding = "UTF-8"
)

corp_desc <- df_desc %>%
  corpus()

summary(corp_desc)

tok_desc <- corp_desc %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = stopwords("en"), padding = TRUE)

print(tok_desc)

col_desc <- tok_desc %>%
  textstat_collocations() %>%
  filter(z > 3)

tok_comp_desc <- tok_desc %>%
  tokens_compound(col_desc)

dfm_desc <- tok_comp_desc %>%
  dfm()

print(dfm_desc)

dfm_desc %>%
  topfeatures()

fcm_desc <- dfm_desc %>%
  fcm() %>%
  topfeatures()

fcm_desc
