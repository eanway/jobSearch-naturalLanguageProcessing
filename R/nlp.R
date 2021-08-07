# Natural language processing

if(!require(pacman)) install.packages("pacman"); library(pacman)

# Import ####
p_load(
  quanteda, 
  quanteda.textstats, 
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
  tokens_remove(pattern = stopwords("en"), padding = TRUE) %>%
  tokens_wordstem()

print(tok_desc)

col_desc <- tok_desc %>%
  textstat_collocations() %>%
  filter(z > 3)

head(col_desc)

tok_comp_desc <- tok_desc %>%
  tokens_compound(col_desc)

dfm_desc <- tok_comp_desc %>%
  dfm()

print(dfm_desc)

dfm_desc %>%
  topfeatures()

if(FALSE) {
  dfm_desc %>%
    dfm_subset(docnames(.) == "cytel.txt") %>%
    topfeatures(n = 10)
}


hclust_dfm_desc <- dfm_desc %>%
  textstat_dist() %>%
  as.dist() %>%
  hclust()

hclust_dfm_desc %>%
  plot()

cutree_dfm_desc <- hclust_dfm_desc %>%
  cutree(5)

cutree_dfm_desc

dfm_desc$cluster <- cutree_dfm_desc

docvars(dfm_desc)

fcm_desc <- dfm_desc %>%
  dfm_group(cluster) %>%
  fcm()

fcm_desc %>%
  topfeatures(20)

# Text frequency ####
tsfreq_desc <- dfm_desc %>%
  textstat_frequency()

tsfreq_desc_cluster <- dfm_desc %>%
  dfm_group(cluster) %>%
  textstat_frequency(groups = cluster)

p_load(
  tidyr
)

paste_phrases <- function(vec) {
  paste(vec, collapse = ", ")
}

df_desc <- tsfreq_desc %>%
  as_tibble() %>%
  select(rank, group, feature) %>%
  filter(rank <= 20) %>%
  pivot_wider(
    names_from = group, values_from = feature, 
    values_fn = paste_phrases
  )

df_desc_cluster <- tsfreq_desc_cluster %>%
  as_tibble() %>%
  select(rank, group, feature) %>%
  filter(rank <= 20) %>%
  pivot_wider(
    names_from = group, values_from = feature, 
    values_fn = paste_phrases
  )

# Term-frequency inverse document frequency ####
df_tfidf_desc <- dfm_desc %>%
  dfm_tfidf() %>%
  textstat_frequency(force = TRUE) %>%
  as_tibble() %>%
  select(rank, group, feature) %>%
  filter(rank <= 20) %>%
  pivot_wider(
    names_from = group, values_from = feature, 
    values_fn = paste_phrases
  )
