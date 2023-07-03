getwd()
setwd("C:\\Users\\LENOVO\\Desktop\\projet_R")
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("textclean")
installed.packages("ggplot2")
installed.packages("igraph")
installed.packages("ggraph")
install.packages("stringr")
library("readxl")
library("dplyr")
library("tidytext")
library("plyr")
library("sentimentr")
library("tm")
library("wordcloud")
library("RColorBrewer")
library("ggplot2")
library("tidyr")
library("igraph")
library("ggraph")
set.seed(2017)


# importation  de la base
df<-read_xlsx("C:\\Users\\LENOVO\\Desktop\\projet_R\\lait.xlsx")

#definition de la base 

base <- df %>% dplyr:: distinct(text, .keep_all = TRUE)
base <- df[1:100,] %>% dplyr::select(text)
                                     

##Numérotation des éléments
base1<-tibble(index_person=1:100, opinion=base$text)
View(base1)


##creation des tokens des mots indexés

base_mot<-base1 %>% 
  tidytext::unnest_tokens(word, opinion)

##Suppression des mots vides
base_mot<-base_mot%>%anti_join(stop_words)

#creations du dictionnaire des mots vides
dictionnaire<-data.frame(word =c("t.co","to","i","are","https","the","their","his","to","for","i","I","l","we","my","of","from","a","at","he","that","so","if","our"))
                         
texte_filtre<-anti_join(base_mot,dictionnaire,by="word")
ivi<-base_mot %>%
  dplyr::filter(!word %in%dictionnaire )
#SUpprimer les chiffres
texte_filtre<-texte_filtre %>% filter(!grepl("\\d",word))


## fréquence des mots
frequence_mot<-base_mot%>%dplyr::count(word,sort=TRUE)%>%
  filter(!word %in% dictionnaire$word) %>% 
  filter(n > 20)

##histogramme des mots
base_mot %>%
  dplyr::count(word, sort = TRUE) %>%filter(!word %in% dictionnaire$word) %>% 
  filter(n > 35) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  ylab(" mots")+
  xlab("fréquence des mots")+
  ggtitle("Histogramme des mots les plus fréquents")

##Mots intéressant
mot_inter<-frequence_mot[1:7,] %>% dplyr::select(word)

### nuages de mots avec tm
dtm <- TermDocumentMatrix(base_mot%>%dplyr::mutate(word=stringr::str_replace_all(.$word, "’", " ")) %>% filter(!word %in% dictionnaire$word))
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

head(d, 10)



set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 10000,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
title(main = "Nuages des mots fréquents")

#nuages des mots


# base2 <- base1 %>%
#   dplyr::mutate(text = stringr::str_replace_all(.$opinion, "’", " ")) 
# base2<- base2 %>%  tidytext::unnest_tokens(word, text) %>% 
#   filter(!word %in% dictionnaire$word) 
# base2 <- base2 %>%dplyr::count(word, sort = TRUE)%>% 
#   filter(n > 20)

base2 <- base1 %>%
  dplyr::mutate(text = stringr::str_replace_all(.$opinion, "’", " "))  %>%
  tidytext::unnest_tokens(word, text) %>% 
  filter(!word %in% dictionnaire$word) %>%
  dplyr::count(word, sort = TRUE)%>% 
  filter(n > 20)


wordcloud(base2$word, base2$n, max.words = 200, rot.per = FALSE, colors = c("#973232", "#1E5B5B", "#6D8D2F", "#287928"))
title(main = "Nuages des mots fréquents")


### Gestion des bigrams
bigram <- base1 %>%
  unnest_tokens(bigram, opinion, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))
bigrams_separated <-bigram %>%
  tidyr::separate(bigram, c("word1", "word2"), sep = " ") 
###Filtrage du bigramme
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% dictionnaire$word) %>%
  filter(!word2 %in% dictionnaire$word)

## recompter les éléments
bigram_counts <- bigrams_filtered %>%
  dplyr::count(word1, word2, sort = TRUE)

##Graphes des bigrams
graphe_bigram <- base1 %>%
  dplyr::mutate(text = stringr::str_replace_all(.$opinion, "’", " "))  %>%
  tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>% 
  dplyr::count(bigram, sort = TRUE)%>% 
  filter(n > 10)
wordcloud(graphe_bigram$bigram, graphe_bigram$n, max.bigram = 200, rot.per = FALSE, colors = c("#973232", "#1E5B5B", "#6D8D2F", "#287928"))
title(main = "Nuages des 2_GRAMM")

#____________________________________________________
##réseau de mots simples
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()
# 
# ##graphes proprement dit
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  xlab("mots")+
  ggtitle("Réseaux des mots simples")

###
#__________________________________________
##réseau de mots complexe
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()
set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()+
  xlab(" mots")+
  ggtitle("réseau de mots dirigés")

mi<-mot_inter$word

### spécification des mots fréquents 
#_____________________
mi<-mot_inter$word   ## mots fréquents
#__________________
##bigram des mots fréquents
base_lait<- bigrams_filtered %>% 
  dplyr:::filter( word2%in%mi) %>% 
  dplyr::count(word1, word2, sort = TRUE)

#____________________

##réseaux des mots fréquents

lait_graph <- base_lait %>%
  filter(n > 1) %>%
  graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(lait_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()+
  xlab(" mots")+
  ggtitle("réseau de mots fréquents dirigés")
##____________________________________________________

##Creer trigram
trigram<-base1 %>%
  unnest_tokens(trigram, opinion, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% dictionnaire$word,
         !word2 %in% dictionnaire$word,
         !word3 %in% dictionnaire$word) 

##filtrage du trigramme en fonction des mots sélectionner
moussa<-trigram %>% filter(word3==mi) %>% 
  dplyr::count(word1, word2, word3, sort = TRUE)

## réseau des trigrammes
### trigramm des mots fréquents
# base_lait_tri<- trigram %>% 
#   dplyr:::filter( word2%in%mi) %>% 
#   dplyr::count(word1, word2,word3, sort = TRUE)
# 
# lait_graph_tri <- base_lait_tri %>%
#   filter(n > 1) %>%
#   graph_from_data_frame()
# 
# ggraph(lait_graph_tri, layout = "fr") +
#   geom_edge_link() +
#   geom_node_point() +
#   geom_node_text(aes(label = name), vjust = 1, hjust = 1)





nouveau_df <- data.frame(base_mot[, 1])

# Parcourir chaque mot dans V et ajouter une colonne au nouveau data frame
for (mot in mot_inter$word) {
  nouveau_df[[mot]] <- as.numeric(mot %in% base_mot[, 2])
  
}

for (i in 1:nrow(mot_inter)) {
  for (j in 1:nrow(base_mot)){
   if(base_mot[j,2]==mot){
     nouveau_df[j,i+1]=1
   }
  }

}















  
  
  
nouveau_df$formula[1]
base_mot$word[2]
base_mot$word[1:4]
