loadLibraries()
tweets <- sentiment_analysis("@","@")
print(tweets[1])
print(tweets[2])
print(tweets[3])
print(tweets[4])
print(tweets[5])
print(tweets[6])

sentiment_analysis <- function(user1, user2, lang){
  user1 <- "@JoeBiden"
  user2 <- "@BernieSanders"
  lang <- "english"
  
  user1clean <- str_replace(user1, "@", "")
  user2clean <- str_replace(user2, "@", "")
  
  tema_graf <-
    theme_minimal() +
    theme(text = element_text(family = "serif"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#EBEBEB", colour = NA),
          legend.position = "none",
          legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))
  
   token <- create_token(
      app = "____",
      consumer_key = "____",
      consumer_secret = "____",
      access_token = "____",
      access_secret = "_____"
    )
  
  datos_new <- get_timeline(user=user2,
                            n = 500,
                            parse = TRUE,
                            check = FALSE,
                            include_rts = FALSE)
  datos_new2 <- get_timeline(user=user1,
                             n = 500, 
                             parse = TRUE, 
                             check = TRUE, 
                             include_rts = FALSE)
  
  
  tweets1 <- bind_rows(datos_new, datos_new2)
  # Mostrar el total de tweets extraídos 
  resumen <- tweets1 %>% group_by("screen_name") %>% summarise(numero_tweets = n()); resumen
  # Seleccionar y renombrar columnas
  tweets1 <- tweets1 %>% select(screen_name, created_at, status_id, text)
  tweets1 <- tweets1 %>% rename(autor = screen_name, fecha = created_at, texto = text, tweet_id = status_id)
  
  tweets1
  # tokenización a cada tweet
  tweets1 <- tweets1 %>% mutate(texto_tokenizado = map(.x = texto, .f = limpiar_tokenizar))
  tweets1 %>% select(texto_tokenizado) %>% head()
  
  tweets_tidy <- tweets1 %>% select(-texto) %>% unnest(cols = c(texto_tokenizado))
  tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
  
  plotTWDate <- ggplot(tweets1, aes(x = as.Date(fecha), fill = autor)) +
    geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "2 week") +
    labs(x = "Posting Date", y = "Number of tweets") +
    facet_wrap(~ autor, ncol = 1) + theme_bw() + theme(axis.text.x = element_text(angle = 90))
  
  plotTWDate
  
  tweets_mes_anyo <- tweets1 %>% mutate(mes_anyo = format(fecha, "%Y-%m"))
  tweets_mes_anyo <- tweets_mes_anyo %>% group_by(autor, mes_anyo) %>% summarise(n = n()) %>% ggplot(aes(x = mes_anyo, y = n, color 
                                                                                                         = autor)) +
    geom_line(aes(group = autor)) +
    labs(title = "Amount of posted tweets", x = "Posting Date",
         y = "Number of tweets") + theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 6),
          legend.position = "bottom")
  
  
  tweets_mes_anyo
  
  # 3) Conteo de palabras por usuario: 
  tweets_tidy %>% group_by(autor) %>% summarise(n = n()) 
  wordCount <- tweets_tidy %>% ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()
  
  tweets_tidy %>% select(autor, token) %>% distinct() %>% group_by(autor) %>% summarise(palabras_distintas = n()) 
  wordCountDist <- tweets_tidy %>% select(autor, token) %>% distinct() %>%
    ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()
  
  
  require(sm)
  
  lista_stopwords <- stopwords(lang)
  # Se añade el término amp al listado de stopwords
  lista_stopwords <- c(lista_stopwords, "amp")
  lista_stopwords
  
  
  #5) Correlación entre usuarios por palabras utilizadas:
  tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
    spread(key = autor, value = n, fill = NA, drop = TRUE)
  tweets_spread
  
  
  coluser1 <- pull(tweets_spread, user1clean)   
  coluser1
  
  coluser2 <- pull(tweets_spread, user2clean)
  coluser2
  
  cor.test(~ coluser1 + coluser2, method = "pearson", data = tweets_spread)
  
  p1 <- ggplot(tweets_spread, aes(coluser1, coluser2)) + geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, 
                                                                          height = 0.25) + geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) + 
    scale_x_log10(labels = percent_format()) + scale_y_log10(labels = percent_format()) +
    geom_abline(color = "red") + theme_bw() + theme(axis.text.x = element_blank(),
                                                    axis.text.y = element_blank())
  
  p1
  
  
  # 6) Conteo de palabras comunes entre dos usuarios: 
  palabras_comunes <- dplyr::intersect(tweets_tidy %>% filter(autor==user1clean) %>%
                                         select(token), tweets_tidy %>% filter(autor==user2clean) %>%
                                         select(token)) %>% nrow()
  
  paste("Número de palabras comunes entre ", user1clean," y ",user2clean, " = ",palabras_comunes)
  
  
  
  
  
  # 7) Comparación en el uso de palabras:
  
  # a) Pivotaje y despivotaje
  tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>% spread(key = autor, value = n, fill = 0, drop = TRUE)
  tweets_unpivot <- tweets_spread %>% gather(key = "autor", value = "n", -token)
  
  # b) Selección de los autores Joe Biden y Bernie Sanders
  tweets_unpivot <- tweets_unpivot %>% 
    filter(autor %in% c(user1clean, user2clean))
  
  # c) Se añade el total de palabras de cada autor
  tweets_unpivot <- tweets_unpivot %>% left_join(tweets_tidy %>% group_by(autor) 
                                                 %>%summarise(N = n()), by = "autor")
  tweets_unpivot
  # d) Cálculo de odds y log of odds de cada palabra
  tweets_logOdds <- tweets_unpivot %>% mutate(odds = (n + 1) / (N + 1))
  
  tweets_logOdds
  
  tweets_logOdds <- tweets_logOdds %>% select(autor, token, odds) %>% spread(key = autor, value = odds)
  tweets_logOdds
  
  # 
  coluser1 <- tweets_logOdds[user1clean] 
  coluser1
  # 
  coluser2 <- tweets_logOdds[user2clean]
  coluser2
  
  head(log(coluser1/coluser2))
  # col3<-coluser1/coluser2
  # col3%>% 
  #   rename(
  #      "log_odds" = "JoeBiden"
  #   )
  # 
  # head(col3)
  # names(col3)
  # logOddscol <- log(col3)
  # logOddscol
  # # probarLog <- tweets_logOdds["JoeBiden"]/tweets_logOdds["BernieSanders"]
  # # funcionLog <- log(probarLog)
  # tweets_logOdds$log_odds <- logOddscol 
  # tweets_logOdds$abs_log_odds <- abs(logOddscol)
  # 
  # names(tweets_logOdds)
  
  
  
  
  tweets_logOdds <- tweets_logOdds %>%
    mutate(log_odds = log(coluser1/coluser2), abs_log_odds = abs(log_odds))
  
  tweets_logOdds
  tweets_logOdds <- tweets_logOdds %>% mutate(autor_frecuente = if_else(log_odds > 0,
                                                                        user1,user2))
  tweets_logOdds %>% arrange(desc(abs_log_odds)) %>% head() 
  a <- tweets_logOdds %>% group_by(autor_frecuente) %>% top_n(15, abs_log_odds)
  
  # e) Graficar resultados
  ggplot(a,aes(x = reorder(token, log_odds), y = log_odds, fill = autor_frecuente)) + 
    geom_col() + labs(x = "word", y = "log odds ratio") +
    coord_flip() + theme_bw()
  
  # f) Representación gráfica de las frecuencias
  tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
    top_n(10, n) %>% arrange(autor, desc(n)) %>%
    ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
    geom_col() +
    theme_bw() +
    labs(y = "", x = "") +
    theme(legend.position = "none") +
    coord_flip() +
    facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)
  
  
  
  return(list(plotTWDate, tweets_mes_anyo, wordCount, wordCountDist, p1, palabras_comunes))
}

limpiar_tokenizar <- function(texto){
  nuevo_texto <- tolower(texto) # 1
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "") # 2
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ") # 3
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ") # 4
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ") # 5
  nuevo_texto <- str_replace_all(nuevo_texto, "[\\U0001f100-\\U0001ffff]", " ")#emojis
  nuevo_texto <- str_replace_all(nuevo_texto, "[\\u200D-\\u2fff]", " ")#emojis
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]] #6
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

