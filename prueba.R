loadLibraries()
tweets_jose <-sentiment_analysis("@ladygaga", "@ArianaGrande")
print(tweets_jose[8])

sentiment_analysis <- function(user1, user2){
  #user1 <- "@JoseDAngelG"
  # user2 <- "@BernieSanders"
  # lang <- "english"
  
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
     app = "Practica1",
     consumer_key = "38H8QSJBnY9xryxqkDshoBi7Q",
     consumer_secret = "78ZGozfvlcJzY3IclARTdMtSEGCkxcb5QLyBRUWm4MRv3WR2x5",
     access_token = "1442475234913775619-2l5DmYvGVlXFYNbV4J6B5IpinNf9Nd",
     access_secret = "103NKYZidMCRkysiPXFNKg3wKNDfxwBKiTL9HlI4Quru3"
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
  
  datos_new2
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
  
  lista_stopwords <- stopwords("english")
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
  # tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>% spread(key = autor, value = n, fill = 0, drop = TRUE)
  # tweets_unpivot <- tweets_spread %>% gather(key = "autor", value = "n", -token)
  # 
  # # b) Selección de los autores Joe Biden y Bernie Sanders
  # tweets_unpivot <- tweets_unpivot %>% 
  #   filter(autor %in% c(user1clean, user2clean))
  # 
  # # c) Se añade el total de palabras de cada autor
  # tweets_unpivot <- tweets_unpivot %>% left_join(tweets_tidy %>% group_by(autor) 
  #                                                %>%summarise(N = n()), by = "autor")
  # tweets_unpivot
  # # d) Cálculo de odds y log of odds de cada palabra
  # tweets_logOdds <- tweets_unpivot %>% mutate(odds = (n + 1) / (N + 1))
  # 
  # tweets_logOdds
  # 
  # tweets_logOdds <- tweets_logOdds %>% select(autor, token, odds) %>% spread(key = autor, value = odds)
  # tweets_logOdds
  # 
  # # 
  # coluser1 <- tweets_logOdds[user1clean] 
  # coluser1
  # # 
  # coluser2 <- tweets_logOdds[user2clean]
  # coluser2
  # 
  # head(log(coluser1/coluser2))
  # # col3<-coluser1/coluser2
  # # col3%>% 
  # #   rename(
  # #      "log_odds" = "JoeBiden"
  # #   )
  # # 
  # # head(col3)
  # # names(col3)
  # # logOddscol <- log(col3)
  # # logOddscol
  # # # probarLog <- tweets_logOdds["JoeBiden"]/tweets_logOdds["BernieSanders"]
  # # # funcionLog <- log(probarLog)
  # # tweets_logOdds$log_odds <- logOddscol 
  # # tweets_logOdds$abs_log_odds <- abs(logOddscol)
  # # 
  # # names(tweets_logOdds)
  # 
  # 
  # 
  # 
  # tweets_logOdds <- tweets_logOdds %>%
  #   mutate(log_odds = log(JoeBiden/BernieSanders), abs_log_odds = abs(log_odds))
  # 
  # 
  # 
  # 
  # tweets_logOdds
  # tweets_logOdds <- tweets_logOdds %>% mutate(autor_frecuente = if_else(log_odds > 0,
  #                                                                       user1,user2))
  # tweets_logOdds %>% arrange(desc(abs_log_odds)) %>% head() 
  # a <- tweets_logOdds %>% group_by(autor_frecuente) %>% top_n(15, abs_log_odds)
  # 
  # # e) Graficar resultados
  # ggplot(a,aes(x = reorder(token, log_odds), y = log_odds, fill = autor_frecuente)) + 
  #   geom_col() + labs(x = "word", y = "log odds ratio") +
  #   coord_flip() + theme_bw()
  # 
  # # f) Representación gráfica de las frecuencias
  # tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  #   top_n(10, n) %>% arrange(autor, desc(n)) %>%
  #   ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  #   geom_col() +
  #   theme_bw() +
  #   labs(y = "", x = "") +
  #   theme(legend.position = "none") +
  #   coord_flip() +
  #   facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)
  
  #------------------------------------------------------------------------------------------
  #PRÁCTICA 12 - Clasificación de tweets:
  # Para poder aplicar algoritmos de clasificación a un texto, es necesario crear una
  # representación numérica del mismo
  
  tweets_user1_user1 <- tweets1 %>% filter(autor %in% c(user1clean, user2clean))
  set.seed(123)
  train <- sample(x = 1:nrow(tweets_user1_user1), size= 0.8*nrow(tweets_user1_user1))
  tweets_train <- tweets_user1_user1[train, ]
  tweets_test  <- tweets_user1_user1[-train, ]
  
  
  # Es importante verificar que la proporción de 
  # cada grupo es similar en el set de entrenamiento 
  # y en el de test.
  table(tweets_train$autor) / length(tweets_train$autor)
  table(tweets_test$autor) / length(tweets_test$autor)
  
  # Limpieza y tokenización de los documentos de entrenamiento
  tweets_train$texto <- tweets_train$texto %>% map(.f = limpiar_tokenizar) %>% map(.f = paste, collapse = " ") %>% 
    unlist()
  
  # Creación de la matriz documento-término
  matriz_tfidf_train <- dfm(x = tweets_train$texto, remove = lista_stopwords)
  
  # Se reduce la dimensión de la matriz eliminando aquellos 
  # términos que aparecen en menos de 5 documentos. Con esto se consigue eliminar ruido.
  matriz_tfidf_train <- dfm_trim(x = matriz_tfidf_train, min_docfreq = 5)
  
  # Conversión de los valores de la matriz a tf-idf
  matriz_tfidf_train <- dfm_tfidf(matriz_tfidf_train, scheme_tf = "prop",scheme_df = "inverse")
  matriz_tfidf_train
  
  # Limpieza y tokenización de los documentos de test
  tweets_test$texto <- tweets_test$texto %>% map(.f = limpiar_tokenizar) %>%map(.f = paste, collapse = " ") %>% unlist()
  
  # Identificación de las dimensiones de la matriz de entrenamiento
  # Los objetos dm() son de clase S4, se accede a sus elementos mediante @
  dimensiones_matriz_train <- matriz_tfidf_train@Dimnames$features
  
  # Conversión de vector a diccionario pasando por lista
  dimensiones_matriz_train <- as.list(dimensiones_matriz_train)
  names(dimensiones_matriz_train) <- unlist(dimensiones_matriz_train)
  dimensiones_matriz_train <- dictionary(dimensiones_matriz_train)
  
  dimensiones_matriz_train
  
  
  # Proyección de los documentos de test
  matriz_tfidf_test<-dfm(x = tweets_test$texto, dictionary = dimensiones_matriz_train)
  matriz_tfidf_test<-dfm_tfidf(matriz_tfidf_test, scheme_tf="prop",scheme_df = "inverse")
  matriz_tfidf_test
  
  #MODELO SVM
  # Como modelo de predicción se emplea una Máquina de soporte vectorial (SVM). 
  # Este método de aprendizaje estadístico suele dar buenos resultados en clasificación.
  # Ajuste del modelo
  modelo_svm <- svm(x = matriz_tfidf_train, y = as.factor(tweets_train$autor),
                    kernel = "linear", cost = 1, scale = TRUE, type = "C-classification")
  modelo_svm
  
  
  
  # Predicciones
  predicciones <- predict(object = modelo_svm, newdata = matriz_tfidf_test)
  
  # Error de predicción
  # Matriz de confusión
  table(observado = tweets_test$autor, predicho = predicciones)
  
  # Error de clasificación
  clasificaciones_erroneas <- sum(tweets_test$autor != predicciones)
  error <- 100 * mean(tweets_test$autor != predicciones)
  paste("Numero de clasificaciones incorrectas =", clasificaciones_erroneas)
  paste("Porcentaje de error =", round(error,2), "%")
  
  
  # Optimización de hiperparámetros
  # El método de SVM lineal tiene un único hiperparámetro C 
  # que establece la penalización por clasificación incorrecta, 
  # regulando así el balance entre bias y varianza. 
  # Al tratarse de un hiperparámetro, su valor óptimo 
  # no se aprende en el proceso de entrenamiento, 
  # para estimarlo hay que recurrir a validación cruzada.
  set.seed(369)
  svm_cv <- tune("svm", train.x = matriz_tfidf_train,
                 train.y = as.factor(tweets_train$autor),
                 kernel = "linear", ranges = list(cost = c(0.1, 0.5, 1, 2.5, 5)))
  summary(svm_cv)
  ggplot(data = svm_cv$performances, aes(x = cost, y = error)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = error - dispersion, ymax = error + dispersion)) +
    theme_bw()
  
  # Acorde al error estimado por validación cruzada, 
  # el valor óptimo del hiperparámetro C es 5. 
  # Se reajusta el modelo con este valor.
  svm_cv <- svm(x = matriz_tfidf_train, y = as.factor(tweets_train$autor),
                kernel = "linear", cost = 9, scale = TRUE)
  
  predicciones <- predict(object = svm_cv, newdata = matriz_tfidf_test)
  
  table(observado = tweets_test$autor, predicho = predicciones)
  
  clasificaciones_erroneas <- sum(tweets_test$autor != predicciones)
  
  error <- 100 * mean(tweets_test$autor != predicciones)
  paste("Numero de clasificaciones incorrectas =", clasificaciones_erroneas)
  paste("Porcentaje de error =", round(error,2), "%")
  
  
  
  
  #1) En este ejercicio se emplea la clasificación positivo/negativo proporcionada por el diccionario
  #bing.
  
  
  sentimientos <- get_sentiments(lexicon = "bing")
  head(sentimientos)
  sentimientos <- sentimientos %>% mutate(valor = if_else(sentiment == "negative", -1, 1))
  # sentimientos
  
  # Sentimiento promedio de cada tweet. Al disponer de los datos en formato tidy (una palabra por fila)
  # ,mediante un inner join se añade a cada palabra su sentimiento y se filtran automáticamente todas
  # aquellas palabras para las que no hay información disponible.
  tweets_sent <- inner_join(x = tweets_tidy, y = sentimientos, by = c("token" = "word"))
  tweets_sent
  
  #PRÁCTICA 13: ANÁLISIS DE SENTIMIENTOS
  # Una forma de analizar el sentimiento de un de un texto es considerando su sentimiento como 
  # la suma de los sentimientos de cada una de las palabras que lo forman. Esta forma consigue un buen
  # equilibro entre complejidad y resultados.
  
  # Para llevar a cabo esta aproximación es necesario disponer de un diccionario en el que se asocie
  # a cada palabra un sentimiento o nivel de sentimiento. A estos diccionarios también se les conoce 
  # como sentiment lexicon. 
  
  # El paquete tidytext contiene 3 diccionarios distintos:
  
  # AFINN: asigna a cada palabra un valor entre -5 y 5. Siendo -5 el máximo de negatividad y +5 el 
  # máximo de positividad.
  
  # bing: clasifica las palabras de forma binaria positivo/negativo.
  
  # nrc: clasifica cada palabra en uno o más de los siguientes sentimientos: positive, negative, 
  # anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.
  
  # ******************************************
  
  sentimientos <- get_sentiments(lexicon = "bing")
  #head(sentimientos)
  
  sentimientos <- sentimientos %>% mutate(valor = if_else(sentiment == "negative", -1, 1))
  sentimientos
  
  # Sentimiento promedio de cada tweet al disponer de los datos en formato tidy (una palabra por fila),
  # mediante un inner join se añade a cada palabra su sentimiento y se filtran automáticamente
  # todas aquellas palabras para las que no hay información disponible.
  tweets_sent <- inner_join(x = tweets_tidy, y = sentimientos, by = c("token" = "word"))
  tweets_sent
  
  
  # Se suman los sentimientos de las palabras que forman 
  # cada tweet.
  tweets_sent %>% group_by(autor, tweet_id) %>%
    summarise(sentimiento_promedio = sum(valor)) %>%
    head()
  
  # Porcentaje de tweets positivos, 
  # negativos y neutros por autor
  tweets_sent %>% group_by(autor, tweet_id) %>%
    summarise(sentimiento_promedio = sum(valor)) %>%
    group_by(autor) %>%
    summarise(positivos = 100 * sum(sentimiento_promedio > 0) / n(),
              neutros = 100 * sum(sentimiento_promedio == 0) / n(),
              negativos = 100 * sum(sentimiento_promedio < 0) / n())
  
  
  # Palabras positivas y negativas más usadas 
  # por cada uno de ellos 
  # A la columna Puntuacion se le asigna la palabra
  # positivas o negativa dependiendo de su valor,
  # esto se hace para facilitas la graficacion
  tweets_sent_2 <- tweets_sent
  tweets_sent_2 <-
    tweets_sent_2 %>%
    mutate(valor = ifelse(valor > 0, "positive", "negative"))
  
  neg_pos_plot <- tweets_sent %>% group_by(autor, tweet_id) %>%
    summarise(sentimiento_promedio = sum(valor)) %>%
    group_by(autor) %>%
    summarise(positivos = 100*sum(sentimiento_promedio > 0) / n(),
              neutros = 100*sum(sentimiento_promedio == 0) / n(),
              negativos = 100*sum(sentimiento_promedio  < 0) / n()) %>%
    ungroup() %>%
    gather(key = "sentimiento", value = "valor", -autor) %>%
    ggplot(aes(x = autor, y = valor, fill = sentimiento)) + 
    geom_col(position = "dodge", color = "black") + coord_flip() +
    theme_bw()
  
  neg_pos_plot
  
  
  
  pos_neg_timeplot <- tweets_sent %>% mutate(anyo = year(fecha),
                         mes = month(fecha),
                         anyo_mes = ymd(paste(anyo, mes, sep="-"),truncated=2)) %>%
    group_by(autor, anyo_mes) %>%
    summarise(sentimiento = mean(valor)) %>%
    ungroup() %>%
    ggplot(aes(x = anyo_mes, y = sentimiento, color = autor)) +
    geom_point() + 
    geom_smooth() + 
    labs(x = "fecha de publicación") +
    facet_wrap(~ autor, ncol = 1) +
    theme_bw() +
    theme(legend.position = "none")
  
  
  # Quitamos “no” de nuestras palabras.
  # Es una palabra muy comun en inglés que no necesariamente implica un sentimiento negativo.
  # Es la palabra negativa más frecuente entre los candidatos, por lo que podría sesgar
  # nuestros resultados.
  
  tweets_sent_2 <-
    tweets_sent_2 %>%
    filter(token != "no")
  
  # Palabras positivas:
  positive_words <- tweets_sent_2 %>%
    filter(sentiment == "positive") %>%
    group_by(autor) %>%
    count(token, sort = T) %>%
    top_n(n = 10, wt = n) %>%
    ggplot() +
    aes(token, n, fill = autor) +
    geom_col() +
    facet_wrap("autor", scales = "free") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = "Positive") +
    tema_graf
  
  
  # Palabras negativas:
  neg_words <- tweets_sent_2 %>%
    filter(sentiment == "negative") %>%
    group_by(autor) %>%
    count(token, sort = T) %>%
    top_n(n = 10, wt = n) %>%
    ggplot() +
    aes(token, n, fill = autor) +
    geom_col() +
    facet_wrap("autor", scales = "free") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = "Negative") +
    tema_graf
  
  # Como deseamos observar tendencias, vamos a obtener la media de sentimientos por día, 
  # usando group_by() y summarise() y asignamos los resultados a tuits_afinn_fecha
  tweets_sent_fecha <-tweets_sent
  
  tweets_sent_fecha <- tweets_sent_fecha %>% 
    separate(fecha, into = c("Fecha", "Hora"), sep = " ")
  
  tweets_sent_fecha <- tweets_sent_fecha %>% 
    separate(Fecha, into = c("Año", "Mes", "Dia"), sep = "-", remove = FALSE)
  
  tweets_sent_fecha_2 <-
    tweets_sent_fecha
  
  tweets_sent_fecha_2 <-
    tweets_sent_fecha_2 %>%
    group_by(tweet_id) %>%
    mutate(Suma = mean(valor)) %>%
    group_by(autor, Fecha) %>%
    summarise(Media = mean(valor))
  tweets_sent_fecha_2 <- tweets_sent_fecha_2 %>% mutate(Fecha = ymd(Fecha))
  
  # Veamos nuestros resultados con ggplot()
  tweets_sent_fecha_2 %>%
    ggplot() +
    aes(Fecha, Media, color = autor) +
    geom_line() +
    tema_graf +
    theme(legend.position = "top")
  
  
  # No nos dice mucho. Sin embargo, 
  # si separamos las líneas por candidato, 
  # usando facet_wrap(), será más fácil observar 
  # el las tendencias de los Candidatos.
  sentiments_date <- tweets_sent_fecha_2 %>%
    ggplot() +
    aes(Fecha, Media, color = autor) +
    geom_hline(yintercept = 0, alpha = .35) +
    geom_line() +
    facet_grid(autor~.) +
    tema_graf +
    theme(legend.position = "none")
  
  
  # PRÁCTICA 14
  #--------------------------------------------------- 
  # Word Clouds 
  # Otra forma visual de representar las palabras más frecuentes 
  # es mediante nubes de palabras (word clouds).  
  # En esta representación, las palabras más importantes  
  # tienen mayor tamaño. 
  
  df_grouped <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>% 
    group_by(autor) %>% mutate(frecuencia = n / n()) %>% 
    arrange(autor, desc(frecuencia)) %>% nest() %>% 
    filter(autor == user1clean)  
  
  options(warn=-1)
  
  # wordcloud_1 <- walk2(.x = user1clean, .y = df_grouped$data, .f = wordcloud_custom) 
  # wordcloud_2 <- walk2(.x = user2clean, .y = df_grouped$data, .f = wordcloud_custom)
  # 
  # # wordcloud(words = df_grouped$token, freq = df_grouped$frecuencia, 
  # #           max.words = 400, random.order = FALSE, rot.per = 0.35, 
  # #           colors = brewer.pal(8, "Dark2"))
  # 
  # 
  # wordcloud_1
  # 
  # 
  # # Word Cloud With Level: 
  # wordcloud1_lvl <- tweets_sent_2 %>% group_by(autor) %>% 
  #   filter(autor == user1clean) %>% 
  #   count(token, valor, sort = TRUE) %>% 
  #   acast(token ~ valor, value.var = "n", fill = 0) %>% 
  #   comparison.cloud(colors = c("indianred1", "lightseagreen"), 
  #                    max.words = 100)
  # 
  # wordcloud2_lvl <- tweets_sent_2 %>% group_by(autor) %>% 
  #   filter(autor == user2clean) %>% 
  #   count(token, valor, sort = TRUE) %>% 
  #   acast(token ~ valor, value.var = "n", fill = 0) %>% 
  #   comparison.cloud(colors = c("indianred1", "lightseagreen"), 
  #                    max.words = 100)
  
  #ANÁLISIS CON NRC
  # sentiments <- tokens %>% 
  #   inner_join(x = tweets_tidy, y = nrc, by = c(token = "word")) %>%
  #   count(sentiment, sort=T)
  # 
  # 
  # nrc <- sentiments %>% 
  #   ggplot(aes(x=reorder(sentiment, n), y=n)) +
  #   geom_bar(stat="identity", aes(fill=sentiment), show.legend=F) +
  #   geom_label(label=sentiments$n) +
  #   labs(x="Sentimiento", y="Frecuencia", title="Sentimiento predominante") +
  #   theme_bw()+
  #   coord_flip() + 
  #   theme(text = element_text(size = 20))
  
  return(list(p1, palabras_comunes, neg_pos_plot, pos_neg_timeplot,
              positive_words, neg_words, sentiments_date))
}

tweets <- sentiment_analysis("@ladygaga","@ArianaGrande")
# print(tweets[1])
# print(tweets[2])
# print(tweets[3])
# print(tweets[4])
print(tweets[1])
print(tweets[2])
#print(tweets[7])
print(tweets[3])
print(tweets[4])
print(tweets[5])
print(tweets[6])
print(tweets[7])

#plotTWDate, tweets_mes_anyo, wordCount, wordCountDist,

#tweets_sent, 

#wordcloud_1, wordcloud_2,
#wordcloud1_lvl, wordcloud2_lvl))



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

wordcloud_custom <- function(grupo, df){ 
  print(grupo) 
  wordcloud(words = df$token, freq = df$frecuencia, 
            max.words = 400, random.order = FALSE, rot.per = 0.35, 
            colors = brewer.pal(8, "Dark2")) 
}
