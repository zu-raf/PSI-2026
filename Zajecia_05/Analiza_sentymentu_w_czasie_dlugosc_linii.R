
# Analiza sentymentu w czasie ----

library(SentimentAnalysis)
library(ggplot2)
library(ggthemes)
library(tidyverse)



# Jeden ca³y plik ----

# Wczytanie danych tekstowych
# Odczytanie lokalnego pliku .txt
text <- readLines(file.choose(), encoding="UTF-8")



# Usuniêcie pustych wierszy
non_empty_lines <- text[nzchar(text)]


# Po³¹czenie wszystkich wierszy w jeden ci¹g znaków
full_text <- paste(non_empty_lines, collapse = " ")

# Usuniêcie zbêdnych spacji
full_text <- gsub("\\s+", " ", full_text)

# Funkcja do dzielenia tekstu na segmenty o okreœlonej d³ugoœci
split_text_into_chunks <- function(text, chunk_size) {
  start_positions <- seq(1, nchar(text), by = chunk_size)
  chunks <- substring(text, start_positions, start_positions + chunk_size - 1)
  return(chunks)
}

#install.packages("SnowballC")

# Podzielenie tekstu na segmenty
#
# ustaw min_lentgh jako jednolit¹ d³ugoœæ jednego segmentu
min_length <- 50
text_chunks <- split_text_into_chunks(full_text, min_length)


# Wyœwietlenie wynikowych segmentów
print(text_chunks)



# Analiza sentymentu przy u¿yciu pakietu SentimentAnalysis ----
sentiment <- analyzeSentiment(text_chunks)


# odkomentuj i zobacz parametry funkcji:
?analyzeSentiment




### S³ownik GI (General Inquirer) ----
#
# S³ownik ogólnego zastosowania
# zawiera listê s³ów pozytywnych i negatywnych
# zgodnych z psychologicznym s³ownikiem harwardzkim Harvard IV-4
# DictionaryGI


# Wczytaj s³ownik GI
data(DictionaryGI)
summary(DictionaryGI)


# Konwersja ci¹g³ych wartoœci sentymentu 
# na odpowiadaj¹ce im wartoœci kierunkowe 
# zgodnie ze s³ownikiem GI
sentimentGI <- convertToDirection(sentiment$SentimentGI)


# Wykres skumulowanego sentymentu kierunkowego
plot(sentimentGI)


# Analiza sentymentu przy u¿yciu pakietu SentimentAnalysis ----
# Analiza sentymentu przy u¿yciu pakietu SentimentAnalysis ----
# Ten sam wykres w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_GI <- data.frame(index = seq_along(sentimentGI), value = sentimentGI, Dictionary = "GI")

# Usuniêcie wierszy, które zawieraj¹ NA
df_GI <- na.omit(df_GI)

ggplot(df_GI, aes(x = value)) +
  geom_bar(fill = "green", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (GI)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()




### S³ownik HE (Henry’s Financial dictionary) ----
#
# zawiera listê s³ów pozytywnych i negatywnych
# zgodnych z finansowym s³ownikiem "Henry 2008"
# pierwszy, jaki powsta³ w wyniku analizy komunikatów prasowych 
# dotycz¹cych zysków w bran¿y telekomunikacyjnej i us³ug IT
# DictionaryHE


# Wczytaj s³ownik HE
data(DictionaryHE)
summary(DictionaryHE)


# Konwersja ci¹g³ych wartoœci sentymentu 
# na odpowiadaj¹ce im wartoœci kierunkowe 
# zgodnie ze s³ownikiem HE
sentimentHE <- convertToDirection(sentiment$SentimentHE)


# Wykres skumulowanego sentymentu kierunkowego
plot(sentimentHE)


# Ten sam wykres w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_HE <- data.frame(index = seq_along(sentimentHE), value = sentimentHE, Dictionary = "HE")

# Usuniêcie wierszy, które zawieraj¹ NA
df_HE <- na.omit(df_HE)

ggplot(df_HE, aes(x = value)) +
  geom_bar(fill = "blue", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (HE)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()




### S³ownik LM (Loughran-McDonald Financial dictionary) ----
#
# zawiera listê s³ów pozytywnych i negatywnych oraz zwi¹zanych z niepewnoœci¹
# zgodnych z finansowym s³ownikiem Loughran-McDonald
# DictionaryLM


# Wczytaj s³ownik LM
data(DictionaryLM)
summary(DictionaryLM)


# Konwersja ci¹g³ych wartoœci sentymentu 
# na odpowiadaj¹ce im wartoœci kierunkowe 
# zgodnie ze s³ownikiem LM
sentimentLM <- convertToDirection(sentiment$SentimentLM)


# Wykres skumulowanego sentymentu kierunkowego
plot(sentimentLM)


# Ten sam wykres w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_LM <- data.frame(index = seq_along(sentimentLM), value = sentimentLM, Dictionary = "LM")

# Usuniêcie wierszy, które zawieraj¹ NA
df_LM <- na.omit(df_LM)

ggplot(df_LM, aes(x = value)) +
  geom_bar(fill = "orange", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (LM)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()




### S³ownik QDAP (Quantitative Discourse Analysis Package) ----
#
# zawiera listê s³ów pozytywnych i negatywnych
# do analizy dyskursu


# Wczytaj s³ownik QDAP
qdap <- loadDictionaryQDAP()
summary(qdap)


# Konwersja ci¹g³ych wartoœci sentymentu 
# na odpowiadaj¹ce im wartoœci kierunkowe 
# zgodnie ze s³ownikiem QDAP
sentimentQDAP <- convertToDirection(sentiment$SentimentQDAP)


# Wykres skumulowanego sentymentu kierunkowego
plot(sentimentQDAP)


# Ten sam wykres w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_QDAP <- data.frame(index = seq_along(sentimentQDAP), value = sentimentQDAP, Dictionary = "QDAP")

# Usuniêcie wierszy, które zawieraj¹ NA
df_QDAP <- na.omit(df_QDAP)

ggplot(df_QDAP, aes(x = value)) +
  geom_bar(fill = "red", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (QDAP)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()



# Porównanie sentymentu na podstawie ró¿nych s³owników ----

# Minimalistycznie
# plot(convertToDirection(sentiment$SentimentGI))
# plot(convertToDirection(sentiment$SentimentHE))
# plot(convertToDirection(sentiment$SentimentLM))
# plot(convertToDirection(sentiment$SentimentQDAP))


# Wizualnie lepsze w ggplot2
# Po³¹czenie poszczególnych ramek w jedn¹ ramkê
df_all <- bind_rows(df_GI, df_HE, df_LM, df_QDAP)

# Tworzenie wykresu z podzia³em na s³owniki
ggplot(df_all, aes(x = value, fill = Dictionary)) +
  geom_bar(alpha = 0.7) + 
  labs(title = "Skumulowany sentyment wed³ug s³owników",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw() +
  facet_wrap(~Dictionary) +  # Podzia³ na cztery osobne wykresy
  scale_fill_manual(values = c("GI" = "green", 
                               "HE" = "blue", 
                               "LM" = "orange",
                               "QDAP" = "red" ))




# Agregowanie sentymentu z ró¿nych s³owników w czasie ----


# Sprawdzenie iloœci obserwacji
length(sentiment[,1])


# Utworzenie ramki danych
df_all <- data.frame(sentence=1:length(sentiment[,1]),
                     GI=sentiment$SentimentGI, 
                     HE=sentiment$SentimentHE, 
                     LM=sentiment$SentimentLM,
                     QDAP=sentiment$SentimentQDAP)



# USUNIÊCIE BRAKUJ¥CYCH WARTOŒCI
# gdy¿ wartoœci NA (puste) uniemo¿liwiaj¹ generowanie wykresu w ggplot
#

# Usuniêcie wartoœci NA
# Wybranie tylko niekompletnych przypadków:
puste <- df_all[!complete.cases(df_all), ]


# Usuniêcie pustych obserwacji
# np. dla zmiennej QDAP (wszystkie maj¹ NA)
df_all <- df_all[!is.na(df_all$QDAP), ]


# Sprawdzenie, czy wartoœci NA zosta³y usuniête
# wtedy puste2 ma 0 wierszy:
puste2 <- df_all[!complete.cases(df_all), ]
puste2




# Wykresy przedstawiaj¹ce ewolucjê sentymentu w czasie ----



ggplot(df_all, aes(x=sentence, y=QDAP)) +
  geom_line(color="red", size=1) +
  geom_line(aes(x=sentence, y=GI), color="green", size=1) +
  geom_line(aes(x=sentence, y=HE), color="blue", size=1) +
  geom_line(aes(x=sentence, y=LM), color="orange", size=1) +
  labs(x = "Oœ czasu zdañ", y = "Sentyment") +
  theme_gdocs() + 
  ggtitle("Zmiana sentymentu w czasie")



ggplot(df_all, aes(x=sentence, y=QDAP)) + 
  geom_smooth(color="red") +
  geom_smooth(aes(x=sentence, y=GI), color="green") +
  geom_smooth(aes(x=sentence, y=HE), color="blue") +
  geom_smooth(aes(x=sentence, y=LM), color="orange") +
  labs(x = "Oœ czasu zdañ", y = "Sentyment") +
  theme_gdocs() + 
  ggtitle("Zmiana sentymentu w czasie")




