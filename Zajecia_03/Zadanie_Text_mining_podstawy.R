
# Instalacja i ładowanie wymaganych pakietów ----
# install.packages(c("tm", "wordcloud", "RColorBrewer", "ggplot2"))
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)


# Stworzenie funkcji do przetwarzania tekstu ----
process_text <- function(file_path) {
  # Wczytanie tekstu z pliku
  text <- tolower(readLines(file_path, encoding = "UTF-8"))
  # Usunięcie znaków interpunkcyjnych i cyfr
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  # Usunięcie stop słów angielskich
  text <- removeWords(text, stopwords("en"))
  # Podział tekstu na słowa
  words <- unlist(strsplit(text, "\\s+"))
  # Usunięcie pustych elementów
  words <- words[words != ""]
  return(words)
  
}


# Stworzenie funkcji do obliczania częstości występowania słów ----
word_frequency <- function(words) {
  freq <- table(words)
  freq_df <- data.frame(word = names(freq), freq = as.numeric(freq))
  freq_df <- freq_df[order(-freq_df$freq), ]
  return(freq_df)
}

# Stworzenie funkcji do tworzenia chmury słów ----
plot_wordcloud <- function(freq_df, title, color_palette = "Dark2") {
  wordcloud(words = freq_df$word, freq = freq_df$freq, min.freq = 16,
            colors = brewer.pal(8, color_palette))
  title(title)
}


# Przykładowe użycie funkcji ----


# Dwa pliki txt równocześnie ----
file_paths <- c("Biden2021.txt", "Biden2024.txt")
custom_stopwords <- c("’m","’ve","’re","’s","’d","’ll","’t","’nt", "—", "–")

for (file_path in file_paths) {
  words <- process_text(file_path)
  words <- words[!words %in% custom_stopwords]
  freq_df <- word_frequency(words)
  plot_wordcloud(freq_df, paste("Chmura słów -", file_path), "Dark2")
  cat("Najczęściej występujące słowa w pliku", file_path, ":\n")
  print(head(freq_df, 10))
  cat("\n")
  top10 <- head(freq_df, 10)
  print(
    ggplot(top10, aes(x = reorder(word, freq), y = freq)) +
      geom_col(fill = "steelblue", color = "white") +
      coord_flip() +
      labs(
        title = paste ("Częstość wystepowania top 10 słów -", file_path ),
        x = "Słowa",
        y = "Częstość"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
  )
}