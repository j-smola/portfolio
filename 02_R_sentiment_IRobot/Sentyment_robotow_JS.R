# copyright 2018 by Janina Smoła
# UTF8

# zrodla
## https://www.tidytextmining.com/dtm.html


# WSTĘP -------------------------------------------------------------------
# _00 wczytanie biblotek ----
library(dplyr)
library(RColorBrewer)

library(tm)
library(qdap)

library(wordcloud)
library(SentimentAnalysis)
library(SnowballC)

par_def <- par()
tab_col  <-  c('darkred', 'red', 'orangered','gold', 'lawngreen', 'green4', 'darkgreen','dodgerblue4', 'midnightblue')

# _01 xrodlo ----
path_src <- '/home/js/DS_studia/06_TM/ZALICZENIE/' # path to dir with filse of 'I, Robot' (each chapter in own file)
path_chap_01 <- file.path(path_src, 'I_Robot/01_Robbie')
path_chap_02 <- file.path(path_src, 'I_Robot/02_Runaround')
path_chap_03 <- file.path(path_src, 'I_Robot/03_Reason')
path_chap_04 <- file.path(path_src, 'I_Robot/04_Catch_That_Rabbit')
path_chap_05 <- file.path(path_src, 'I_Robot/05_Liar')
path_chap_06 <- file.path(path_src, 'I_Robot/06_Little_Lost_Robot')
path_chap_07 <- file.path(path_src, 'I_Robot/07_Escape')
path_chap_08 <- file.path(path_src, 'I_Robot/08_Evidence')
path_chap_09 <- file.path(path_src, 'I_Robot/09_The_Evitable_Conflict')


# _02 potzrbne dane ----
# lista ścieżek do plików
ls_path <- c(path_chap_01, path_chap_02, path_chap_03, 
             path_chap_04, path_chap_05, path_chap_06,
             path_chap_07, path_chap_07, path_chap_09) 

# lista tytułów = nazw plików poszczególnych rozdziałów
ls_chap <- c('01_Robbie', '02_Runaround', '03_Reason', 
             '04_Catch_That_Rabbit', '05_Liar', '06_Little_Lost_Robot',
             '07_Escape', '08_Evidence', '09_The_Evitable_Conflict')

#imiona robotów
ls_name_robot <- c('RB', 'Robbi', # chap_01
                   'Speedy', 'SPD-13', # chap_02
                   'Cutie', 'QT-1', # chap_03
                   'Dave', 'DV-5', # chap_04
                   'Herbie','RB-34', # chap_05
                   'Nestor', 'NS-5', #chap_06
                   'The Brain', 'Thinker', # chap_07
                   'Byerley' # chap_08 & chap_09
                   ) 
ls_name_ppl <- c('Byerley', 'Lawrence', 'Robertson',
                'Gloria', 'Weston', 'George', 'Grace',
                'Susan', 'Calvin', 
                'Mike', 'Michael', 'Donovan', 
                'Greg', 'Gregory', 'Powell',
                'Peter', 'Bogert', 
                'Alfred', ' Lanning', 'Milton', ' Ashe', 
                'Kallner', 
                'Gerald', ' Black', 
                'Alfred', ' Tanning',
                'Francis', 'Quinn', 
                'Lenton'
                )


# _03 korpus ----
getSources()

# funkcja tworząca korpus
create_corpus <- function(path){
  txt_doc <- readLines(path)
  vec_source <- VectorSource(txt_doc)
  Corpus(vec_source, readerControl = list(readPlain, language = 'en', load = TRUE))
}

cor_chap_01 <- create_corpus(path_chap_01)
length(cor_chap_01) ## 251

cor_chap_02 <- create_corpus(path_chap_02)
length(cor_chap_02) ## 245

cor_chap_03 <- create_corpus(path_chap_03)
length(cor_chap_03) ## 258

cor_chap_04 <- create_corpus(path_chap_04)
length(cor_chap_04) # 294

cor_chap_05 <- create_corpus(path_chap_05)
length(cor_chap_05) ## 279

cor_chap_06 <- create_corpus(path_chap_06)
length(cor_chap_06) ## 345

cor_chap_07 <- create_corpus(path_chap_07)
length(cor_chap_07) ## 353

cor_chap_08 <- create_corpus(path_chap_08)
length(cor_chap_08) ## 326

cor_chap_09 <- create_corpus(path_chap_09)
length(cor_chap_09) ## 261

# _04 czyszczenie corpusu ----
getTransformations()
# [1] "removeNumbers"     "removePunctuation" "removeWords"       "stemDocument"      "stripWhitespace"  

# funkcja oczyszczająca corpus
corpus_clean <- function(corpus) {
  corpus %>% tm_map(content_transformer(tolower)) %>% # małe litery
  tm_map(removeNumbers) %>%   # usun liczby
  tm_map(removeWords, stopwords(kind = 'en')) %>% # usuń stopword dla języka angielskiego
  #tm_map(removeWords,mystopwords)%>% # usuń własne stopword
  tm_map(removePunctuation) %>% # usuń znaki interpunkcyjne
  tm_map(stripWhitespace) # %>%  # usuń spacje
  # tm_map(stemDocument)  # zestemuj tekst: sprowadż wyrazy do rdzenia
}

clean_cor_chap_01 <- corpus_clean(cor_chap_01)
clean_cor_chap_02 <- corpus_clean(cor_chap_02)
clean_cor_chap_03 <- corpus_clean(cor_chap_03)
clean_cor_chap_04 <- corpus_clean(cor_chap_04)
clean_cor_chap_05 <- corpus_clean(cor_chap_05)
clean_cor_chap_06 <- corpus_clean(cor_chap_06)
clean_cor_chap_07 <- corpus_clean(cor_chap_07)
clean_cor_chap_08 <- corpus_clean(cor_chap_08)
clean_cor_chap_09 <- corpus_clean(cor_chap_09)

# _05 dok tekstowe ----
# funkcja tworząca dok trekstowe z zamianą Mrs. Mr. i U.S. na Mrs, Mr i U.S. (Zaburza podzial na zdania)
ls_old_all <- c('Mrs.', 'Mr.', 'U. S.', 'Dr.')
ls_new_all <- c('Mrs', 'Mr', 'US', 'Dr')

# funkcja tworząca dokument=treść opowiadania 
create_txt <- function(path, ls_old, ls_new){
  txt_doc <- readLines(path)
  for(i in 1:length(ls_old)){
    txt_doc <- gsub(ls_old[i], ls_new[i], txt_doc)
  }
  txt_doc
}

# tworzenia data frame: tot | txt | chap (bez NA)
doc_chap_01 <- create_txt(path_chap_01, ls_old_all, ls_new_all) ## scan(file.path(getwd(), 'I_Robot', ls_chap[1]), what ='character')
sen_chap_01 <- sentSplit(data.frame(txt = doc_chap_01), 'txt')
sen_chap_01$chap <- 'ch_01'
sen_chap_01 <- sen_chap_01[!is.na(sen_chap_01$txt), ]
dim(sen_chap_01) ## 590  3

doc_chap_02 <- create_txt(path_chap_02, ls_old_all, ls_new_all) ## scan(file.path(getwd(), 'I_Robot', ls_chap[2]), what ='character')
sen_chap_02 <- sentSplit(data.frame(txt = doc_chap_02), 'txt')
sen_chap_02$chap <- 'ch_02'
sen_chap_02 <- sen_chap_02[!is.na(sen_chap_02$txt), ]
dim(sen_chap_02) ## 620  3

doc_chap_03 <- create_txt(path_chap_03, ls_old_all, ls_new_all) ## scan(file.path(getwd(), 'I_Robot', ls_chap[3]), what ='character')
sen_chap_03 <- sentSplit(data.frame(txt = doc_chap_03), 'txt')
sen_chap_03$chap <- 'ch_03'
sen_chap_03 <- sen_chap_03[!is.na(sen_chap_03$txt), ]
dim(sen_chap_03) ## 617 3

doc_chap_04 <- create_txt(path_chap_04, ls_old_all, ls_new_all) ## scan(file.path(getwd(), 'I_Robot', ls_chap[4]), what ='character')
sen_chap_04 <- sentSplit(data.frame(txt = doc_chap_04), 'txt')
sen_chap_04$chap <- 'ch_04'
sen_chap_04 <- sen_chap_04[!is.na(sen_chap_04$txt), ]
dim(sen_chap_04) ## 776 3

doc_chap_05 <- create_txt(path_chap_05, ls_old_all, ls_new_all) ## scan(file.path(getwd(), 'I_Robot', ls_chap[5]), what ='character')
sen_chap_05 <- sentSplit(data.frame(txt = doc_chap_05), 'txt')
sen_chap_05$chap <- 'ch_05'
sen_chap_05 <- sen_chap_05[!is.na(sen_chap_05$txt), ]
dim(sen_chap_05) ## 635 3

doc_chap_06 <- create_txt(path_chap_06, ls_old_all, ls_new_all) ## scan(file.path(getwd(), 'I_Robot', ls_chap[6]), what ='character')
sen_chap_06 <- sentSplit(data.frame(txt = doc_chap_06), 'txt')
sen_chap_06$chap <- 'ch_06'
sen_chap_06 <- sen_chap_06[!is.na(sen_chap_06$txt), ]
dim(sen_chap_06) ## 874 3

doc_chap_07 <- create_txt(path_chap_07, ls_old_all, ls_new_all) ## scan(file.path(getwd(), 'I_Robot', ls_chap[7]), what ='character')
sen_chap_07 <- sentSplit(data.frame(txt = doc_chap_07), 'txt')
sen_chap_07$chap <- 'ch_07'
sen_chap_07 <- sen_chap_07[!is.na(sen_chap_07$txt), ]
dim(sen_chap_07) ## 806 3

doc_chap_08 <- create_txt(path_chap_08, ls_old_all, ls_new_all) ## scan(file.path(getwd(), 'I_Robot', ls_chap[8]), what ='character')
sen_chap_08 <- sentSplit(data.frame(txt = doc_chap_08), 'txt')
sen_chap_08$chap <- 'ch_08'
sen_chap_08 <- sen_chap_08[!is.na(sen_chap_08$txt), ]
dim(sen_chap_08)  ## 762 3

doc_chap_09 <- create_txt(path_chap_09, ls_old_all, ls_new_all) ## scan(file.path(getwd(), 'I_Robot', ls_chap[9]), what ='character')
sen_chap_09 <- sentSplit(data.frame(txt = doc_chap_09), 'txt')
sen_chap_09$chap <- 'ch_09'
sen_chap_091 <- sen_chap_09[!is.na(sen_chap_09$txt), ]
dim(sen_chap_09) ## 680 3

sen_chap_all <- rbind(sen_chap_01, sen_chap_02, sen_chap_03,
                      sen_chap_04, sen_chap_05, sen_chap_06,
                      sen_chap_07, sen_chap_08, sen_chap_09)


# _06 Document Term Matrix ----
dtm_chap_01 <- DocumentTermMatrix(clean_cor_chap_01)
str(dtm_chap_01)

dtm_chap_02 <- DocumentTermMatrix(clean_cor_chap_02)
str(dtm_chap_02)

dtm_chap_03 <- DocumentTermMatrix(clean_cor_chap_03)
str(dtm_chap_03)

dtm_chap_04 <- DocumentTermMatrix(clean_cor_chap_04)
str(dtm_chap_04)

dtm_chap_05 <- DocumentTermMatrix(clean_cor_chap_05)
str(dtm_chap_05)

dtm_chap_06 <- DocumentTermMatrix(clean_cor_chap_06)
str(dtm_chap_06)

dtm_chap_07 <- DocumentTermMatrix(clean_cor_chap_07)
str(dtm_chap_07)

dtm_chap_08 <- DocumentTermMatrix(clean_cor_chap_08)
str(dtm_chap_08)

dtm_chap_09 <- DocumentTermMatrix(clean_cor_chap_09)
str(dtm_chap_09)

# _07 Term Document Matrix ----
tdm_chap_01 <- TermDocumentMatrix(clean_cor_chap_01)
str(tdm_chap_01)
dim(tdm_chap_01) ## 1793 251  słowa zdania
tdm_chap_01$dimnames['Terms']
tdm_chap_01$dimnames['Docs']

tdm_chap_02 <- TermDocumentMatrix(clean_cor_chap_02)
str(tdm_chap_02)
dim(tdm_chap_02)  ## 1487  245
tdm_chap_02$dimnames['Terms']

tdm_chap_03 <- TermDocumentMatrix(clean_cor_chap_03)
str(tdm_chap_03)
dim(tdm_chap_03) ## 1513 258
tdm_chap_03$dimnames['Terms']

tdm_chap_04 <- TermDocumentMatrix(clean_cor_chap_04)
str(tdm_chap_04)
dim(tdm_chap_04) ## 1530 294
tdm_chap_04$dimnames['Terms']

tdm_chap_05 <- TermDocumentMatrix(clean_cor_chap_05)
str(tdm_chap_05)
dim(tdm_chap_05) ## 1333 279
tdm_chap_05$dimnames['Terms']

tdm_chap_06 <- TermDocumentMatrix(clean_cor_chap_06)
str(tdm_chap_06)
dim(tdm_chap_06) ## 1820 345
tdm_chap_06$dimnames['Terms']

tdm_chap_07 <- TermDocumentMatrix(clean_cor_chap_07)
str(tdm_chap_07)
dim(tdm_chap_07) ## 1647 353
tdm_chap_07$dimnames['Terms']

tdm_chap_08 <- TermDocumentMatrix(clean_cor_chap_08)
str(tdm_chap_08)
dim(tdm_chap_08) ## 1835 326
tdm_chap_05$dimnames['Terms']

tdm_chap_09 <- TermDocumentMatrix(clean_cor_chap_09)
str(tdm_chap_09)
dim(tdm_chap_09) ## 1858 261
tdm_chap_09$dimnames['Terms']

# ANALIZA -----------------------------------------------------------------
# _01 statystyki słów ----
# ___chap 01 ----
sort(rowSums(as.matrix(tdm_chap_01)), decreasing = TRUE)
barplot(sort(rowSums(as.matrix(tdm_chap_01)), decreasing = TRUE)[1:20],
        las = 2, col = colorRampPalette(brewer.pal(7, 'Reds'))(20), #grDevices::topo.colors(20))
        horiz = TRUE)

table(sort(findFreqTerms(dtm_chap_01), decreasing = TRUE))
findFreqTerms(dtm_chap_01, lowfreq = 40)
## [1] "robbi"  "gloria" "robot"  "weston"

freq_ch_01 <- colSums(as.matrix(dtm_chap_01))
wordcloud(names(freq_ch_01), freq_ch_01, #min.freq = 20, 
          scale = c(3, .5), colors = brewer.pal(6, 'RdBu'))
dev.copy(jpeg, 'wordcloud_chap_01.jpg')
dev.off()


# ___chap 02 ----
sort(rowSums(as.matrix(tdm_chap_02)), decreasing = TRUE)
barplot(sort(rowSums(as.matrix(tdm_chap_02)), decreasing = TRUE)[1:20],
        las = 2, col = colorRampPalette(brewer.pal(7, 'Reds'))(20), #grDevices::topo.colors(20))
        horiz = TRUE)

table(sort(findFreqTerms(dtm_chap_02), decreasing = TRUE))
findFreqTerms(dtm_chap_02, lowfreq = 40)
## [1] "donovan" "powell"  "speedy"  "robot"  

freq_ch_02 <- colSums(as.matrix(dtm_chap_02))
wordcloud(names(freq_ch_02), freq_ch_02, #min.freq = 20, 
          scale = c(3, .5), colors = brewer.pal(6, 'RdBu'))
dev.copy(jpeg, 'wordcloud_chap_02.jpg')
dev.off()

# ___chap 03 ----
sort(rowSums(as.matrix(tdm_chap_03)), decreasing = TRUE)
barplot(sort(rowSums(as.matrix(tdm_chap_03)), decreasing = TRUE)[1:20],
        las = 2, col = colorRampPalette(brewer.pal(7, 'Reds'))(20), #grDevices::topo.colors(20))
        horiz = TRUE)

table(sort(findFreqTerms(dtm_chap_03), decreasing = TRUE))
findFreqTerms(dtm_chap_03, lowfreq = 40)
## [1] "donovan" "powell"  "robot"   "cutie"  

freq_ch_03 <- colSums(as.matrix(dtm_chap_03))
wordcloud(names(freq_ch_03), freq_ch_03, #min.freq = 20, 
          scale = c(3, .5), colors = brewer.pal(6, 'RdBu'))
dev.copy(jpeg, 'wordcloud_chap_03.jpg')
dev.off()

# ___chap 04 ----
sort(rowSums(as.matrix(tdm_chap_04)), decreasing = TRUE)
barplot(sort(rowSums(as.matrix(tdm_chap_04)), decreasing = TRUE)[1:20],
        las = 2, col = colorRampPalette(brewer.pal(7, 'Reds'))(20), #grDevices::topo.colors(20))
        horiz = TRUE)

table(sort(findFreqTerms(dtm_chap_04), decreasing = TRUE))
findFreqTerms(dtm_chap_04, lowfreq = 40)
## [1] "donovan" "powell"  "robots"  "said"    "know"

freq_ch_04 <- colSums(as.matrix(dtm_chap_04))
wordcloud(names(freq_ch_04), freq_ch_04, #min.freq = 20, 
          scale = c(3, .5), colors = brewer.pal(6, 'RdBu'))
dev.copy(jpeg, 'wordcloud_chap_04.jpg')
dev.off()

# ___chap 05 ----
sort(rowSums(as.matrix(tdm_chap_05)), decreasing = TRUE)
barplot(sort(rowSums(as.matrix(tdm_chap_05)), decreasing = TRUE)[1:20],
        las = 2, col = colorRampPalette(brewer.pal(7, 'Reds'))(20), #grDevices::topo.colors(20))
        horiz = TRUE)

table(sort(findFreqTerms(dtm_chap_05), decreasing = TRUE))
findFreqTerms(dtm_chap_05, lowfreq = 40)
## [1] "lanning" "herbie" 

freq_ch_05 <- colSums(as.matrix(dtm_chap_05))
wordcloud(names(freq_ch_05), freq_ch_05, #min.freq = 20, 
          scale = c(3, .5), colors = brewer.pal(6, 'RdBu'))
dev.copy(jpeg, 'wordcloud_chap_05.jpg')
dev.off()

# ___chap 06 ----
sort(rowSums(as.matrix(tdm_chap_06)), decreasing = TRUE)
barplot(sort(rowSums(as.matrix(tdm_chap_06)), decreasing = TRUE)[1:20],
        las = 2, col = colorRampPalette(brewer.pal(7, 'Reds'))(20), #grDevices::topo.colors(20))
        horiz = TRUE)

table(sort(findFreqTerms(dtm_chap_06), decreasing = TRUE))
findFreqTerms(dtm_chap_06, lowfreq = 40)
## [1]  "calvin" "said"   "one"    "robot"  "bogert" "robots"

freq_ch_06 <- colSums(as.matrix(dtm_chap_06))
wordcloud(names(freq_ch_06), freq_ch_06, #min.freq = 20, 
          scale = c(3, .5), colors = brewer.pal(6, 'RdBu'))
dev.copy(jpeg, 'wordcloud_chap_06.jpg')
dev.off()

# ___chap 07 ----
sort(rowSums(as.matrix(tdm_chap_07)), decreasing = TRUE)
barplot(sort(rowSums(as.matrix(tdm_chap_07)), decreasing = TRUE)[1:20],
        las = 2, col = colorRampPalette(brewer.pal(7, 'Reds'))(20), #grDevices::topo.colors(20))
        horiz = TRUE)

table(sort(findFreqTerms(dtm_chap_07), decreasing = TRUE))
findFreqTerms(dtm_chap_07, lowfreq = 40)
## [1] "said"    "brain"   "donovan" "powell" 

freq_ch_07 <- colSums(as.matrix(dtm_chap_07))
wordcloud(names(freq_ch_07), freq_ch_07, #min.freq = 20, 
          scale = c(3, .5), colors = brewer.pal(6, 'RdBu'))
dev.copy(jpeg, 'wordcloud_chap_07.jpg')
dev.off()

# ___chap 08 ----
sort(rowSums(as.matrix(tdm_chap_08)), decreasing = TRUE)
barplot(sort(rowSums(as.matrix(tdm_chap_08)), decreasing = TRUE)[1:20],
        las = 2, col = colorRampPalette(brewer.pal(7, 'Reds'))(20), #grDevices::topo.colors(20))
        horiz = TRUE)

table(sort(findFreqTerms(dtm_chap_08), decreasing = TRUE))
findFreqTerms(dtm_chap_08, lowfreq = 40)
## [1] "byerley" "quinn"   "lanning" "robot"   

freq_ch_08 <- colSums(as.matrix(dtm_chap_08))
wordcloud(names(freq_ch_08), freq_ch_08, #min.freq = 20, 
          scale = c(3, .5), colors = brewer.pal(6, 'RdBu'))
dev.copy(jpeg, 'wordcloud_chap_08.jpg')
dev.off()

# ___chap 09 ----
sort(rowSums(as.matrix(tdm_chap_09)), decreasing = TRUE)
barplot(sort(rowSums(as.matrix(tdm_chap_09)), decreasing = TRUE)[1:20],
        las = 2, col = colorRampPalette(brewer.pal(7, 'Reds'))(20), #grDevices::topo.colors(20))
        horiz = TRUE)

table(sort(findFreqTerms(dtm_chap_09), decreasing = TRUE))
findFreqTerms(dtm_chap_09, lowfreq = 40)
## [1]  "one"      "machines" "machine" 

freq_ch_09 <- colSums(as.matrix(dtm_chap_09))
wordcloud(names(freq_ch_09), freq_ch_09, #min.freq = 20, 
          scale = c(3, .5), colors = brewer.pal(6, 'RdBu'))
dev.copy(jpeg, 'wordcloud_chap_09.jpg')
dev.off()

# ___rys do prezentacji STAT SŁÓW ----
par(mfcol = c(3, 3))
# chap_01
barplot(sort(rowSums(as.matrix(tdm_chap_01)), decreasing = TRUE)[1:5],
        las = 1, col = colorRampPalette(brewer.pal(7, 'Reds'))(10), #grDevices::topo.colors(20))
        horiz = TRUE, main = 'Chap 01', yaxt = 'n')
text(15, seq(0.7, 5.5, 1.2), labels = names(sort(rowSums(as.matrix(tdm_chap_01)), decreasing = TRUE))[1:5], 
     font = 4, cex = 1.5)
# chap_02
barplot(sort(rowSums(as.matrix(tdm_chap_02)), decreasing = TRUE)[1:5],
        las = 1, col = colorRampPalette(brewer.pal(7, 'Reds'))(10), #grDevices::topo.colors(20))
        horiz = TRUE, main = 'Chap 02', yaxt = 'n')
text(15, seq(0.7, 5.5, 1.2), labels = names(sort(rowSums(as.matrix(tdm_chap_02)), decreasing = TRUE))[1:5], 
     font = 4, cex = 1.5)
# chap_03
barplot(sort(rowSums(as.matrix(tdm_chap_03)), decreasing = TRUE)[1:5],
        las = 1, col = colorRampPalette(brewer.pal(7, 'Reds'))(10), #grDevices::topo.colors(20))
        horiz = TRUE, main = 'Chap 03', yaxt = 'n')
text(15, seq(0.7, 5.5, 1.2), labels = names(sort(rowSums(as.matrix(tdm_chap_03)), decreasing = TRUE))[1:5], 
     font = 4, cex = 1.5)
# chap_04
barplot(sort(rowSums(as.matrix(tdm_chap_04)), decreasing = TRUE)[1:5],
        las = 1, col = colorRampPalette(brewer.pal(7, 'Reds'))(10), #grDevices::topo.colors(20))
        horiz = TRUE, main = 'Chap 04', yaxt = 'n')
text(15, seq(0.7, 5.5, 1.2), labels = names(sort(rowSums(as.matrix(tdm_chap_04)), decreasing = TRUE))[1:5], 
     font = 4, cex = 1.5)
# chap_05
barplot(sort(rowSums(as.matrix(tdm_chap_05)), decreasing = TRUE)[1:5],
        las = 1, col = colorRampPalette(brewer.pal(7, 'Reds'))(10), #grDevices::topo.colors(20))
        horiz = TRUE, main = 'Chap 05', yaxt = 'n')
text(15, seq(0.7, 5.5, 1.2), labels = names(sort(rowSums(as.matrix(tdm_chap_05)), decreasing = TRUE))[1:5], 
     font = 4, cex = 1.5)
# chap_06
barplot(sort(rowSums(as.matrix(tdm_chap_06)), decreasing = TRUE)[1:5],
        las = 1, col = colorRampPalette(brewer.pal(7, 'Reds'))(10), #grDevices::topo.colors(20))
        horiz = TRUE, main = 'Chap 06', yaxt = 'n')
text(15, seq(0.7, 5.5, 1.2), labels = names(sort(rowSums(as.matrix(tdm_chap_06)), decreasing = TRUE))[1:5], 
     font = 4, cex = 1.5)
# chap_07
barplot(sort(rowSums(as.matrix(tdm_chap_07)), decreasing = TRUE)[1:5],
        las = 1, col = colorRampPalette(brewer.pal(7, 'Reds'))(10), #grDevices::topo.colors(20))
        horiz = TRUE, main = 'Chap 07', yaxt = 'n')
text(15, seq(0.7, 5.5, 1.2), labels = names(sort(rowSums(as.matrix(tdm_chap_07)), decreasing = TRUE))[1:5], 
     font = 4, cex = 1.5)
# chap_08
barplot(sort(rowSums(as.matrix(tdm_chap_08)), decreasing = TRUE)[1:5],
        las = 1, col = colorRampPalette(brewer.pal(7, 'Reds'))(10), #grDevices::topo.colors(20))
        horiz = TRUE, main = 'Chap 08', yaxt = 'n')
text(15, seq(0.7, 5.5, 1.2), labels = names(sort(rowSums(as.matrix(tdm_chap_08)), decreasing = TRUE))[1:5], 
     font = 4, cex = 1.5)
# chap_09
barplot(sort(rowSums(as.matrix(tdm_chap_09)), decreasing = TRUE)[1:5],
        las = 1, col = colorRampPalette(brewer.pal(7, 'Reds'))(10), #grDevices::topo.colors(20))
        horiz = TRUE, main = 'Chap 09', yaxt = 'n')
text(15, seq(0.7, 5.5, 1.2), labels = names(sort(rowSums(as.matrix(tdm_chap_09)), decreasing = TRUE))[1:5], 
     font = 4, cex = 1.5)
par(mfcol = c(par_def$mfcol))



# _02 sentyment ----
DictionaryGI[[1]][2:5]
## [1] "abandonment" "abate"       "abdicate"    "abhor" 
DictionaryGI[[2]][50:55]
## [1] "adherent"   "adhesion"   "adhesive"   "adjunct"    "adjust"     "adjustable"

str(DictionaryGI)
    # List of 2
    # $ negative: chr [1:2005] "abandon" "abandonment" "abate" "abdicate" ...
    # $ positive: chr [1:1637] "abide" "ability" "able" "abound" ...
summary(DictionaryGI)
    #          Length Class  Mode     
    # negative 2005   -none- character
    # positive 1637   -none- character
# Dictionary with a list of positive and negative words according to the psychological Harvard-IV
# dictionary as used in the General Inquirer software. This is a general-purpose dictionary developed
# by the Harvard University
data(DictionaryGI)

# cale opowiadania ----
# ___chap 01 ----
class(sen_chap_01$txt) ## "character"
typeof(sen_chap_01$txt) ## "character"

senti_chap_01 <- analyzeSentiment(sen_chap_01$txt) 
# language = "english", rules = defaultSentimentRules(), removeStopwords = TRUE, stemming = TRUE
colnames(senti_chap_01)
    # [1] "WordCount"          "SentimentGI"        "NegativityGI"       "PositivityGI"       "SentimentHE"        "NegativityHE"       "PositivityHE"      
    # [8] "SentimentLM"        "NegativityLM"       "PositivityLM"       "RatioUncertaintyLM" "SentimentQDAP"      "NegativityQDAP"     "PositivityQDAP"  
val_senti_chap_01 <- colSums(senti_chap_01, na.rm = TRUE, dims = 1)
length(sen_chap_01$txt) ## 590

# ___chap 02 ----
senti_chap_02 <- analyzeSentiment(sen_chap_02$txt)
val_senti_chap_02 <- colSums(senti_chap_02, na.rm = TRUE, dims = 1)
length(sen_chap_02$txt) ## 620

# ___chap 03 ----
senti_chap_03 <- analyzeSentiment(sen_chap_03$txt)
val_senti_chap_03 <- colSums(senti_chap_03, na.rm = TRUE, dims = 1)
length(sen_chap_03$txt) ## 617

# ___chap 04 ----
senti_chap_04 <- analyzeSentiment(sen_chap_04$txt)
val_senti_chap_04 <- colSums(senti_chap_04, na.rm = TRUE, dims = 1)
length(sen_chap_04$txt) ## 776

# ___chap 05 ----
senti_chap_05 <- analyzeSentiment(sen_chap_05$txt)
val_senti_chap_05 <- colSums(senti_chap_05, na.rm = TRUE, dims = 1)
length(sen_chap_05$txt) ## 635

# ___chap 06 ----
senti_chap_06 <- analyzeSentiment(sen_chap_06$txt)
val_senti_chap_06 <- colSums(senti_chap_06, na.rm = TRUE, dims = 1)
length(sen_chap_06$txt) ## 874

# ___chap 07 ----
senti_chap_07 <- analyzeSentiment(sen_chap_07$txt)
val_senti_chap_07 <- colSums(senti_chap_07, na.rm = TRUE, dims = 1)
length(sen_chap_07$txt) ## 806

# ___chap 08 ----
senti_chap_08 <- analyzeSentiment(sen_chap_08$txt)
val_senti_chap_08 <- colSums(senti_chap_08, na.rm = TRUE, dims = 1)
length(sen_chap_08$txt) ## 762

# ___chap 09 ----
senti_chap_09 <- analyzeSentiment(sen_chap_09$txt)
val_senti_chap_09 <- colSums(senti_chap_09, na.rm = TRUE, dims = 1)
length(sen_chap_09$txt) ## 680

# wykres zbiorczy
plot(val_senti_chap_01[2:14], type = 'b', col = tab_col[1], lwd = 2, 
     ylim = c(-50, 150), xaxt = 'n',
     main = 'Wykres wartości sentymentu wg różnych miar', ylab = 'wartość', xlab = 'miary')
points(1:13, val_senti_chap_02[2:14], type = 'b', col = tab_col[2], lwd = 2)
points(1:13, val_senti_chap_03[2:14], type = 'b', col = tab_col[3], lwd = 2)
points(1:13, val_senti_chap_04[2:14], type = 'b', col = tab_col[4], lwd = 2)
points(1:13, val_senti_chap_05[2:14], type = 'b', col = tab_col[5], lwd = 2)
points(1:13, val_senti_chap_06[2:14], type = 'b', col = tab_col[6], lwd = 2)
points(1:13, val_senti_chap_07[2:14], type = 'b', col = tab_col[7], lwd = 2)
points(1:13, val_senti_chap_08[2:14], type = 'b', col = tab_col[8], lwd = 2)
points(1:13, val_senti_chap_09[2:14], type = 'b', col = tab_col[9], lwd = 2)
abline(v = c(3, 6, 10), lty = 2)
abline(h = 0, lty = 1)
    # legend('topleft', legend = c('chap_01', 'chap_02'),
    #        col = tab_col[1:9], lty = 1, 
    #        #pch = c(1, 2), lwd = c(2, 2), box.lty = 0, cex = 1, 
    #        horiz = TRUE
    #        )
axis(1, at = seq(1, 13, 1), labels = c('SenGI', 'NegGI', 'PosGI',
                                       'SenHE', 'NegHE', 'PosHE',
                                       'SenLM', 'NegLM', 'PosLM', 'RatUncerLM',
                                       'SenQDAP', 'NegQDAP', 'PosQDAP'),
     cex.axis = 0.8,  las = 2)

grid()


# filtr robotów ----

ls_name_robot
# ___chap 01 ----
# lista słów filtrujących - opisująca roboty
ls_to_match_chap_01 <- c('RB', 'Robbi', 'robot','machine', 'computer', 'humanoid', 'robotics')
robot_chap_01 <-  filter(sen_chap_01, grepl(paste(ls_to_match_chap_01, collapse = '|'),   
                                               sen_chap_01$txt))$txt
senti_robot_chap_01 <- analyzeSentiment(robot_chap_01) ## removeStopwords = TRUE, stemming = TRUE,
dim(senti_robot_chap_01) ## 135 14

(val_senti_robot_chap_01 <- colSums(senti_robot_chap_01, na.rm = TRUE, dims = 1))

#wykres
plot(val_senti_robot_chap_01[2:4]/length(robot_chap_01)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Wykres porównanie wartości sentymentu \n dla całego opowiadania i dla zdań o robotach',
     ylab = 'wartość', xlab = 'miara')
points(1:3, val_senti_chap_01[2:4]/nrow(sen_chap_01)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))


# sentyment zdań z robotami w opowidaniu
plot(rowSums(senti_chap_01[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')
plot(rowSums(senti_robot_chap_01[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')

plotSentiment(senti_robot_chap_01$SentimentGI)
plotSentiment(senti_chap_01$SentimentGI)

plotSentiment(senti_robot_chap_01$NegativityGI)
plotSentiment(senti_chap_01$NegativityGI)

plotSentiment(senti_robot_chap_01$PositivityGI)
plotSentiment(senti_chap_01$PositivityGI)

# ___chap 02 ----
# lista słóœ filtrujących - opisująca roboty
ls_to_match_chap_02 <- c('SPD-13', 'Speedy', 'robot','machine', 'computer', 'humanoid', 'robotics')
robot_chap_02 <-  filter(sen_chap_02, grepl(paste(ls_to_match_chap_02, collapse = '|'),   
                                            sen_chap_02$txt))$txt
senti_robot_chap_02 <- analyzeSentiment(robot_chap_02)
dim(senti_robot_chap_02) ## 113 14

(val_senti_robot_chap_02 <- colSums(senti_robot_chap_02, na.rm = TRUE, dims = 1))

#wykres
plot(val_senti_robot_chap_02[2:4]/length(robot_chap_02)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Wykres porównanie wartości sentymentu \n dla całego opowiadania i dla zdań o robotach',
     ylab = 'wartość', xlab = 'miara')
points(1:3, val_senti_chap_02[2:4]/nrow(sen_chap_02)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))

# sentyment zdań z robotami w opowidaniu
plot(rowSums(senti_chap_02[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')
plot(rowSums(senti_robot_chap_02[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')

plotSentiment(senti_robot_chap_02$SentimentGI)
plotSentiment(senti_chap_02$SentimentGI)

# ___chap 03 ----
# lista słóœ filtrujących - opisująca roboty
ls_to_match_chap_03 <- c('QT-1', 'Cutie', 'robot','machine', 'computer', 'humanoid', 'robotics')
robot_chap_03 <-  filter(sen_chap_03, grepl(paste(ls_to_match_chap_03, collapse = '|'),   
                                            sen_chap_03$txt))$txt
senti_robot_chap_03 <- analyzeSentiment(robot_chap_03)
dim(senti_robot_chap_03) ## 119 14

(val_senti_robot_chap_03 <- colSums(senti_robot_chap_03, na.rm = TRUE, dims = 1))

#wykres
plot(val_senti_robot_chap_03[2:4]/length(robot_chap_03)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Wykres porównanie wartości sentymentu \n dla całego opowiadania i dla zdań o robotach',
     ylab = 'wartość', xlab = 'miara')
points(1:3, val_senti_chap_03[2:4]/nrow(sen_chap_03)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))

# sentyment zdań z robotami w opowidaniu
plot(rowSums(senti_chap_03[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')
plot(rowSums(senti_robot_chap_03[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')

plotSentiment(senti_robot_chap_03$SentimentGI)
plotSentiment(senti_chap_03$SentimentGI)

# ___chap 04 ----
# lista słóœ filtrujących - opisująca roboty
ls_to_match_chap_04 <- c('DV-5', 'Dave', 'robot','machine', 'computer', 'humanoid', 'robotics')
robot_chap_04 <-  filter(sen_chap_04, grepl(paste(ls_to_match_chap_04, collapse = '|'),   
                                            sen_chap_04$txt))$txt
senti_robot_chap_04 <- analyzeSentiment(robot_chap_04)
dim(senti_robot_chap_04) ## 100 14

(val_senti_robot_chap_04 <- colSums(senti_robot_chap_04, na.rm = TRUE, dims = 1))

#wykres
plot(val_senti_robot_chap_04[2:4]/length(robot_chap_04)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Wykres porównanie wartości sentymentu \n dla całego opowiadania i dla zdań o robotach',
     ylab = 'wartość', xlab = 'miara')
points(1:3, val_senti_chap_04[2:4]/nrow(sen_chap_04)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))

# sentyment zdań z robotami w opowidaniu
plot(rowSums(senti_chap_04[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')
plot(rowSums(senti_robot_chap_04[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')

plotSentiment(senti_robot_chap_04$SentimentGI)
plotSentiment(senti_chap_04$SentimentGI)

# ___chap 05 ----
# lista słóœ filtrujących - opisująca roboty
ls_to_match_chap_05 <- c('RB-34', 'Herbie', 'robot','machine', 'computer', 'humanoid', 'robotics')
robot_chap_05 <-  filter(sen_chap_05, grepl(paste(ls_to_match_chap_05, collapse = '|'),   
                                            sen_chap_05$txt))$txt
senti_robot_chap_05 <- analyzeSentiment(robot_chap_05)
dim(senti_robot_chap_05) ## 97 14

(val_senti_robot_chap_05 <- colSums(senti_robot_chap_05, na.rm = TRUE, dims = 1))

#wykres
plot(val_senti_robot_chap_05[2:4]/length(robot_chap_05)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Wykres porównanie wartości sentymentu \n dla całego opowiadania i dla zdań o robotach',
     ylab = 'wartość', xlab = 'miara')
points(1:3, val_senti_chap_05[2:4]/nrow(sen_chap_05)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))

# sentyment zdań z robotami w opowidaniu
plot(rowSums(senti_chap_05[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')
plot(rowSums(senti_robot_chap_05[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')

plotSentiment(senti_robot_chap_05$SentimentGI)
plotSentiment(senti_chap_05$SentimentGI)

# ___chap 06 ----
# lista słóœ filtrujących - opisująca roboty
ls_to_match_chap_06 <- c('NS-5R', 'Nestor', 'robot','machine', 'computer', 'humanoid', 'robotics')
robot_chap_06 <-  filter(sen_chap_06, grepl(paste(ls_to_match_chap_06, collapse = '|'),   
                                            sen_chap_06$txt))$txt
senti_robot_chap_06 <- analyzeSentiment(robot_chap_06)
dim(senti_robot_chap_06) ## 155 14

(val_senti_robot_chap_06 <- colSums(senti_robot_chap_06, na.rm = TRUE, dims = 1))

#wykres
plot(val_senti_robot_chap_06[2:4]/length(robot_chap_06)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Wykres porównanie wartości sentymentu \n dla całego opowiadania i dla zdań o robotach',
     ylab = 'wartość', xlab = 'miara')
points(1:3, val_senti_chap_06[2:4]/nrow(sen_chap_06)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))

# sentyment zdań z robotami w opowidaniu
plot(rowSums(senti_chap_06[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')
plot(rowSums(senti_robot_chap_06[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')

plotSentiment(senti_robot_chap_06$SentimentGI)
plotSentiment(senti_chap_06$SentimentGI)

# ___chap 07 ----
# lista słóœ filtrujących - opisująca roboty
ls_to_match_chap_07 <- c('Thinker', 'The Brain', 'robot','machine', 'computer', 'humanoid', 'robotics')
robot_chap_07 <-  filter(sen_chap_07, grepl(paste(ls_to_match_chap_07, collapse = '|'),   
                                            sen_chap_07$txt))$txt
senti_robot_chap_07 <- analyzeSentiment(robot_chap_07)
dim(senti_robot_chap_07) ## 77 14

(val_senti_robot_chap_07 <- colSums(senti_robot_chap_07, na.rm = TRUE, dims = 1))

#wykres
plot(val_senti_robot_chap_07[2:4]/length(robot_chap_07)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Wykres porównanie wartości sentymentu \n dla całego opowiadania i dla zdań o robotach',
     ylab = 'wartość', xlab = 'miara')
points(1:3, val_senti_chap_07[2:4]/nrow(sen_chap_07)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))

# sentyment zdań z robotami w opowidaniu
plot(rowSums(senti_chap_07[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')
plot(rowSums(senti_robot_chap_07[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')

plotSentiment(senti_robot_chap_07$SentimentGI)
plotSentiment(senti_chap_07$SentimentGI)

# ___chap 08 ----
# lista słóœ filtrujących - opisująca roboty
ls_to_match_chap_08 <- c('Stephen', 'Byerley', 'robot','machine', 'computer', 'humanoid', 'robotics')
robot_chap_08 <-  filter(sen_chap_08, grepl(paste(ls_to_match_chap_08, collapse = '|'),   
                                            sen_chap_08$txt))$txt
senti_robot_chap_08 <- analyzeSentiment(robot_chap_08)
dim(senti_robot_chap_08) ## 137 14

(val_senti_robot_chap_08 <- colSums(senti_robot_chap_08, na.rm = TRUE, dims = 1))

#wykres
plot(val_senti_robot_chap_08[2:4]/length(robot_chap_08)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Wykres porównanie wartości sentymentu \n dla całego opowiadania i dla zdań o robotach',
     ylab = 'wartość', xlab = 'miara')
points(1:3, val_senti_chap_08[2:4]/nrow(sen_chap_08)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))

# sentyment zdań z robotami w opowidaniu
plot(rowSums(senti_chap_08[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')
plot(rowSums(senti_robot_chap_08[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')

plotSentiment(senti_robot_chap_08$SentimentGI)
plotSentiment(senti_chap_08$SentimentGI)

# ___chap 09 ----
# lista słóœ filtrujących - opisująca roboty
ls_to_match_chap_09 <- c('Stephen', 'Byerley', 'robot', 'machine', 'computer', 'humanoid', 'robotics')
robot_chap_09 <-  filter(sen_chap_09, grepl(paste(ls_to_match_chap_09, collapse = '|'),   
                                            sen_chap_09$txt))$txt
senti_robot_chap_09 <- analyzeSentiment(robot_chap_09)
dim(senti_robot_chap_09) ## 59 14

(val_senti_robot_chap_09 <- colSums(senti_robot_chap_09, na.rm = TRUE, dims = 1))

#wykres
plot(val_senti_robot_chap_09[2:4]/length(robot_chap_09)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Wykres porównanie wartości sentymentu \n dla całego opowiadania i dla zdań o robotach',
     ylab = 'wartość', xlab = 'miara')
points(1:3, val_senti_chap_09[2:4]/nrow(sen_chap_09)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))

# sentyment zdań z robotami w opowidaniu
plot(rowSums(senti_chap_09[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')
plot(rowSums(senti_robot_chap_09[3:4], na.rm = TRUE, dims = 1), col = ' blue', type = 'b', pch = 'x')

plotSentiment(senti_robot_chap_09$SentimentGI)
plotSentiment(senti_chap_09$SentimentGI)


# ___rys do prezentacji PORÓWNANIE ----
par(mfcol = c(1, 2))
plot(val_senti_robot_chap_01[2:4], 
     type = 'b', ylim = c(-5, 100), xaxt = 'n', las = 2, lwd = 2,
     ylab = '', xlab = '', main = 'Wartości bezwzględne')
points(1:3, val_senti_chap_01[2:4], type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))
text(x = 1:3, y = val_senti_robot_chap_01[2:4], 
     labels = round(val_senti_robot_chap_01[2:4], 1), pos = 1)
text(x = 1:3, y = val_senti_chap_01[2:4], 
     labels = round(val_senti_chap_01[2:4], 1), pos = 3, col = 'red')
grid()

plot(val_senti_robot_chap_01[2:4]/length(robot_chap_01)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2, lwd = 2,
     ylab = '', xlab = '', main = 'Wartości ustandaryzowane')
points(1:3, val_senti_chap_01[2:4]/nrow(sen_chap_01)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))
text(x = 1:3, y = val_senti_robot_chap_01[2:4]/length(robot_chap_01)*100, 
     labels = round(val_senti_robot_chap_01[2:4]/length(robot_chap_01)*100, 1), pos = 1)
text(x = 1:3, val_senti_chap_01[2:4]/nrow(sen_chap_01)*100, 
     labels = round(val_senti_chap_01[2:4]/nrow(sen_chap_01)*100, 1), pos = 3, col = 'red')
grid()
par(mfcol = c(par_def$mfcol))


# ___rys do prezentacji WYKRES SENTYMENTU ----
par(mfcol = c(3, 3))
# chap_01
plot(val_senti_robot_chap_01[2:4]/length(robot_chap_01)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Chap 01', ylab = '', xlab = '')
points(1:3, val_senti_chap_01[2:4]/nrow(sen_chap_01)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))
# chap_02
plot(val_senti_robot_chap_02[2:4]/length(robot_chap_02)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Chap 02', ylab = '', xlab = '')
points(1:3, val_senti_chap_02[2:4]/nrow(sen_chap_02)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))
# chap 03
plot(val_senti_robot_chap_03[2:4]/length(robot_chap_03)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Chap 03', ylab = '', xlab = '')
points(1:3, val_senti_chap_03[2:4]/nrow(sen_chap_03)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))
# chap_04
plot(val_senti_robot_chap_04[2:4]/length(robot_chap_04)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Chap 04', ylab = '', xlab = '')
points(1:3, val_senti_chap_04[2:4]/nrow(sen_chap_04)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))
# chap_05
plot(val_senti_robot_chap_05[2:4]/length(robot_chap_05)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Chap 05', ylab = '', xlab = '')
points(1:3, val_senti_chap_05[2:4]/nrow(sen_chap_05)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))
# chap 06
plot(val_senti_robot_chap_06[2:4]/length(robot_chap_06)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Chap 06', ylab = '', xlab = '')
points(1:3, val_senti_chap_06[2:4]/nrow(sen_chap_06)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))
# chap_07
plot(val_senti_robot_chap_07[2:4]/length(robot_chap_07)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Chap 07', ylab = '', xlab = '')
points(1:3, val_senti_chap_07[2:4]/nrow(sen_chap_07)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))
# chap_08
plot(val_senti_robot_chap_08[2:4]/length(robot_chap_08)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Chap 08', ylab = '', xlab = '')
points(1:3, val_senti_chap_08[2:4]/nrow(sen_chap_08)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))
# chap 09
plot(val_senti_robot_chap_09[2:4]/length(robot_chap_09)*100, 
     type = 'b', ylim = c(-5, 25), xaxt = 'n', las = 2,
     main = 'Chap 09', ylab = '', xlab = '')
points(1:3, val_senti_chap_09[2:4]/nrow(sen_chap_09)*100, type = 'b', col = 'red', lwd = 2)
abline(h = 0)
axis(1, at = seq(1, 3, 1), labels = c('SenGI', 'NegGI', 'PosGI'))

par(mfcol = c(par_def$mfcol))


# ___lista obiektów senti_chap ----
ls_senti_chap_all <- list(senti_chap_01, senti_chap_02, senti_chap_03,
                          senti_chap_04, senti_chap_05, senti_chap_06,
                          senti_chap_07, senti_chap_08, senti_chap_09)

names(ls_senti_chap_all) <- c('senti_chap_01', 'senti_chap_02', 'senti_chap_03',
                              'senti_chap_04', 'senti_chap_05', 'senti_chap_06',
                              'senti_chap_07', 'senti_chap_08', 'senti_chap_09')

ls_senti_chap_all[[1]][1:4]


# ___lista obiektów senti_robot ----
ls_senti_robot_chap_all <- list(senti_robot_chap_01, senti_robot_chap_02, senti_robot_chap_03,
                                senti_robot_chap_04, senti_robot_chap_05, senti_robot_chap_06,
                                senti_robot_chap_07, senti_robot_chap_08, senti_robot_chap_09)

names(ls_senti_robot_chap_all) <- c('senti_robot_chap_01', 'senti_robot_chap_02', 'senti_robot_chap_03',
                                    'senti_robot_chap_04', 'senti_robot_chap_05', 'senti_robot_chap_06',
                                    'senti_robot_chap_07', 'senti_robot_chap_08', 'senti_robot_chap_09')

ls_senti_robot_chap_all[[1]][1:4]




# ___rys do prezentacji SŁUPKI % ----
    # filter(senti_robot_chap_01, WordCount != 0) %>% select(WordCount) %>% count() ## 135
    # filter(senti_robot_chap_01, SentimentGI == 0) %>% select(WordCount) %>% colSums() ## 299
    # filter(senti_robot_chap_01, SentimentGI == 0) %>% select(WordCount) %>% count() ## 50
    # filter(senti_robot_chap_01, NegativityGI == 0, SentimentGI != 0) %>% select(WordCount) %>% colSums() ## 227
    # filter(senti_robot_chap_01, PositivityGI == 0, SentimentGI != 0) %>% select(WordCount) %>% colSums() ## 187
    # filter(senti_robot_chap_01, PositivityGI == NegativityGI, PositivityGI != 0) %>% select(WordCount) %>% colSums() ## 170
    # filter(senti_robot_chap_01, PositivityGI == NegativityGI, PositivityGI == 0) %>% select(WordCount) %>% colSums() ## 129
    # filter(senti_robot_chap_01, SentimentGI < 0) %>% select(WordCount) %>% colSums() ## 367
    # filter(senti_robot_chap_01, SentimentGI < 0) %>% select(WordCount) %>% count() ## 38
    # filter(senti_robot_chap_01, SentimentGI > 0) %>% select(WordCount) %>% colSums() ## 434
    # filter(senti_robot_chap_01, SentimentGI > 0) %>% select(WordCount) %>% count() ## 47


tab_rob_all <- data.frame(matrix(0, ncol = 9, nrow = 4))
rownames(tab_rob_all) <- c('neg', 'pos', 'neu', 'sum')
tab_rob_all['neg', ] <- sapply(ls_senti_robot_chap_all, function(x) nrow(select(filter(x, SentimentGI < 0), WordCount)))
tab_rob_all['pos', ] <- sapply(ls_senti_robot_chap_all, function(x) nrow(select(filter(x, SentimentGI > 0), WordCount)))
tab_rob_all['neu', ] <- sapply(ls_senti_robot_chap_all, function(x) nrow(select(filter(x, SentimentGI == 0), WordCount)))
tab_rob_all['sum', ] <- colSums(tab_rob_all)
colnames(tab_rob_all) <- c('rob_chap_01', 'rob_chap_02', 'rob_chap_03',
                           'rob_chap_04', 'rob_chap_05', 'rob_chap_06',
                           'rob_chap_07', 'rob_chap_08', 'rob_chap_09')
tab_rob_all
  #     rob_chap_01 rob_chap_02 rob_chap_03 rob_chap_04 rob_chap_05 rob_chap_06 rob_chap_07 rob_chap_08 rob_chap_09
  # neg          38          34          36          28          37          46          19          26          11
  # pos          47          48          50          33          29          58          30          66          28
  # neu          50          31          33          39          31          51          28          45          20
  # sum         135         113         119         100          97         155          77         137          59

# tab procentowa
tab_rob_all_proc <- data.frame(matrix(0, ncol = 9, nrow = 3))
rownames(tab_rob_all_proc) <- c('neg','neu', 'pos')
tab_rob_all_proc['neg', ] <- round(tab_rob_all[1, ]/tab_rob_all[4, ]*100, 2)
tab_rob_all_proc['pos', ] <- round(tab_rob_all[2, ]/tab_rob_all[4, ]*100, 2)
tab_rob_all_proc['neu', ] <- round(tab_rob_all[3, ]/tab_rob_all[4, ]*100, 2)
colnames(tab_rob_all_proc) <- c('rob_chap_01', 'rob_chap_02', 'rob_chap_03',
                                'rob_chap_04', 'rob_chap_05', 'rob_chap_06',
                                'rob_chap_07', 'rob_chap_08', 'rob_chap_09')
tab_rob_all_proc
  #     rob_chap_01 rob_chap_02 rob_chap_03 rob_chap_04 rob_chap_05 rob_chap_06 rob_chap_07 rob_chap_08 rob_chap_09
  # neg       28.15       30.09       30.25          28       38.14       29.68       24.68       18.98       18.64
  # neu       37.04       27.43       27.73          39       31.96       32.90       36.36       32.85       33.90
  # pos       34.81       42.48       42.02          33       29.90       37.42       38.96       48.18       47.46

tab_chap_all <- data.frame(matrix(0, ncol = 9, nrow = 4))
rownames(tab_chap_all) <- c('neg', 'pos', 'neu', 'sum')
tab_chap_all['neg', ] <- sapply(ls_senti_chap_all, function(x) nrow(select(filter(x, SentimentGI < 0), WordCount)))
tab_chap_all['pos', ] <- sapply(ls_senti_chap_all, function(x) nrow(select(filter(x, SentimentGI > 0), WordCount)))
tab_chap_all['neu', ] <- sapply(ls_senti_chap_all, function(x) nrow(select(filter(x, SentimentGI == 0), WordCount)))
tab_chap_all['sum', ] <- colSums(tab_chap_all)
colnames(tab_chap_all) <- c('all_chap_01', 'all_chap_02', 'all_chap_03',
                            'all_chap_04', 'all_chap_05', 'all_chap_06',
                            'all_chap_07', 'all_chap_08', 'all_chap_09')
tab_chap_all
  #     all_chap_01 all_chap_02 all_chap_03 all_chap_04 all_chap_05 all_chap_06 all_chap_07 all_chap_08 all_chap_09
  # neg         139         178         169         209         179         232         217         162         140
  # pos         226         203         229         239         177         290         244         296         252
  # neu         213         221         202         296         248         323         313         284         235
  # sum         578         602         600         744         604         845         774         742         627

# tab procentowa
tab_chap_all_proc <- data.frame(matrix(0, ncol = 9, nrow = 3))
rownames(tab_chap_all_proc) <- c('neg', 'neu', 'pos')
tab_chap_all_proc['neg', ] <- round(tab_chap_all[1, ]/tab_chap_all[4, ]*100, 2)
tab_chap_all_proc['pos', ] <- round(tab_chap_all[2, ]/tab_chap_all[4, ]*100, 2)
tab_chap_all_proc['neu', ] <- round(tab_chap_all[3, ]/tab_chap_all[4, ]*100, 2)
colnames(tab_chap_all_proc) <- c('all_chap_01', 'all_chap_02', 'all_chap_03',
                                 'all_chap_04', 'all_chap_05', 'all_chap_06',
                                 'all_chap_07', 'all_chap_08', 'all_chap_09')
tab_chap_all_proc
  #     all_chap_01 all_chap_02 all_chap_03 all_chap_04 all_chap_05 all_chap_06 all_chap_07 all_chap_08 all_chap_09
  # neg       24.05       29.57       28.17       28.09       29.64       27.46       28.04       21.83       22.33
  # neu       36.85       36.71       33.67       39.78       41.06       38.22       40.44       38.27       37.48
  # pos       39.10       33.72       38.17       32.12       29.30       34.32       31.52       39.89       40.19

par(mfcol = c(3, 3))
# chap_01
barplot(cbind(tab_rob_all_proc[ , 1], tab_chap_all_proc[ , 1]),
        las = 2, col = c( 'dodgerblue4', 'gray', 'orangered'), density = c(99, 40, 99),
        main = 'Chap 01')
mtext(side = 1, text = c('roboty', 'całość'), at = c(0.7, 1.9), font = 4)
# chap_02
barplot(cbind(tab_rob_all_proc[ , 2], tab_chap_all_proc[ , 2]),
        las = 2, col = c( 'dodgerblue4', 'gray', 'orangered'), density = c(99, 40, 99),
        main = 'Chap 02')
mtext(side = 1, text = c('roboty', 'całość'), at = c(0.7, 1.9), font = 4)
# chap_03
barplot(cbind(tab_rob_all_proc[ , 3], tab_chap_all_proc[ , 3]),
        las = 2, col = c( 'dodgerblue4', 'gray', 'orangered'), density = c(99, 40, 99),
        main = 'Chap 03')
mtext(side = 1, text = c('roboty', 'całość'), at = c(0.7, 1.9), font = 4)
# chap_04
barplot(cbind(tab_rob_all_proc[ , 4], tab_chap_all_proc[ , 4]),
        las = 2, col = c( 'dodgerblue4', 'gray', 'orangered'), density = c(99, 40, 99),
        main = 'Chap 04')
mtext(side = 1, text = c('roboty', 'całość'), at = c(0.7, 1.9), font = 4)
# chap_05
barplot(cbind(tab_rob_all_proc[ , 5], tab_chap_all_proc[ , 5]),
        las = 2, col = c( 'dodgerblue4', 'gray', 'orangered'), density = c(99, 40, 99),
        main = 'Chap 05')
mtext(side = 1, text = c('roboty', 'całość'), at = c(0.7, 1.9), font = 4)
# chap_06
barplot(cbind(tab_rob_all_proc[ , 6], tab_chap_all_proc[ , 6]),
        las = 2, col = c( 'dodgerblue4', 'gray', 'orangered'), density = c(99, 40, 99),
        main = 'Chap 06')
mtext(side = 1, text = c('roboty', 'całość'), at = c(0.7, 1.9), font = 4)
# chap_07
barplot(cbind(tab_rob_all_proc[ , 7], tab_chap_all_proc[ , 7]),
        las = 2, col = c( 'dodgerblue4', 'gray', 'orangered'), density = c(99, 40, 99),
        main = 'Chap 07')
mtext(side = 1, text = c('roboty', 'całość'), at = c(0.7, 1.9), font = 4)
# chap_08
barplot(cbind(tab_rob_all_proc[ , 8], tab_chap_all_proc[ , 8]),
        las = 2, col = c( 'dodgerblue4', 'gray', 'orangered'), density = c(99, 40, 99),
        main = 'Chap 08')
mtext(side = 1, text = c('roboty', 'całość'), at = c(0.7, 1.9), font = 4)
# chap_09
barplot(cbind(tab_rob_all_proc[ , 9], tab_chap_all_proc[ , 9]),
        las = 2, col = c( 'dodgerblue4', 'gray', 'orangered'), density = c(99, 40, 99),
        main = 'Chap 09')
mtext(side = 1, text = c('roboty', 'całość'), at = c(0.7, 1.9), font = 4)

par(mfcol = c(par_def$mfcol))

# _03 spolaryzowanie ----
# __x polarity ----
pol_chap_all <- polarity(text.var = sen_chap_all$txt, grouping.var = sen_chap_all$chap)
pol_chap_all
    #    chap total.sentences total.words ave.polarity sd.polarity stan.mean.polarity
    # 1 ch_01             607        7181       -0.016       0.263             -0.062
    # 2 ch_02             625        6405        0.000       0.298              0.001
    # 3 ch_03             629        6370       -0.010       0.276             -0.036
    # 4 ch_04             788        7026       -0.001       0.266             -0.002
    # 5 ch_05             649        5945       -0.034       0.267             -0.127
    # 6 ch_06             891        9566       -0.021       0.261             -0.079
    # 7 ch_07             834        7452       -0.041       0.264             -0.155
    # 8 ch_08             786        8475       -0.001       0.273             -0.004
    # 9 ch_09             680        8472       -0.010       0.262             -0.039
plot(pol_chap_all)
plot(scores(pol_chap_all))

# __x formality ----
dim(sen_chap_all) ## 6360 3
form_chap_all <- formality(text.var = sen_chap_all$txt, grouping.var = sen_chap_all$chap)
form_chap_all
    #    chap word.count formality
    # 1 ch_09       8521     59.15
    # 2 ch_02       6596     55.81
    # 3 ch_03       6516     55.49
    # 4 ch_01       7347     54.46
    # 5 ch_08       8643     53.88
    # 6 ch_06       9769     52.76
    # 7 ch_04       7260     52.44
    # 8 ch_07       7683     52.21
    # 9 ch_05       6165     49.59
form_chap_all$form.prop.by
    #    chap word.count  noun   adj  prep articles pronoun  verb adverb interj other
    # 1 ch_01       7347 21.61  9.30 14.09     7.36   12.41 21.31   9.45   0.26  4.21
    # 2 ch_02       6596 22.23 10.23 13.22     8.13   11.90 21.10   8.85   0.32  4.02
    # 3 ch_03       6516 22.50  9.16 13.93     7.98   11.99 22.07   8.32   0.23  3.82
    # 4 ch_04       7260 21.25  9.61 12.82     6.91   13.36 23.39   8.65   0.33  3.66
    # 5 ch_05       6165 18.78  9.20 12.91     6.62   15.34 24.22   8.50   0.26  4.17
    # 6 ch_06       9769 19.83 10.63 13.35     7.22   13.04 23.58   8.54   0.34  3.48
    # 7 ch_07       7683 20.92  9.67 12.05     7.60   13.09 23.23   9.05   0.46  3.93
    # 8 ch_08       8643 20.83  9.21 13.78     8.31   13.29 22.25   8.58   0.24  3.51
    # 9 ch_09       8521 21.86 11.61 14.48     8.92   10.52 19.76   8.06   0.23  4.55

plot(form_chap_all)

# __x formality of dictionary ----
df_dict_pos <- data.frame(word = DictionaryGI$positive,
                          senti = 'pos')
df_dict_neg <- data.frame(word = DictionaryGI$negative,
                          senti = 'neg')
df_dict <- rbind(df_dict_pos, df_dict_neg)
dim(df_dict) ## 3642 2

form_dict <- formality(text.var = df_dict[ ,1], grouping.var = df_dict[ ,2])
form_dict
    #     2 word.count formality
    # 1 neg       2005     86.36
    # 2 pos       1637     85.09

summary(form_dict)
    #              Length Class      Mode     
    # text         3642   -none-     character
    # POStagged       3   data.frame list     
    # POSprop        22   data.frame list     
    # POSfreq        22   data.frame list     
    # POSrnp         22   data.frame list     
    # percent         1   -none-     logical  
    # zero.replace    1   -none-     numeric  
    # pos.by.freq    23   data.frame list     
    # pos.by.prop    23   data.frame list     
    # pos.by.rnp     23   data.frame list     
    # form.freq.by   11   data.frame list     
    # form.prop.by   11   data.frame list     
    # formality       3   data.frame list     
    # pos.reshaped    4   data.frame list     
    # group           1   -none-     character

form_dict$form.freq.by
    #     2 word.count noun adj prep articles pronoun verb adverb interj other
    # 1 pos       1637  999 381    3        0       2  177     55      0    20
    # 2 neg       2005 1301 412    6        0       5  223     29      4    25

form_dict$form.prop.by  ## 2 11
    #     2 word.count  noun   adj prep articles pronoun  verb adverb interj other
    # 1 pos       1637 61.03 23.27 0.18        0    0.12 10.81   3.36    0.0  1.22
    # 2 neg       2005 64.89 20.55 0.30        0    0.25 11.12   1.45    0.2  1.25

plot(form_dict)

# wykres
plot(t(form_dict$form.prop.by[1, 3:11]), type = 'b', col = 'red', lty = 1, pch = 'o', lwd = 2, 
     ylim = c(-10, 70), xaxt = 'n', las = 1,
     main = 'Statystyka - części mowy w słowniku sentymentu GI',
     xlab = '', ylab = '[%]')
points(1:9, t(form_dict$form.prop.by[2, 3:11]), type = 'b', col = 'blue', lty = 2, pch = 'x', lwd = 2)
axis(1, at = seq(1, 9, 1), labels = colnames(form_dict$form.prop.by)[3:11], 
     cex.axis = 0.9, las = 2)
# mtext(side = 3, text = (form_dict$form.prop.by[1, 3:11]), at = seq(1, 9, 1),
#       col = 'red', font = 4, cex = 0.8)
# mtext(side = 1, text = (form_dict$form.prop.by[2, 3:11]), at = seq(1, 9, 1),
#       col = 'blue', font = 4, cex = 0.8)
legend('topright', legend = c('pos', 'neg'), col = c('red', 'blue'),
      lty = c(1,2), pch = c('o', 'x'), box.lty = 0)
grid()
abline(h = 0)

# __x diversity ----
div_chap_all <- diversity(text.var = sen_chap_all$txt, grouping.var = sen_chap_all$chap)
div_chap_all
    #    chap   wc simpson shannon collision berger_parker brillouin
    # 1 ch_01 7120   0.992   6.195     4.806         0.044     5.868
    # 2 ch_02 6354   0.991   6.148     4.746         0.060     5.824
    # 3 ch_03 6321   0.991   6.146     4.728         0.061     5.818
    # 4 ch_04 6964   0.993   6.171     4.962         0.045     5.865
    # 5 ch_05 5912   0.992   6.057     4.823         0.046     5.742
    # 6 ch_06 9497   0.992   6.205     4.869         0.050     5.934
    # 7 ch_07 7394   0.992   6.188     4.842         0.052     5.885
    # 8 ch_08 8428   0.991   6.163     4.739         0.050     5.866
    # 9 ch_09 8404   0.989   6.124     4.537         0.068     5.826

plot(div_chap_all)

# __x dispersion -----
# okreslenia dla robotów
ls_word_robot <- c('robot', 'robots', 'machine', 'machines', 'computer', 'computers', 'humanoid', 'robotics')
ls_word_robot %in% DictionaryGI$negative
    ## FALSE FALSE FALSE FALSE FALSE
ls_word_robot %in% DictionaryGI$positive
    ## FALSE FALSE FALSE FALSE FALSE
analyzeSentiment(ls_word_robot)[1:4]
    #   WordCount SentimentGI NegativityGI PositivityGI
    # 1         1           0            0            0
    # 2         1           0            0            0
    # 3         1           0            0            0
    # 4         1           0            0            0
    # 5         1           0            0            0

dispersion_plot(text.var = sen_chap_all$txt,
                grouping.var = sen_chap_all$chap,
                match.terms = ls_word_robot,
                color = 'cadetblue', bg.color = 'white', total.color = 'navyblue')

# określenia dla ludzi
ls_word_human <- c('human', 'man', 'men', 'women', 'woman', 'child')
ls_word_human %in% DictionaryGI$negative
  ## FALSE FALSE FALSE FALSE FALSE FALSE
ls_word_human %in% DictionaryGI$positive
  ## TRUE FALSE FALSE FALSE FALSE FALSE
analyzeSentiment(ls_word_human)[1:4]
    #   WordCount SentimentGI NegativityGI PositivityGI
    # 1         1           1            0            1
    # 2         1           1            0            1
    # 3         1           0            0            0
    # 4         1           0            0            0
    # 5         1           0            0            0
    # 6         1           0            0            0

dispersion_plot(text.var = sen_chap_all$txt,
                grouping.var = sen_chap_all$chap, #symbol = '*',
                match.terms = c('human', 'man', 'men', 'woman', 'women', 'child'),
                color = 'lightseagreen', bg.color = 'white', total.color = 'navyblue')



# imiona robotów
ls_name_robot

dispersion_plot(text.var = sen_chap_all$txt,
                grouping.var = sen_chap_all$chap,
                match.terms = c('robbi', # chap_01
                                'speedy', #'spd', # chap_02
                                'cutie', # 'qt', # chap_03
                                'dave', # 'dv', # chap_04
                                'herbie',# 'rb', # chap_05
                                'nestor', # 'ns', #chap_06
                                'brain', # chap_07
                                'byerley'), # chap_08 & chap_09,
                color = 'orange', bg.color = 'white', total.color = 'navyblue')

# imina bohaterów
ls_name_ppl_2 <- c('gloria', 'weston', 'george', 'grace', #chap_01
                 'susan', 'calvin', # all
                 'mike', 'michael', 'donovan', # all
                 'greg', 'gregory', 'powell', # all
                 'lawrence', 'robertson', ## U.S. Robots
                 'peter', 'bogert',  ## U.S. Robots
                 'alfred', ' lanning', 'milton', ' ashe',  ## U.S. Robots
                 'kallner',  ## U.S. Robots
                 'gerald', ' black', ## U.S. Robots
                 'tanning', ## U.S. Robots
                 'francis', ' quinn', ## end
                 'lenton', 'byerley') ## end
length(ls_name_ppl_2) ## 28

dispersion_plot(text.var = sen_chap_all$txt,
                grouping.var = sen_chap_all$chap,
                match.terms = ls_name_ppl_2[1:4],
                color = 'orange', bg.color = 'white', total.color = 'navyblue')
dispersion_plot(text.var = sen_chap_all$txt,
                grouping.var = sen_chap_all$chap,
                match.terms = ls_name_ppl_2[5:12],
                color = 'orange', bg.color = 'white', total.color = 'navyblue')
dispersion_plot(text.var = sen_chap_all$txt,
                grouping.var = sen_chap_all$chap,
                match.terms = ls_name_ppl_2[13:24],
                color = 'orange', bg.color = 'white', total.color = 'navyblue')
dispersion_plot(text.var = sen_chap_all$txt,
                grouping.var = sen_chap_all$chap,
                match.terms = ls_name_ppl_2[25:28],
                color = 'orange', bg.color = 'white', total.color = 'navyblue')
