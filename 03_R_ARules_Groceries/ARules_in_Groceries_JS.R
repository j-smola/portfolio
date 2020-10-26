# copyright 2018 by Janina Smoła
# UTF8

# WSTĘP -----------------------------------------------------------------
rm(list = ls())

library(arules)
library(arulesViz)
library(dplyr)
library(igraph)


## Do opracowaniazadania wykorzystano artykuły:
## https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf
## https://cran.r-project.org/web/packages/arules/vignettes/arules.pdf
## http://r-statistics.co/Association-Mining-With-R.html

# UWAGA komentarze po polsku, za wyjątkiem tytułów rysunków, które są w jęz.ang. tak jak
# wykorzystywana baza

# CELE GŁÓWNE EKSPERYMENTU --------------------------------------------------------
# wybór interesujących reguł (definicja na bazie dostępnych parametrów reguł);
# wybór reguły najlepszej
# znalezienie praktycznych zastosować odkrytych reguł

# OPIS POSTAWIONEGO ZADANIA
# Właściciel sklepu wielobranżowego postanowił zysk zainwestować w rozwój; zaplanowano powiększenie
# powierzchni użytkowej sklepu oraz lekką rearanżacje. Wychodząc naprzeciw potrzebom klientów rozwijane
# miały być działy/grupy produktów  cieszące się największym zainteresowaniem, a o ich lokalizacji w obrębie 
# nowej powierzchni zadecydować miała zasada maksymalizacji ewentualnych zakupów (możliwość zakupów impulsowych). 
# Wybór działów/grup produktów  do rozwoju i ewentualny ich układ bazować ma na analizie list zakupów (najpopularniejsze)
# natomiast ich wzajemną lokalizację określić miałyby najciekwasze/ najsilniejsze reguły asocjacyjne.
# Na podstawie liczby transakcji w wykorzystywanym zbiorze zakłada się, że sklep ma rozmiary marketu osiedlowego 
# - 9835 transakcji z okresu 1 miesiąca tj. ok. 30 transakcji na godzinę, przy 12 godzinnym dniu pracy 
# i 7 dniowym tygodniu pracy.


# EKSPERYMENT - ELEMENTY CZĘSTE -------------------------------------------
# _1 Źrodlo - zbiór Groceries, lista produktów i charakterystyka bazy ----
data(Groceries)
typeof(Groceries) 
## S4
class(Groceries)
# [1] "transactions"
# attr(,"package")
# [1] "arules"
dim(Groceries) ## 9835  169

length(colnames(Groceries)) ##169
itemLabels(Groceries) #wszystkie produkty - tekst

colnames(Groceries)[1:20]
colnames(Groceries)[149:169]

summary(Groceries)
# transactions as itemMatrix in sparse format with
# 9835 rows (elements/itemsets/transactions) and
# 169 columns (items) and a density of 0.02609146 
# 
# most frequent items:
#       whole milk other vegetables       rolls/buns             soda           yogurt          (Other) 
#             2513             1903             1809             1715             1372            34055 
# 
# element (itemset/transaction) length distribution:
# sizes
#    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   26   27   28   29   32 
# 2159 1643 1299 1005  855  645  545  438  350  246  182  117   78   77   55   46   29   14   14    9   11    4    6    1    1    1    1    3    1 
# 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   2.000   3.000   4.409   6.000  32.000 
# 
# includes extended item information - examples:
#        labels  level2           level1
# 1 frankfurter sausage meat and sausage
# 2     sausage sausage meat and sausage
# 3  liver loaf sausage meat and sausage

# transakcje
inspect(head(Groceries)) #pierwsze 6 z bazy danych

# items                                                                
# [1] {citrus fruit,semi-finished bread,margarine,ready soups}             
# [2] {tropical fruit,yogurt,coffee}                                       
# [3] {whole milk}                                                         
# [4] {pip fruit,yogurt,cream cheese, meat spreads}                        
# [5] {other vegetables,whole milk,condensed milk,long life bakery product}
# [6] {whole milk,butter,yogurt,rice,abrasive cleaner}  

inspect(tail(Groceries)) # ostatnie 6 z bazy danych
# [1] {tropical fruit, other vegetables, domestic eggs, zwieback, ketchup, 
#     soda, dishes} 
# [2] {sausage, chicken, beef, hamburger meat, citrus fruit, grapes, 
#     root vegetables, whole milk, butter, whipped/sour cream, flour, 
#     coffee, red/blush wine, salty snack, chocolate, hygiene articles, napkins} 
# [3] {cooking chocolate} 
# [4] {chicken, citrus fruit, other vegetables, butter, yogurt, frozen dessert, 
#     domestic eggs, rolls/buns, rum, cling film/bags} 
# [5] {semi-finished bread, bottled water, soda, bottled beer} 
# [6] {chicken, tropical fruit, other vegetables, vinegar, shopping bags}

View(Groceries@itemInfo)

length(Groceries@data@i) #indices ## 43367
length(Groceries@data@p) #pointers ## 9836

str(Groceries@itemInfo) ## = str(df_groc)
# 'data.frame':	169 obs. of  3 variables:
# $ labels: chr  "frankfurter" "sausage" "liver loaf" "ham" ...
# $ level2: Factor w/ 55 levels "baby food","bags",..: 44 44 44 44 44 44 44 42 42 41 ...
# $ level1: Factor w/ 10 levels "canned food",..: 6 6 6 6 6 6 6 6 6 6 ...

# długości kolejnych transakcji
size(Groceries)
table(size(Groceries))
summary(size(Groceries))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   2.000   3.000   4.409   6.000  32.000 
boxplot(size(Groceries), col = 'orange', horizontal = TRUE,
        main = 'Size - number of products in each of transaction')
abline(v = 3, col = 'red')

# wizualizacja macierzy rzadkiej (macierz opisująca transakcje) - losowe 100 transakcji
image(sample(Groceries, 100),
      main = 'Image of spare matrix = transaction & products')

# _____CHARAKTERYSTYKA BAZY DANYCH ----
# 1 zbiór zawiera 9835 transakcji zawierających w sumie 169 typów produktów
# 2 zbiór danych jest raczej rzadki (gęstość - density of 0.02609146 -> 2,6%)
# 3 najczęściej występującym produktem jest mleko (whole milk - 2513), 
#     a następnie inne warzywa (other vegetables - 1903)
# 4 typowa transakcja (lista zakupów) zawiera mniej niż 5 produktów (Mean = 4.409) - mediana wynosi 3
# 5 zbiór danych zawiera dodatkowe inforamcje o produktach - grupowanie produktów na dwóch poziomach
#     tj. level1 (10 elementów) i level2 (55 elementów).


# _2 analiza listy produktów ----
df_groc <- Groceries@itemInfo
any(is.na(df_groc)) ## FALSE
colnames(df_groc) ## "labels" "level2" "level1"

length(levels(df_groc$level1)) ##10
levels(df_groc$level1)
# [1] "canned food"          "detergent"            "drinks"               "fresh products"       "fruit and vegetables" "meat and sausage"    
# [7] "non-food"             "perfumery"            "processed food"       "snacks and candies"  

length(levels(df_groc$level2)) ##55
levels(df_groc$level2)
#  [1] "baby food"                       "bags"                            "bakery improver"                 "bathroom cleaner"               
#  [5] "beef"                            "beer"                            "bread and backed goods"          "candy"                          
#  [9] "canned fish"                     "canned fruit/vegetables"         "cheese"                          "chewing gum"                    
# [13] "chocolate"                       "cleaner"                         "coffee"                          "condiments"                     
# [17] "cosmetics"                       "dairy produce"                   "delicatessen"                    "dental care"                    
# [21] "detergent/softener"              "eggs"                            "fish"                            "frozen foods"                   
# [25] "fruit"                           "games/books/hobby"               "garden"                          "hair care"                      
# [29] "hard drinks"                     "health food"                     "jam/sweet spreads"               "long-life bakery products"      
# [33] "meat spreads"                    "non-alc. drinks"                 "non-food house keeping products" "non-food kitchen"               
# [37] "packaged fruit/vegetables"       "perfumery"                       "personal hygiene"                "pet food/care"                  
# [41] "pork"                            "poultry"                         "pudding powder"                  "sausage"                        
# [45] "seasonal products"               "shelf-stable dairy"              "snacks"                          "soap"                           
# [49] "soups/sauces"                    "staple foods"                    "sweetener"                       "tea/cocoa drinks"               
# [53] "vegetables"                      "vinegar/oils"                    "wine"     

df_groc$labels

# lista produktów przypisanych do poszczególnych grup produktowych na poziomie 2
list_lvl2 <- list()
(lbl_lvl2 <- levels(df_groc$level2))

for (i in 1:length(lbl_lvl2)){ #tworzy listę produktów w grupach produktowych
  name_lvl2 <- lbl_lvl2[i]
  iter_list <- df_groc %>% filter(level2 == lbl_lvl2[i]) %>% select(labels) %>% as.list()
  list_lvl2[name_lvl2] <- iter_list
}

# produkty przypisane do grup produktowych
list_lvl2
str(list_lvl2)

# liczba produktów w każdej grupie 
len_list_lvl2 <- sapply(list_lvl2, function(x) length(x)) 
table(len_list_lvl2)
# len_list_lvl2
#  1  2  3  4  5  6  7  8 
# 15 11 10  8  4  2  2  3
summary(len_list_lvl2)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   3.000   3.073   4.000   8.000 
boxplot(len_list_lvl2, horizontal = TRUE, col = 'lawngreen', 
        main = 'Number of items in each products\' group')
abline(v = 3, col = 'red')

Groceries_lvl2 <- aggregate(Groceries, by = 'level2')
inspect(head(Groceries)) #pierwsze 6 transakcji z bazy - wg produktów
# [1] {citrus fruit,semi-finished bread,margarine,ready soups}             
# [2] {tropical fruit,yogurt,coffee}                                       
# [3] {whole milk}                                                         
# [4] {pip fruit,yogurt,cream cheese ,meat spreads}                        
# [5] {other vegetables,whole milk,condensed milk,long life bakery product}
# [6] {whole milk,butter,yogurt,rice,abrasive cleaner} 
inspect(head(Groceries_lvl2)) #pierwsze 6 transakcji z bazy - analogicznie do ww. - wg grup produktów
# [1] {bread and backed goods,fruit,soups/sauces,vinegar/oils}               
# [2] {coffee,dairy produce,fruit}                                           
# [3] {dairy produce}                                                        
# [4] {cheese,dairy produce,fruit,meat spreads}                              
# [5] {dairy produce,long-life bakery products,shelf-stable dairy,vegetables}
# [6] {cleaner,dairy produce,staple foods} 

# _____DZIAŁY/ GRUPY TOWAROWE ----
# 1 "level1"/ Poziom 1 określa najogólniejszy podział na działy w sklepie, których jest 10
# 2 Natomiast "level2"/ Poziom 2 określa dokładniejsze 55 grup towarowych
# 3 Z uwagi na jakość analizy uznano, że do przygotowania decyzji zarządczych odpowiedniejsza będzie analiza
#   oparta o poszczególne towary oraz na poziomie ogólniejszym grupy towarowe ("level2"/ Poziom 2)
#   W daleszej części materiału analiza jest prowadzona dwutorowo dla produktów i ich grup (level2).
# 4 W ramach level2 196 produktów podzielono na 55 grup produktowych 
#   średnio grupa liczy więc 3 produkty, natomiast jest 7 grup z 6-cioma lub więcej produktami


# _3 wykresy dla produktów - Groceries ----
# Wsparcie względne dla wszystkich 169 produktów
itemFrequencyPlot(Groceries, population = Groceries, popCol = 'red', 
                  xlab = 'product name', ylab = 'relative support', 
                  main = 'Plot of frequency for all products')

# 20 produktów o najwyższym wsparciu względnym
itemFrequencyPlot(Groceries, topN = 20, col = rainbow(30), horiz = TRUE,
                  xlab = 'relative support', #ylab = 'product name',
                  main = 'Plot of frequency for top 20 products')
abline(v = 0.05, col = 'black', lwd = 2)

# produkty, dla których min wsparcie względne wynosi 0.10
itemFrequencyPlot(Groceries, support = 0.10, col = 'gold',horiz = TRUE,
                  xlab = 'relative support', #ylab = 'product name',
                  main = 'Plot of frequency for products with min support = 0.1')
abline(v = 0.10, col = 'blue', lwd = 2)

# produkty, dla których min wsparcie względne wynosi 0.05
itemFrequencyPlot(Groceries, support = 0.05, col = 'orange', horiz = TRUE,
                  xlab = 'relative support', #ylab = 'product name',
                  main = 'Plot of frequency for products with min support = 0.05')
abline(v = 0.05, col = 'green', lwd = 2)

# _4 wykresy Groceries_lvl2 (Poziom level2)----

# Wsparcie względne dla wszystkich 55 grup produktów
itemFrequencyPlot(Groceries_lvl2, population = Groceries_lvl2, popCol = 'red', 
                  xlab = 'products\' group name', ylab = 'relative support', 
                  main = 'Plot of frequency for all products\' groups')

# 20 grup produktów o najwyższym wsparciu względnym
itemFrequencyPlot(Groceries_lvl2, topN = 20, col = rainbow(30), horiz = TRUE,
                  xlab = 'relative support', #ylab = 'products\' group name',
                  main = 'Plot of frequency for top 20 products\' group')
abline(v = 0.05, col = 'black', lwd = 2)

# grupy produktów, dla których min wsparcie względne wynosi 0.10
itemFrequencyPlot(Groceries_lvl2, support = 0.10, col = 'gold', horiz = TRUE,
                  xlab = 'relative support', #ylab = 'products\' group name',
                  main = 'Plot of frequency for products\' group with min support = 0.10')
abline(v = 0.10, col = 'blue', lwd = 2)

# grupy produktów, dla których min wsparcie względne wynosi 0.05
itemFrequencyPlot(Groceries_lvl2, support = 0.05, col = 'orange', horiz = TRUE,
                  xlab = 'relative support', #ylab = 'products\' group name',
                  main = 'Plot of frequency for products\' group with min support = 0.05')
abline(v = 0.05, col = 'green', lwd = 2)


# _5A wsparcie i wsparcie względne poszczególnych produktów ----
# wsparcie relatywne ~ prawdopodobieństow (tj. wsparcie absolutne/9835)
tab_freq <- itemFrequency(Groceries, type = 'relative')  ## frequency/support 
# uporządkowanie listy produktów wg malejącej wartości wsparcia względnego
tab_freq <- sort(tab_freq, decreasing = TRUE)

is.vector(tab_freq) ## TRUE
names(tab_freq) ## nazwy produktów
str(tab_freq)
# Named num [1:169] 0.256 0.193 0.184 0.174 0.14 ...
# - attr(*, "names")= chr [1:169] "whole milk" "other vegetables" "rolls/buns" "soda" ...

# stat
summary(tab_freq)
boxplot(tab_freq, horizontal = TRUE, col = 'gold',
        xlab = 'relative support', main = 'Relative support for each single product') 

print(tab_freq)

head(tab_freq) # 6 pierwszych produktów - o największym wsparciu względnym
# whole milk other vegetables       rolls/buns             soda           yogurt    bottled water 
#  0.2555160        0.1934926        0.1839349        0.1743772        0.1395018        0.1105236 
tail(tab_freq) # 6 ostatnich produktów - o najmniejszym wsparciu względnym
# baby cosmetics       kitchen utensil                  bags preservation products             baby food  sound storage medium 
#   0.0006100661          0.0004067107          0.0004067107          0.0002033554          0.0001016777          0.0001016777

# wsparcie absolutne/bezwzględne zbioru elementu = liczebność transakcji z danym produktem
tab_freq_ab <- itemFrequency(Groceries, type = 'absolute') 
tab_freq_ab <- sort(tab_freq_ab, decreasing = TRUE)
print(tab_freq_ab)

# stat
summary(tab_freq_ab)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0    38.0   103.0   256.6   305.0  2513.0 

boxplot(tab_freq_ab, horizontal = TRUE, col = 'gold', #outline = FALSE,
        xlab = 'support', main = 'Support for each single product') 

# ANALIZA propozycje minSup
# elementy=produkty ze wsparciem względnym > 20% 
# (wsparcie bezwzględne: 1968 = 0.20 * 9835)
print(tab_freq[tab_freq > 0.20]) ## whole milk  0.255516 
length(tab_freq[tab_freq > 0.20]) ## 1

# elementy=produkty ze wspaciem względnym >= 10%
# (wsparcie bezwzględne: 984 = 0.10 * 9835)
length(tab_freq[tab_freq >= 0.1]) ## 8 
print(tab_freq[tab_freq >= 0.1])

# whole milk other vegetables       rolls/buns             soda           yogurt    bottled water 
# 0.2555160        0.1934926        0.1839349        0.1743772        0.1395018        0.1105236 
# root vegetables   tropical fruit 
# 0.1089985        0.1049314 

# elementy=produkty ze wspaciem względnym >= 5%
# (wsparcie bezwzględne: 490 = 0.05 * 9835)
length(tab_freq[tab_freq >= 0.05]) ## 28 
print(tab_freq[tab_freq >= 0.05][1:5])
# whole milk other vegetables       rolls/buns             soda           yogurt 
# 0.2555160        0.1934926        0.1839349        0.1743772        0.1395018 

# liczba elementów ze wspaciem >= 100 (~>1%)
length(tab_freq_ab[tab_freq_ab >= 100]) ## 88
print(tab_freq_ab[tab_freq_ab >= 100][1:5])
# frankfurter     sausage         ham        meat     chicken 
#         580         924         256         254         422

# _5B produkty częste ----

#Wybór produktów częstych przy założeniu wsparcia względnego na poziomie 10% (984 transakcje)
(list_pop_products <- names(tab_freq_ab[tab_freq_ab > 984])) ## minSup = 984, minrSup = 0.10

# grupy, do którch należą najpopularniejsze produkty
filter(itemInfo(Groceries), labels %in% list_pop_products) %>% select(level2) %>% unique()
filter(itemInfo(Groceries), labels %in% list_pop_products) %>% select(level2) %>% table() %>% as.data.frame() %>% filter(Freq != 0)
#                        . Freq
# 1 bread and backed goods    1
# 2          dairy produce    2
# 3                  fruit    1
# 4        non-alc. drinks    2
# 5             vegetables    2


# _____WNIOSKI list_pop_products ----
# Najczęsciej występujące produkty to (= produkty częste o wsparciu większym od minSup = 984)
list_pop_products
#  [1] "whole milk"       "other vegetables" "rolls/buns"       "soda"             "yogurt"          
#  [6] "bottled water"    "root vegetables"  "tropical fruit" 

# Najczęściej występujące produkty należą do następujących grup produktów
# bread and backed goods    1 produkt, tj. "rolls/buns"
#          dairy produce    2 produkt, tj. "whole milk"       "yogurt" 
#                  fruit    1 produkt, tj. "tropical fruit" 
#        non-alc. drinks    2 produkt, tj. "soda"             "bottled water" 
#             vegetables    2 produkt, tj. "other vegetables" "root vegetables" 


# SPRAWDZENIE (obliczenie apriori dla częstych elementów)
list_pop_products_spr  <- apriori(data = Groceries, ## object of class transactions
                                  parameter = list(support = 0.003, confidence = 0.2, # zmienione na 1 
                                                   target = 'frequent itemsets'))
summary(list_pop_products_spr)

inspect(sort(subset(list_pop_products_spr, subset = count > 984)))
#     items              support   count
# [1] {whole milk}       0.2555160 2513 
# [2] {other vegetables} 0.1934926 1903 
# [3] {rolls/buns}       0.1839349 1809 
# [4] {soda}             0.1743772 1715 
# [5] {yogurt}           0.1395018 1372 
# [6] {bottled water}    0.1105236 1087 
# [7] {root vegetables}  0.1089985 1072 
# [8] {tropical fruit}   0.1049314 1032 
# A zatem przy zastosowaniu metody alternatywnej uzyskany został ten sam wynik.


# _6A wsparcie i wsparcie względne grup produktów (level2)----
# wsparcie relatywne ~ prawdopodobieństwo (tj. wsparcie absolutne/9835)
tab_freq_lvl2 <- itemFrequency(Groceries_lvl2, type = 'relative')  ## frequency/support 
# uporzadkowanie listy produktów wg malejącej wartości wsparcia względnego
tab_freq_lvl2 <- sort(tab_freq_lvl2, decreasing = TRUE)

is.vector(tab_freq_lvl2) ## TRUE
names(tab_freq_lvl2) ## nazwy grup produktów
str(tab_freq_lvl2)
# Named num [1:55] 0.443 0.346 0.318 0.273 0.249 ...
# - attr(*, "names")= chr [1:55] "dairy produce" "bread and backed goods" "non-alc. drinks" "vegetables" ...

# stat
summary(tab_freq_lvl2)
boxplot(tab_freq_lvl2, horizontal = TRUE, col = 'gold',
        xlab = 'relative support', main = 'Relative support for each single products\' group') 

print(tab_freq_lvl2)

head(tab_freq_lvl2) # 6 pierwszych produktów - o największym wsparciu relatywnym
# dairy produce bread and backed goods        non-alc. drinks             vegetables                  fruit                sausage 
#     0.4430097              0.3455008              0.3179461              0.2730046              0.2491103              0.1891205 

tail(tab_freq_lvl2) # 6 ostatnich produktów - o najmniejszym wsparciu relatywnym
#         fish             soap   pudding powder        hair care personal hygiene        baby food 
# 0.0029486528     0.0026436197     0.0023385867     0.0011184545     0.0010167768     0.0001016777

# wsparcie absolutne/bezwzględne zbioru elementu = liczebność transakcji z danym produktem
tab_freq_lvl2_ab <- itemFrequency(Groceries_lvl2, type = 'absolute') 
tab_freq_lvl2_ab <- sort(tab_freq_lvl2_ab, decreasing = TRUE)
print(tab_freq_lvl2_ab)

# stat
summary(tab_freq_lvl2_ab)
boxplot(tab_freq_lvl2_ab, horizontal = TRUE, col = 'gold', #outline = FALSE,
        xlab = 'support', main = 'Support for each single products\' group') 

# ANALIZA propozycje minSup
# elementy=produkty ze wsparciem relatywnym > 20%
# (wsparcie bezwzględne: (1968 = 0.20 * 9835))
length(tab_freq_lvl2[tab_freq_lvl2 > 0.20]) ## 5
print(tab_freq_lvl2[tab_freq_lvl2 > 0.20])  
# dairy produce bread and backed goods        non-alc. drinks             vegetables                  fruit 
#     0.4430097              0.3455008              0.3179461              0.2730046              0.2491103
print(tab_freq_lvl2_ab[tab_freq_lvl2_ab > 1968])
# dairy produce bread and backed goods        non-alc. drinks             vegetables                  fruit 
#          4357                   3398                   3127                   2685                   2450 

# elementy=produkty ze wsparciem relatywnym > 10%
# (wsparcie bezwzględne: (984 = 0.10 * 9835))
length(tab_freq_lvl2[tab_freq_lvl2 > 0.10]) ## 10
print(tab_freq_lvl2[tab_freq_lvl2 > 0.10][1:5])  
# dairy produce bread and backed goods        non-alc. drinks             vegetables                  fruit 
# 0.4430097              0.3455008              0.3179461              0.2730046              0.2491103 
print(tab_freq_lvl2_ab[tab_freq_lvl2_ab > 984])
# dairy produce bread and backed goods        non-alc. drinks             vegetables                  fruit 
#          4357                   3398                   3127                   2685                   2450 
#       sausage                   beer                 cheese           frozen foods              chocolate 
#          1860                   1530                   1246                   1150                   1070 

# elementy=produkty ze wspaciem relatywnym >= 5%
# (wsparcie bezwzględne: (490 = 0.05 * 9835))
length(tab_freq_lvl2[tab_freq_lvl2 >= 0.05]) ## 21
print(tab_freq_lvl2[tab_freq_lvl2 >= 0.05][1:5])
# dairy produce bread and backed goods        non-alc. drinks             vegetables                  fruit 
# 0.4430097              0.3455008              0.3179461              0.2730046              0.2491103 
print(tab_freq_lvl2_ab[tab_freq_lvl2_ab > 490][1:5]) 
# dairy produce bread and backed goods        non-alc. drinks             vegetables                  fruit 
# 4357                   3398                   3127                   2685                   2450 

# grupy produktów ze wspaciem >= 100 (~>1%)
length(tab_freq_lvl2_ab[tab_freq_lvl2_ab >= 100]) ## 44
print(tab_freq_lvl2_ab[tab_freq_lvl2_ab >= 100][1:5])


# _6B częste grupy produktów (level2) ----
(list_pop_gr_products <- names(sort(tab_freq_lvl2_ab[tab_freq_lvl2_ab > 984], decreasing = TRUE))) ## minSup = 984, minrSup = 0.10
# [1] "dairy produce"          "bread and backed goods" "non-alc. drinks"        "vegetables"            
# [5] "fruit"                  "sausage"                "beer"                   "cheese"                
# [9] "frozen foods"           "chocolate"

head(tab_freq_lvl2)
# dairy produce bread and backed goods        non-alc. drinks             vegetables                  fruit                sausage 
#     0.4430097              0.3455008              0.3179461              0.2730046              0.2491103              0.1891205 
head(tab_freq_lvl2_ab)
# dairy produce bread and backed goods        non-alc. drinks             vegetables                  fruit                sausage 
#          4357                   3398                   3127                   2685                   2450                   1860  


# PODSUMOWANIE
# produkty z grupy produkty mleczne występują w 4357 z 9835 transakcji (44,3%)
# produkty z grupy pieczywo i wypieki - w 3398 z 9835 transakcji (34,5%)
# produkty z grupy napoje niealkoholowe - 3127 z 9835 transakcji (31,8%)
# produkty z grupy warzywa - 2685 z 9835 transakcji (27,3%)
# produkty z grupy owoce - 2450 z 9835 transakcji (24,9%)


# _____WNIOSKI list_pop_gr_products ----
list_pop_gr_products
# najczęsciej występujące grupy produktów to:
#  [1] "dairy produce"          "bread and backed goods" "non-alc. drinks"        "vegetables"             "fruit"                  "sausage"               
#  [7] "beer"                   "cheese"                 "frozen foods"           "chocolate" 

# SPRAWDZENIE (obliczenie apriori dla częstych grup produktów)
list_pop_gr_products_spr  <- apriori(data = Groceries_lvl2, ## object of class transactions
                                     parameter = list(support = 0.003, confidence = 0.2, # zmienione na 1 
                                                      target = 'frequent itemsets'))
summary(list_pop_gr_products_spr)

inspect(sort(subset(list_pop_gr_products_spr, subset = count > 984)))
#      items                                    support   count
# [1]  {dairy produce}                          0.4430097 4357 
# [2]  {bread and backed goods}                 0.3455008 3398 
# [3]  {non-alc. drinks}                        0.3179461 3127 
# [4]  {vegetables}                             0.2730046 2685 
# [5]  {fruit}                                  0.2491103 2450 
# [6]  {sausage}                                0.1891205 1860 
# [7]  {bread and backed goods,dairy produce}   0.1876970 1846 
# [8]  {dairy produce,vegetables}               0.1704118 1676 
# [9]  {dairy produce,fruit}                    0.1563803 1538 
# [10] {beer}                                   0.1555669 1530 
# [11] {dairy produce,non-alc. drinks}          0.1519065 1494 
# [12] {cheese}                                 0.1266904 1246 
# [13] {bread and backed goods,non-alc. drinks} 0.1242501 1222 
# [14] {frozen foods}                           0.1169293 1150 
# [15] {bread and backed goods,vegetables}      0.1162176 1143 
# [16] {chocolate}                              0.1087951 1070 
# [17] {bread and backed goods,fruit}           0.1075750 1058 
# [18] {dairy produce,sausage}                  0.1073716 1056 
# [19] {fruit,vegetables}                       0.1070666 1053 
# [20] {bread and backed goods,sausage}         0.1036096 1019 
# Potwierdza to wcześniejsze wyniki - otrzymano tą samą listę najczęstszych grup produktowych.


# jakie produkty są w których grupach produktów, mających największe wsparcie (>30% transakcji, tzn. dla trzech pierwszych)
# UWAGA potwierdzenie obserwacji, iż wsprcie częstej grupu 'budują' dwa-trzy produkty (linie poziome = średnia dla grupy)
## 1 ## dairy produce`
(items_lvl2_1 <- list_lvl2$`dairy produce`)
# [1] "whole milk"         "butter"             "curd"               "dessert"            "butter milk"        "yogurt"            
# [7] "whipped/sour cream" "beverages" 
length(items_lvl2_1) ## 8
(gr_prod_1 <- sort(tab_freq[items_lvl2_1], decreasing = TRUE))
# whole milk             yogurt whipped/sour cream             butter               curd            dessert        butter milk          beverages 
# 0.25551601         0.13950178         0.07168277         0.05541434         0.05327911         0.03711235         0.02796136         0.02602949 

plot(gr_prod_1, type = 'h', lwd = 5, xaxt = 'n', col = 'blue', las = 1,
     ylab = 'relative support', xlab = '', main = 'Relative support for products from `dairy produce` group')
axis(side = 1, at = seq(1, length(items_lvl2_1)), labels = names(gr_prod_1), las = 2)
abline(h = mean(gr_prod_1), col = 'tomato', lwd = 2)
grid()

## 2 ## bread and backed goods
(items_lvl2_2 <- list_lvl2$`bread and backed goods`)
# [1] "rolls/buns"          "white bread"         "brown bread"         "pastry"              "roll products "      "semi-finished bread" "zwieback"
length(items_lvl2_2) ## 7
(gr_prod_2 <- sort(tab_freq[items_lvl2_2], decreasing = TRUE))
#  rolls/buns              pastry         brown bread         white bread semi-finished bread      roll products             zwieback 
# 0.183934926         0.088967972         0.064870361         0.042094560         0.017691917         0.010269446         0.006914082 

plot(gr_prod_2, type = 'h', lwd = 5, 	xaxt = 'n', col = 'darkred', las = 1,
     ylab = 'relative support', xlab = '', main = 'Relative support for products from `bread and backed goods` group')
axis(side = 1, at = seq(1, length(items_lvl2_2)), labels = names(gr_prod_2), las = 2)
abline(h = mean(gr_prod_2), col = 'lawngreen', lwd = 2)
grid()

## 3 ## non-alc. drinks
(items_lvl2_3 <- list_lvl2$`non-alc. drinks`)
# "bottled water"         "soda"                  "misc. beverages"       "fruit/vegetable juice" "syrup" 
length(items_lvl2_3) ## 5
(gr_prod_3 <- sort(tab_freq[items_lvl2_3], decreasing = TRUE))
#        soda         bottled water fruit/vegetable juice       misc. beverages                 syrup 
# 0.174377224           0.110523640           0.072292832           0.028368073           0.003253686

plot(gr_prod_3, type = 'h', lwd = 5, 	xaxt = 'n', col = 'darkgreen', las = 1,
     ylab = 'relative support', xlab = '', main = 'Relative support for products from `non-alc. drinks` group')
axis(side = 1, at = seq(1, length(items_lvl2_3)), labels = names(gr_prod_3), las = 2)
abline(h = mean(gr_prod_3), col = 'darkorange', lwd = 2)
grid()



# EKSPERYMENT - REGUŁY ----------------------------------------------------
# Przeprowadzono trzy wersje eksperymentu.
# Każda z nich prowadzona była osobno dla pojedynczych produktów i dla grup produktów (level2) 
# - oznaczenia A i B.
#   1.podejście   support = 0.003, confidence = 0.2
#   2.podejście   support = 0.03,  confidence = 0.2
#   3.podejście   support = 0.003, confidence = 0.5

# _1A model apriori "produkty" - 1. podejście ----
# budujemy model - reguły
rules_groc <- apriori(data = Groceries, ## object of class transactions
                      parameter = list(support = 0.003, confidence = 0.2))

# przyjęto minSup = 0.003, to oznacza że produkt musi mieć wsparcie bezwzględne powyżej 29 transakcji
# przyjęto minConf = 0.2 co oznacza, iż jeżeli w lhs są wskazane produkty to 20% rhs będzie zgodne z regułą

summary(rules_groc)
# set of 2246 rules
# 
# rule length distribution (lhs + rhs):sizes
#    1    2    3    4    5 
#    1  368 1514  353   10 
# 
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       1       3       3       3       3       5 
# 
# summary of quality measures:
#   support        confidence        lift          count     
# Min.   :0.003   Min.   :0.20   Min.   : 0.8   Min.   :  30  
# 1st Qu.:0.004   1st Qu.:0.26   1st Qu.: 1.7   1st Qu.:  35  
# Median :0.004   Median :0.34   Median : 2.1   Median :  43  
# Mean   :0.006   Mean   :0.37   Mean   : 2.3   Mean   :  62  
# 3rd Qu.:0.006   3rd Qu.:0.46   3rd Qu.: 2.7   3rd Qu.:  61  
# Max.   :0.256   Max.   :0.89   Max.   :11.4   Max.   :2513  
# 
# mining info:
#      data ntransactions support confidence
# Groceries          9835   0.003        0.2


size(rules_groc)
barplot(table(size(rules_groc)), col = 'gold', las = 1,
        main = 'Length of the associate rules = LHS+RHS (products)')

# PODSUMOWANIE 
# Znalezieno 2246 reguł asocjacyjnych dla żądanych parametrow przeszukiwania
# tj. support = 0.003, confidence = 0.2
# większośc reguł - 1514 z 2246 (67,4%) - ma długość (lhs+rhs) równa 3 (3 elementy)

inspect(sort(rules_groc, by = 'lift')[1:10])
#      lhs                                                             rhs                  support confidence lift count
# [1]  {Instant food products}                                      => {hamburger meat}     0.0031  0.38       11.4 30   
# [2]  {flour}                                                      => {sugar}              0.0050  0.29        8.5 49   
# [3]  {processed cheese}                                           => {white bread}        0.0042  0.25        6.0 41   
# [4]  {citrus fruit,tropical fruit,other vegetables,whole milk}    => {root vegetables}    0.0032  0.63        5.8 31   
# [5]  {tropical fruit,root vegetables,other vegetables,whole milk} => {citrus fruit}       0.0032  0.45        5.4 31   
# [6]  {liquor}                                                     => {bottled beer}       0.0047  0.42        5.2 46   
# [7]  {citrus fruit,root vegetables,other vegetables,whole milk}   => {tropical fruit}     0.0032  0.54        5.2 31   
# [8]  {berries,whole milk}                                         => {whipped/sour cream} 0.0043  0.36        5.1 42   
# [9]  {herbs,whole milk}                                           => {root vegetables}    0.0042  0.54        4.9 41   
# [10] {tropical fruit,whole milk,yogurt}                           => {curd}               0.0040  0.26        4.9 39  

# Reguły o wartości lift = x oznacza, że elementy z LHS i RHS występują ze sobą x razy częściej w porównaniu do zakupów
# gdzie zakłada się, że nie mają ze sobą związku; tzn.
# produkty spożywcze typu instant i mięso na hamburgery są kupowae 11 razy częściej razem niż osobno, 
# serki topione i białe pieczywo kupowane są razem 8,5 raza częściej łącznie niż osobno, itd.

inspect(sort(rules_groc, by = 'confidence')[1:10])
#      lhs                                                         rhs                support     confidence lift     count
# [1]  {citrus fruit,tropical fruit,root vegetables,whole milk} => {other vegetables} 0.003152008 0.8857143  4.577509 31   
# [2]  {root vegetables,butter,yogurt}                          => {whole milk}       0.003050330 0.7894737  3.089723 30   
# [3]  {citrus fruit,tropical fruit,root vegetables}            => {other vegetables} 0.004473818 0.7857143  4.060694 44   
# [4]  {root vegetables,other vegetables,brown bread}           => {whole milk}       0.003152008 0.7750000  3.033078 31   
# [5]  {onions,butter}                                          => {whole milk}       0.003050330 0.7500000  2.935237 30   
# [6]  {tropical fruit,curd,yogurt}                             => {whole milk}       0.003965430 0.7500000  2.935237 39   
# [7]  {curd,domestic eggs}                                     => {whole milk}       0.004778851 0.7343750  2.874086 47   
# [8]  {tropical fruit,butter,yogurt}                           => {whole milk}       0.003355363 0.7333333  2.870009 33   
# [9]  {tropical fruit,root vegetables,whipped/sour cream}      => {other vegetables} 0.003355363 0.7333333  3.789981 33   
# [10] {butter,curd}                                            => {whole milk}       0.004880529 0.7164179  2.803808 48  

# Reguły o wartości pewności = x oznaczają, że za każdym razem, gdy zakupiono produkty z LHS, produkty z RHS  
# były kupowane w x przypadków. (conf(X->Y) można intepretować jak prawdopodobieństwo warunkowe P(Y|X)) tzn.
# gdy zakupiono owoce cytrusowe, owoce tropikalne, warzywa korzeniowe, mleko pełne to w 88% przypadków zakupiono inne warzywa,
# równiez inne warzywa;
# gdy zakupiono warzywa korzeniowe, masło i jogurt to w 79% przypadkach zakupiono również mleko pełne;
# gdy zakupiono owoce cytrusowe, owoce tropikalne, warzywa korzeniowe to w 79% przypadków
# zakupiono również inne warzywa.

inspect(sort(rules_groc, by = 'support')[1:10])
#      lhs                   rhs                support    confidence lift     count
# [1]  {}                 => {whole milk}       0.25551601 0.2555160  1.000000 2513 
# [2]  {other vegetables} => {whole milk}       0.07483477 0.3867578  1.513634  736 
# [3]  {whole milk}       => {other vegetables} 0.07483477 0.2928770  1.513634  736 
# [4]  {rolls/buns}       => {whole milk}       0.05663447 0.3079049  1.205032  557 
# [5]  {whole milk}       => {rolls/buns}       0.05663447 0.2216474  1.205032  557 
# [6]  {yogurt}           => {whole milk}       0.05602440 0.4016035  1.571735  551 
# [7]  {whole milk}       => {yogurt}           0.05602440 0.2192598  1.571735  551 
# [8]  {root vegetables}  => {whole milk}       0.04890696 0.4486940  1.756031  481 
# [9]  {root vegetables}  => {other vegetables} 0.04738180 0.4347015  2.246605  466 
# [10] {other vegetables} => {root vegetables}  0.04738180 0.2448765  2.246605  466 

# Reguła o wartości wsparcia względnego = x oznacza, że x transakcji zawiera wymienione w LHS i RHS produkty.
# Reguła o wsparciu względnym 0,25 oznacza, iż w 25% transakcji wystęþuje mleko pełne (count = 2513)
# Reguła pełne mleko <=> inne warzywa o wsparciu względnym 0,075 oznacza, iż pełne mleko i inne warzywa występują razem
# w 7,5% transakcji (w tym wypadku nie ma znaczenia co jest następnikiem, a co poprzednikiem)
# reguła bułki i bułeczki <=> jogurt o wsparciu względnym 0,057 oznacza, iż bułki i bułeczki oraz jogurt występują razem
# w 7,5% transakcji (w tym wypadku nie ma znaczenia co jest następnikiem, a co poprzednikiem)


# parametry określające 'jakość' otrzymanych reguł (przegląd)
quality(rules_groc)
summary(quality(rules_groc))
#    support           confidence          lift             count       
# Min.   :0.003050   Min.   :0.2000   Min.   : 0.8076   Min.   :  30.0  
# 1st Qu.:0.003559   1st Qu.:0.2587   1st Qu.: 1.7277   1st Qu.:  35.0  
# Median :0.004372   Median :0.3400   Median : 2.1491   Median :  43.0  
# Mean   :0.006284   Mean   :0.3669   Mean   : 2.2506   Mean   :  61.8  
# 3rd Qu.:0.006202   3rd Qu.:0.4588   3rd Qu.: 2.6622   3rd Qu.:  61.0  
# Max.   :0.255516   Max.   :0.8857   Max.   :11.4214   Max.   :2513.0 

boxplot(quality(rules_groc)[1:3], horizontal = TRUE, las = 1, #outline = FALSE,
        col = c('mediumseagreen', 'skyblue4', 'orangered'), 
        main = 'Boxplot of quality measures')

boxplot(quality(rules_groc)[1], horizontal = TRUE, las = 1, outline = FALSE,
        col = 'mediumseagreen', main = 'Boxplot of quality measures - support')
boxplot(quality(rules_groc)[2], horizontal = TRUE, las = 1, #outline = FALSE,
        col = 'skyblue4', main = 'Boxplot of quality measures - confidence')
boxplot(quality(rules_groc)[3], horizontal = TRUE, las = 1, #outline = FALSE,
        col = 'orangered', main = 'Boxplot of quality measures - lift ')

# PODSUMOWANIE - wartości miar jakości reguł
#1# wsparcie względne max wynosi 25,5% (wsparcie bezwzględne 2513), natomiast średnie wsparcie względne
#     wynosi 0,6% (wsparcie bezwzględne 61,8) - co prawie pokrywa się z kwantylem 3/4
#2# zaufanie max wynosi 88,5%, zaufanie min = 20%, natomiast zaufanie średnie wynosi 36,7% (w przybliżeniu można
#     powiedziec że w 1 na 3 przypadki po zakupieniu produktów LHS kupowane sa RHS)
#3# lift max wynosi 11,4, co jest jednak outlierem - z wykresu widać, iż zaczynają się one powyżej wartości 4; 
#     wartością średnią jest 2,25 (powyżej 4 zaczynają się outliery)


# WYKRESY
# wykresy dla otrzymanych reguł asocjacyjnych
plot(rules_groc) #, measure = 'support', shading = 'lift', interactive = TRUE)

# wizualizacja reguł - uwzględniająca ich długość (dłuższe mają mniejsze wsparcie, ale większą pewność?)
plot(rules_groc, shading = 'order', interactive = TRUE, jitter = 0,
     main = 'Two-key plot: order = lenght of the rule')
# UWAGA interaktywny
plot(rules_groc, method = 'grouped')


# ANALIZA długości reguł
size(rules_groc) ## długość reguł
table(size(rules_groc))
# 1    2    3    4    5 
# 1  368 1514  353   10 
barplot(table(size(rules_groc)), col = 'gold', main = 'Length of the associate rules = LHS+RHS')

# WNIOSKI 67,4% (1514/2246) reguł ma długość (LHS+RHS) wynoszącą 3 (reguła składa się z trzech elementów = produktów)
# natomiast 17% i 15,7% ma długość  odpowiednio - 2 i 4; występuje jedna reguła, gdzie LHS to zbiór pusty


# wykresy dla reguł
plot(rules_groc[size(rules_groc) == 5], method = 'matrix')
plot(rules_groc[size(rules_groc) == 4], method = 'matrix')

plot(rules_groc[size(rules_groc) == 5], method = 'graph')
plot(rules_groc[size(rules_groc) == 4], method = 'graph') #nieczytelne

plot(rules_groc[size(rules_groc) == 5], method = 'paracoord')
plot(rules_groc[size(rules_groc) == 4], method = 'paracoord') #nieczytelne

plot(rules_groc[size(rules_groc) == 5], method = 'grouped')
plot(rules_groc[size(rules_groc) == 4], method = 'grouped')

# ANALIZA lhs i rhs
inspect(rules_groc@rhs)
size(rules_groc@rhs)
summary(size(rules_groc@rhs))
# wszystkie poprzedniki mają długość 1

inspect(rules_groc@lhs)
size(rules_groc@lhs)
summary(size(rules_groc@lhs))
table(size(rules_groc@lhs))
barplot(table(size(rules_groc@lhs)), col = 'gold', main = 'Length of the associate rules = LHS+RHS')
# 67% (1514 z 2246) reguł ma następnik o długości 2, zbiory następników o długości 1 i 3 to 16% reguł każdy

# _1B model apriori grupy produktów - 1.podejście ----
# budujemy model - reguły
rules_groc_lvl2 <- apriori(data = Groceries_lvl2, ## object of class transactions
                           parameter = list(support = 0.003, confidence = 0.2, minlen = 2))

summary(rules_groc_lvl2)
# set of 8411 rules
# 
# rule length distribution (lhs + rhs):sizes
#    2    3    4    5    6    7 
#  306 2303 3548 1905  342    7 
# 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  2.000   3.000   4.000   3.964   5.000   7.000 
# 
# summary of quality measures:
#   support           confidence          lift            count        
# Min.   :0.003050   Min.   :0.2000   Min.   :0.6371   Min.   :  30.00  
# 1st Qu.:0.003559   1st Qu.:0.3690   1st Qu.:1.6619   1st Qu.:  35.00  
# Median :0.004575   Median :0.5079   Median :1.9328   Median :  45.00  
# Mean   :0.007222   Mean   :0.5098   Mean   :2.0486   Mean   :  71.03  
# 3rd Qu.:0.007016   3rd Qu.:0.6327   3rd Qu.:2.3271   3rd Qu.:  69.00  
# Max.   :0.187697   Max.   :1.0000   Max.   :9.1590   Max.   :1846.00  
# 
# mining info:
#           data ntransactions support confidence
# Groceries_lvl2          9835   0.003        0.2

# PODSUMOWANIE 
# Znalezieno 8411 reguł asocjacyjnych dla żądanych parametrów przeszukiwania
# tj. support = 0.003, confidence = 0.2 oraz minlen=2
# 42% reguł - 3548 z 8411 - ma długość (lhs+rhs) równą 4 (4 elementy)
# 27% i 22% reguł - ma długość - odpowiednio 3 i 5 elementów

size(rules_groc_lvl2)
barplot(table(size(rules_groc_lvl2)), col = 'gold', las = 1,
        main = 'Length of the associate rules = LHS+RHS (products group)')

inspect(sort(rules_groc_lvl2, by = 'lift')[1:10])
#      lhs                                          rhs            support     confidence lift     count
# [1]  {dairy produce,sweetener,vinegar/oils}    => {staple foods} 0.003253686 0.4637681  9.158955 32   
# [2]  {sweetener,vinegar/oils}                  => {staple foods} 0.003660397 0.4186047  8.267022 36   
# [3]  {dairy produce,staple foods,vinegar/oils} => {sweetener}    0.003253686 0.3076923  8.026933 32   
# [4]  {staple foods,vinegar/oils}               => {sweetener}    0.003660397 0.2880000  7.513210 36   
# [5]  {dairy produce,fruit,sweetener}           => {staple foods} 0.003965430 0.3451327  6.816025 39   
# [6]  {dairy produce,non-alc. drinks,sweetener} => {staple foods} 0.003558719 0.3181818  6.283771 35   
# [7]  {dairy produce,sweetener,vegetables}      => {staple foods} 0.003965430 0.3145161  6.211378 39   
# [8]  {dairy produce,fruit,staple foods}        => {sweetener}    0.003965430 0.2363636  6.166144 39   
# [9]  {dairy produce,sweetener}                 => {staple foods} 0.007727504 0.3003953  5.932505 76   
# [10] {fruit,sweetener}                         => {staple foods} 0.004270463 0.2957746  5.841252 42 

# Reguły o wartości lift = x oznacza, że elementy z LHS i RHS występują ze sobą x razy częściej w porównaniu do zakupów
# gdzie zakłada się, że nie mają ze sobą zwiazku; tzn.
# produkty z grup: produkty mleczne, słodycze,ocet/olej wraz z podstawowymi produktami (mąka, ryz, kasze) są 9,2 raza
# częściej kupowane razem niż osobno (dotyczy LHS i RHS)
# słodycze oraz ocet/olej są kupowane razem z podstawowymi produktami (mąka, ryz, kasze) 8,3 raza częściej niż osobno
# zasada [3] jest bardzo podobna do [1] - ale lift = 8,0
# zasada [4] jest bardzo podobno do [2] - ale lift = 7,5

inspect(sort(rules_groc_lvl2, by = 'confidence')[1:10])
#      lhs                                                                rhs             support     confidence lift     count
# [1]  {chocolate,fruit,staple foods}                                  => {dairy produce} 0.003558719 1.0000000  2.257287 35   
# [2]  {eggs,fruit,staple foods}                                       => {dairy produce} 0.003660397 0.9729730  2.196279 36   
# [3]  {cheese,eggs,perfumery}                                         => {dairy produce} 0.003660397 0.9729730  2.196279 36   
# [4]  {eggs,fruit,non-alc. drinks,vinegar/oils}                       => {dairy produce} 0.003457041 0.9714286  2.192793 34   
# [5]  {frozen foods,fruit,non-alc. drinks,staple foods}               => {dairy produce} 0.003152008 0.9687500  2.186747 31   
# [6]  {frozen foods,fruit,non-alc. drinks,vinegar/oils}               => {dairy produce} 0.003050330 0.9677419  2.184471 30   
# [7]  {bread and backed goods,cheese,fruit,non-alc. drinks,perfumery} => {dairy produce} 0.003050330 0.9677419  2.184471 30   
# [8]  {bread and backed goods,fruit,pork,sausage}                     => {dairy produce} 0.004880529 0.9600000  2.166996 48   
# [9]  {bread and backed goods,cheese,non-alc. drinks,perfumery}       => {dairy produce} 0.004270463 0.9545455  2.154683 42   
# [10] {bread and backed goods,eggs,fruit,vinegar/oils}                => {dairy produce} 0.003863752 0.9500000  2.144423 38 

# Reguły o wartości pewności = x oznacza, że za każdym razem, gdy zakupiono produkty z LHS, produkty z RHS były kupowane x przypadków 
# (conf(X->Y) można intepretować jak prawdopodobieństwo warunkowe P(Y|X)) tzn.
# jeżeli zakupiono produkty z grup czekolada, owoce, podstawowe produkty (mąka, ryz, kasze) to na 100% zakupiono produkt
# z działu produkty mleczne
# UWAGA ponieważ produkty mleczne występują w 44% transakcji - tworzą reguły o dużej pewności, gdy są w następniku
# wynika to z definicji conf(X->Y) = sup(X->Y) / sup(X)  (produkty mleczne 'budują' wartość sup(X->Y))

# sprawdzenie jak wyglądaja reguły, gdy poprzednikiem są produkty mleczne
inspect(sort(subset(rules_groc_lvl2, subset = lhs %in% 'dairy produce' & confidence > 0.80), by = 'confidence'))
#      lhs                                                             rhs                      support     confidence lift     count
# [1]  {dairy produce,frozen foods,fruit,poultry}                   => {vegetables}             0.004168785 0.8367347  3.064911 41   
# [2]  {chocolate,dairy produce,non-alc. drinks,sausage,vegetables} => {bread and backed goods} 0.003558719 0.8333333  2.411958 35   
# [3]  {cheese,dairy produce,fruit,staple foods}                    => {vegetables}             0.003863752 0.8260870  3.025909 38   
# [4]  {dairy produce,non-alc. drinks,poultry,sausage}              => {bread and backed goods} 0.004067107 0.8163265  2.362734 40   
# [5]  {dairy produce,poultry,staple foods}                         => {vegetables}             0.003965430 0.8125000  2.976141 39   
# [6]  {chocolate,dairy produce,fruit,non-alc. drinks,sausage}      => {bread and backed goods} 0.004372140 0.8113208  2.348246 43   
# [7]  {beef,dairy produce,fruit,pork}                              => {vegetables}             0.003457041 0.8095238  2.965239 34   
# [8]  {dairy produce,fruit,non-alc. drinks,sausage,vinegar/oils}   => {bread and backed goods} 0.003863752 0.8085106  2.340112 38   
# [9]  {chocolate,dairy produce,long-life bakery products,sausage}  => {bread and backed goods} 0.004270463 0.8076923  2.337744 42   
# [10] {chocolate,dairy produce,non-alc. drinks,sausage}            => {bread and backed goods} 0.007015760 0.8023256  2.322211 69 
# należy zauwazyć, iż reguły zawierajace w poprzedniku produkty mleczne mają równiez stosunkowo duża pewność, a następnikiem
# są grupy produktów z największym wsparciem, tj. pieczywo i wypieki oraz warzywa (bardzo duże sup(X->Y))

inspect(sort(subset(rules_groc_lvl2, subset = lhs %in% 'bread and backed goods' & confidence > 0.80), by = 'confidence'))
# CIEKAWOSTKA jeżeli w poprzedniku występuje produkt z grupy pieczywo i wypieki to z pewnością > 0,80 w następniku wystąpi 
# produkt z działu 'produkty mleczne' (takich reguł jest 258); z powyższego zestawieni wynika, iż jeżeli w poprzedniku wystąpiła 
# grupa produkty mleczne to z pewnością > 0,80 wystąpia podukty z grupy pieczywo i wypieki lub warzywa.

inspect(sort(rules_groc_lvl2, by = 'support')[1:10])
#      lhs                         rhs                      support   confidence lift     count
# [1]  {bread and backed goods} => {dairy produce}          0.1876970 0.5432607  1.226295 1846 
# [2]  {dairy produce}          => {bread and backed goods} 0.1876970 0.4236860  1.226295 1846 
# [3]  {vegetables}             => {dairy produce}          0.1704118 0.6242086  1.409018 1676 
# [4]  {dairy produce}          => {vegetables}             0.1704118 0.3846683  1.409018 1676 
# [5]  {fruit}                  => {dairy produce}          0.1563803 0.6277551  1.417024 1538 
# [6]  {dairy produce}          => {fruit}                  0.1563803 0.3529952  1.417024 1538 
# [7]  {non-alc. drinks}        => {dairy produce}          0.1519065 0.4777742  1.078474 1494 
# [8]  {dairy produce}          => {non-alc. drinks}        0.1519065 0.3428965  1.078474 1494 
# [9]  {non-alc. drinks}        => {bread and backed goods} 0.1242501 0.3907899  1.131083 1222 
# [10] {bread and backed goods} => {non-alc. drinks}        0.1242501 0.3596233  1.131083 1222

# Reguła o wartości wsparcia względnego = x oznacza, że x transakcji zawiera wymienione w LHS i RHS produkty.
# reguła o wsparciu względnym 0,18 oznacza, iż w 18% transakcji występują produkty z działu produkty mleczne
# oraz pieczywo i wypieki (tu co jest następnikiem a co poprzednikiem nie ma znaczenia)
# reguła o wsparciu względnym 0,17 oznacza, iż w 17% transakcji występują produkty z działu warzywa
# oraz produkty mleczne (tu co jest następnikiem a co poprzednikiem nie ma znaczenia)
# reguła o wsparciu względnym 0,156 oznacza, iż w 15,6% transakcji występują produkty z działu owoce
# oraz produkty mleczne (tu co jest następnikiem a co poprzednikiem nie ma znaczenia)
# reguła o wsparciu względnym 0,152 oznacza, iż w 15,2% transakcji występują produkty z napoje niealkoholowe
# oraz produkty mleczne (tu co jest następnikiem a co poprzednikiem nie ma znaczenia)
# reguła o wsparciu względnym 0,124 oznacza, iż w 12,4% transakcji występują produkty z napoje niealkoholowe
# oraz pieczywo i wypieki (tu co jest następnikiem a co poprzednikiem nie ma znaczenia)

# Powyższe reguły sa zbudowane z grup produktów o największym wsparciu (częste produkty dają częste reguły), ale
# aby znaleźć odpowiedź na pytanie, które działy sklepu należy rozwijać, należy przeanalizowac jeszcze zaufanie ww. reguł.
# tzn. zakup których grup produktów warunkuje zakup innych grup.


# parametry określające 'jakość' otrzymanych reguł (przegląd)
quality(rules_groc_lvl2)
summary(quality(rules_groc_lvl2))
#    support           confidence          lift            count        
# Min.   :0.003050   Min.   :0.2000   Min.   :0.6371   Min.   :  30.00  
# 1st Qu.:0.003559   1st Qu.:0.3690   1st Qu.:1.6619   1st Qu.:  35.00  
# Median :0.004575   Median :0.5079   Median :1.9328   Median :  45.00  
# Mean   :0.007222   Mean   :0.5098   Mean   :2.0486   Mean   :  71.03  
# 3rd Qu.:0.007016   3rd Qu.:0.6327   3rd Qu.:2.3271   3rd Qu.:  69.00  
# Max.   :0.187697   Max.   :1.0000   Max.   :9.1590   Max.   :1846.00  

boxplot(quality(rules_groc_lvl2)[1:3], horizontal = TRUE, las = 1, #outline = FALSE,
        col = c('mediumseagreen', 'skyblue4', 'orangered'), 
        main = 'Boxplot of quality measures')

boxplot(quality(rules_groc_lvl2)[1], horizontal = TRUE, las = 1, outline = FALSE,
        col = 'mediumseagreen', main = 'Boxplot of quality measures - support')
boxplot(quality(rules_groc_lvl2)[2], horizontal = TRUE, las = 1, #outline = FALSE,
        col = 'skyblue4', main = 'Boxplot of quality measures - confidence')
boxplot(quality(rules_groc_lvl2)[3], horizontal = TRUE, las = 1, #outline = FALSE,
        col = 'orangered', main = 'Boxplot of quality measures - lift ')
boxplot(quality(rules_groc_lvl2)[4], horizontal = TRUE, las = 1, outline = FALSE,
        col = 'yellow', main = 'Boxplot of quality measures - count')

# PODSUMOWANIE - wartości miar jakości reguł
#1# wsparcie względne max wynosi 18,8% (Wsparcie bezwzględne 1846), natomiast średnie wsparcie względne
#   wynosi 0,7% (wsparcie bezwzględne 71,0) - co prawie pokrywa się z kwantylem 3/4, ale jest ciut wyższe
#   z wykresu widać, iż dla wsparcia bezwzględnego powyżej 120 występują ouliery 
#2# zaufanie max wynosi 100%, zaufanie min = 20%, natomiast zaufanie średnie wynosi 51% (w przybliżeniu można
#   powiedziec że w 50% przypadków po zakupieniu produktów LHS kupowane są RHS)
#3# lift max wynosi 9,16, co jest jednak outlierem - z wykresu widać, iż zaczynają się one powyżej wartości 3,5; 
#   wartością średnią jest 2,05

# WYKRES
plot(rules_groc_lvl2[size(rules_groc_lvl2) == 6], method = 'grouped')


# _2A model apriori produkty - 2.podejście ----
# parametry z pierwszego podejścia (do porównania)
rules_groc@info
# $data
# Groceries
# $ntransactions
# [1] 9835
# $support
# [1] 0.003
# $confidence
# [1] 0.2

# PONIEWAŻ zależy nam na silnych regułach opartych na częstych produktach zwiększamy parametr support;
# potrzebne są też reguły złożone z więcej niż jednego elementu (minlen) (tzn. żeby reguła nie
# zaczynała się od zbioru pustego)
# definujemy parametry - drugie podejście
ap_param <- new('APparameter', 'confidence' = 0.2, 'support' = 0.03, 'minlen' = 2, maxtime = 20) 

# przyjęto minSup = 0.03, to oznacza że produkt musi mieć wsparcie bezwzględne powyżej 295 transakcji
# przyjęto minConf = 0.2 co oznacza, iż jeżeli w lhs są wskazane produkty to 20% rhs będzie zgodne z regułą

print(ap_param)
# confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target   ext
#        0.2    0.1    1 none FALSE            TRUE      20    0.03      2     10  rules FALSE

# budujemy model - reguły
rules_groc_new <- apriori(Groceries, ap_param)

summary(rules_groc_new)
# set of 25 rules
# 
# rule length distribution (lhs + rhs):sizes
#   2 
#   25 
# 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#      2       2       2       2       2       2 
# 
# summary of quality measures:
#      support          confidence          lift            count      
# Min.   :0.03010   Min.   :0.2084   Min.   :0.8991   Min.   :296.0  
# 1st Qu.:0.03437   1st Qu.:0.2297   1st Qu.:1.2050   1st Qu.:338.0  
# Median :0.04260   Median :0.3109   Median :1.5136   Median :419.0  
# Mean   :0.04444   Mean   :0.3129   Mean   :1.5050   Mean   :437.1  
# 3rd Qu.:0.04891   3rd Qu.:0.3868   3rd Qu.:1.6085   3rd Qu.:481.0  
# Max.   :0.07483   Max.   :0.4496   Max.   :2.2466   Max.   :736.0  
# 
# mining info:
#      data ntransactions support confidence
# Groceries          9835    0.03        0.2

# PODSUMOWANIE 
# Znalezieno 25 reguł asocjacyjnych dla żądanych parametrów przeszukiwania
# tj. support = 0.03, confidence = 0.2, minlen=2
# wszytkie reguły mają długośc 2 (2 elementy w lhs+rhs)

size(rules_groc_new)
barplot(table(size(rules_groc_new)), col = 'gold', las = 1,
        main = 'Length of the associate rules = LHS+RHS (products)')

inspect(sort(rules_groc_new, by = 'lift')[1:10])
#      lhs                     rhs                support    confidence lift     count
# [1]  {root vegetables}    => {other vegetables} 0.04738180 0.4347015  2.246605 466  
# [2]  {other vegetables}   => {root vegetables}  0.04738180 0.2448765  2.246605 466  
# [3]  {sausage}            => {rolls/buns}       0.03060498 0.3257576  1.771048 301  
# [4]  {tropical fruit}     => {other vegetables} 0.03589222 0.3420543  1.767790 353  
# [5]  {whipped/sour cream} => {whole milk}       0.03223183 0.4496454  1.759754 317  
# [6]  {root vegetables}    => {whole milk}       0.04890696 0.4486940  1.756031 481  
# [7]  {yogurt}             => {other vegetables} 0.04341637 0.3112245  1.608457 427  
# [8]  {other vegetables}   => {yogurt}           0.04341637 0.2243826  1.608457 427  
# [9]  {tropical fruit}     => {whole milk}       0.04229792 0.4031008  1.577595 416  
# [10] {yogurt}             => {whole milk}       0.05602440 0.4016035  1.571735 551  

# Reguły o wartości lift = x oznacza, że elementy z LHS i RHS występują ze sobą x razy częściej w porównaniu do zakupów
# gdzie zakłada się, że nie mają ze sobą zwiazku; tzn.
# warzywa korzeniowe oraz inne warzywa są kupowane 2,2 razy częściej razem niż osobno, 
# (reguła ta niezależnie od kolejności ma takie samo wsparcie i lift, ale zależenie od kolejności - inne zaufanie różniące się dwukrotnie)
# kiełbasa oraz bułki i bułeczki kupowane są 1,77 razy częściej razem niż osobno, 
# owoce tropikalne i inne warzywa kupowane są 1,77 razy częściej razem niż osobno, itd. 
# Zwraca uwagę fakt, iż w zasadzie reguły te są zbudowane tylko z produktów częstych (z wyjątekim [3] gdzie lhs = {sausage})
# W porównaniu reguł w 1. podejściu - zwraca uwagę spadek wartości lift dla reguł 
# (poprzedniu było powyżej 4,9 - do 11 - teraz jest max 2,2)

inspect(sort(rules_groc_new, by = 'confidence')[1:10])
#      lhs                     rhs                support    confidence lift     count
# [1]  {whipped/sour cream} => {whole milk}       0.03223183 0.4496454  1.759754 317  
# [2]  {root vegetables}    => {whole milk}       0.04890696 0.4486940  1.756031 481  
# [3]  {root vegetables}    => {other vegetables} 0.04738180 0.4347015  2.246605 466  
# [4]  {tropical fruit}     => {whole milk}       0.04229792 0.4031008  1.577595 416  
# [5]  {yogurt}             => {whole milk}       0.05602440 0.4016035  1.571735 551  
# [6]  {pip fruit}          => {whole milk}       0.03009659 0.3978495  1.557043 296  
# [7]  {other vegetables}   => {whole milk}       0.07483477 0.3867578  1.513634 736  
# [8]  {pastry}             => {whole milk}       0.03324860 0.3737143  1.462587 327  
# [9]  {citrus fruit}       => {whole milk}       0.03050330 0.3685504  1.442377 300  
# [10] {tropical fruit}     => {other vegetables} 0.03589222 0.3420543  1.767790 353 

# Reguły o wartości pewności = x oznacza, że za każdym razem, gdy zakupiono produkty z LHS, produkty z RHS były również kupowane x przypadków 
# (conf(X->Y) można intepretować jak prawdopodobieństwo warunkowe P(Y|X)) tzn.
# gdy zakupiono bitą śmietanę i śmietanę zwykłą to w 45% przypadków zakupiono też mleko pełne
# gdy zakupiono warzywa korzeniowe to w 44,9% przypadkach zakupiono również mleko pełne
# gdy zakupiono warzywa korzeniowe to w 43,4% przypadkach zakupiono również inne warzywa
# W porównaniu reguł w 0. podejściu - zwraca uwagę spadek wartości zaufania reguł 
# (poprzednio było powyżej 70% teraz jest max 45%)

inspect(sort(rules_groc_new, by = 'support')[1:10])
#      lhs                   rhs                support    confidence lift     count
# [1]  {other vegetables} => {whole milk}       0.07483477 0.3867578  1.513634 736  
# [2]  {whole milk}       => {other vegetables} 0.07483477 0.2928770  1.513634 736  
# [3]  {rolls/buns}       => {whole milk}       0.05663447 0.3079049  1.205032 557  
# [4]  {whole milk}       => {rolls/buns}       0.05663447 0.2216474  1.205032 557  
# [5]  {yogurt}           => {whole milk}       0.05602440 0.4016035  1.571735 551  
# [6]  {whole milk}       => {yogurt}           0.05602440 0.2192598  1.571735 551  
# [7]  {root vegetables}  => {whole milk}       0.04890696 0.4486940  1.756031 481  
# [8]  {root vegetables}  => {other vegetables} 0.04738180 0.4347015  2.246605 466  
# [9]  {other vegetables} => {root vegetables}  0.04738180 0.2448765  2.246605 466  
# [10] {yogurt}           => {other vegetables} 0.04341637 0.3112245  1.608457 427 

# Reguła o wartości wsparcia względnego = x oznacza, że x transakcji zawiera wymienione w LHS i RHS produkty.
# reguła pełne mleko <=> inne warzywa o wsparciu względnym 0,075 oznacza, iż pełne mleko i inne warzywa występują razem
# w 7,5% transakcji (nie ma znaczenia co jest tu poprzednikiem, a co następnikiem)
# reguła bułki i bułeczki <=> jogurt o wsparciu względnym 0,057 oznacza, iż bułki i bułeczki oraz jogurt występują razem
# w 5,7% transakcji (nie ma znaczenia co jest tu poprzednikiem, a co następnikiem)
# Reguły powyższe - z wyjątkiem, że tu nie występuje reguła ze zbiorem pustym 
# - są takie same jak otrzymano w 1. podejściu 

# SPR dla reguł [1] i [2] - wsparcie 736 transakcje zawierają produkty inne warzywa oraz mleko pełne
inspect(subset(subset(Groceries, items %in% 'other vegetables'), items %in% 'whole milk'))
# SPR dla reguł [3] i [4] - wsparcie 557 transakcje zawierają produkty bułki i bułeczki oraz mleko pełne
inspect(subset(subset(Groceries, items %in% 'rolls/buns'), items %in% 'whole milk'))

size(rules_groc_new@lhs)
summary(size(rules_groc_new@lhs))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1       1       1       1       1       1 

size(rules_groc_new@rhs)
summary(size(rules_groc_new@rhs))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    1       1       1       1       1       1 
# UWAGA Wszytskie reguły mają długośc 2 (tzn. następnik i poprzednik są jednoelementowe)

# wykresy
plot(rules_groc_new, method = 'scatterplot') #, measure = c('support', 'confidence'), shading = 'lift')

plot(rules_groc_new, method = "matrix") 
# Itemsets in Antecedent (LHS)
# [1] "{root vegetables}"    "{sausage}"            "{whipped/sour cream}" "{tropical fruit}"     "{other vegetables}"  
# [6] "{pip fruit}"          "{yogurt}"             "{pastry}"             "{citrus fruit}"       "{whole milk}"        
# [11] "{bottled water}"      "{rolls/buns}"         "{soda}"              
# Itemsets in Consequent (RHS)
# [1] "{soda}"             "{rolls/buns}"       "{whole milk}"       "{yogurt}"           "{other vegetables}" "{root vegetables}" 

# WNIOSKI 
# Zwiększczenie wartości minimalnego wsparcia wzglednego (minrSup = 0.03) spowodowało
# skrócenie reguł - do dwóch elementów: jeden poprzednik i jeden nastepnik, 
# natomiast zasadniczo nie zmieniło zestawienia reguł wg parametru wsparcie bezwzględne;
# reguły przefitrowane wg parametru lift i zaufanie zawierają najczęstsze produkty - zmiana w stosunku
# do otrzymanych w 0.podejeściu zestawień wynikająca z wymaganego większego wsparcia poprzedznika reguły
# (dla zaufania conf = P(XY) / P(X) oraz dla lift = P(XY)/ [P(X) * P(Y)])

# _2B model apriori grupy produkty - 2.podejście ----
# budujemy model - reguły
rules_groc_lvl2_new <- apriori(Groceries_lvl2, ap_param)

summary(rules_groc_lvl2_new)
# set of 193 rules
# 
# rule length distribution (lhs + rhs):sizes
#   2  3  4 
#  86 83 24 
# 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  2.000   2.000   3.000   2.679   3.000   4.000 
# 
# summary of quality measures:
#   support          confidence          lift            count       
# Min.   :0.03010   Min.   :0.2017   Min.   :0.6669   Min.   : 296.0  
# 1st Qu.:0.03437   1st Qu.:0.3771   1st Qu.:1.2986   1st Qu.: 338.0  
# Median :0.04423   Median :0.4409   Median :1.5224   Median : 435.0  
# Mean   :0.05543   Mean   :0.4682   Mean   :1.5183   Mean   : 545.2  
# 3rd Qu.:0.06396   3rd Qu.:0.5478   3rd Qu.:1.7101   3rd Qu.: 629.0  
# Max.   :0.18770   Max.   :0.7984   Max.   :2.2873   Max.   :1846.0  
# 
# mining info:
#           data ntransactions support confidence
# Groceries_lvl2          9835    0.03        0.2

# PODSUMOWANIE 
# Znalezieno 193 reguł asocjacyjnych dla żądanych parametrów przeszukiwania
# tj. support = 0.03, confidence = 0.2, minnlen=2
# 44,5% reguł - 86 z 193 - ma długość (lhs+rhs) równą 2 (2 elementy)
# 43% reguł - 83 z 193 - ma długość (lhs+rhs) równą 3 (3 elementy)
# pozostałe reguły maja długość 4 (4 elementy)

summary(size(rules_groc_lvl2_new))
barplot(table(size(rules_groc_lvl2_new)), col = 'gold', 
        main = 'Length of the associate rules = LHS+RHS (products group)')

inspect(sort(rules_groc_lvl2_new, by = 'lift')[1:10])
#      lhs                                                       rhs          support    confidence lift     count
# [1]  {dairy produce,sausage}                                => {cheese}     0.03111337 0.2897727  2.287251 306  
# [2]  {bread and backed goods,dairy produce,vegetables}      => {sausage}    0.03284189 0.4007444  2.118990 323  
# [3]  {bread and backed goods,dairy produce,fruit}           => {sausage}    0.03060498 0.3960526  2.094182 301  
# [4]  {dairy produce,non-alc. drinks,vegetables}             => {fruit}      0.03304525 0.5126183  2.057796 325  
# [5]  {beef}                                                 => {vegetables} 0.04585663 0.5595533  2.049612 451  
# [6]  {dairy produce,fruit}                                  => {cheese}     0.03965430 0.2535761  2.001541 390  
# [7]  {bread and backed goods,dairy produce,vegetables}      => {fruit}      0.04077275 0.4975186  1.997182 401  
# [8]  {bread and backed goods,vegetables}                    => {sausage}    0.04382308 0.3770779  1.993850 431  
# [9]  {dairy produce,vinegar/oils}                           => {vegetables} 0.03141840 0.5355286  1.961610 309  
# [10] {bread and backed goods,dairy produce,non-alc. drinks} => {fruit}      0.03528216 0.4880450  1.959152 347  

# Reguły o wartości lift = x ozanczają, że elementy z LHS i RHS występują ze sobą x razy częściej w porównaniu do zakupów
# gdzie zakałda się, że nie mają ze sobą związku; tzn.
# produkty z grup: produkty mleczne i kiełabsy wraz z serem są 2,3 raza częściej kupowane razem niż osobno (dotyczy LHS i RHS)
# pieczywo i wypieki, produkty mleczne i warzywa razem z kiełbasą są 2,1 raza częściej kupowane niż osobno
# pieczywo i wypieki, produkty mleczne i owoce razem z kiełbasą są 2,1 raza częściej kupowane niż osobno
# Wartości lift są przeciętnie niższe niż w 1. podejściu max. 9,1 (outlier) i 2,3 na Q3 
# vs max. 2,3 w podejściu 2., róznice wartości nie są jednak tak znaczne jak w przypadku 
# pojedynczych produktów.

inspect(sort(rules_groc_lvl2_new, by = 'confidence')[1:10])
#      lhs                                               rhs             support    confidence lift     count
# [1]  {bread and backed goods,fruit,sausage}         => {dairy produce} 0.03060498 0.7984085  1.802237 301  
# [2]  {bread and backed goods,fruit,vegetables}      => {dairy produce} 0.04077275 0.7956349  1.795976 401  
# [3]  {cheese,fruit}                                 => {dairy produce} 0.03965430 0.7707510  1.739806 390  
# [4]  {cheese,vegetables}                            => {dairy produce} 0.04219624 0.7628676  1.722011 415  
# [5]  {fruit,non-alc. drinks,vegetables}             => {dairy produce} 0.03304525 0.7575758  1.710066 325  
# [6]  {bread and backed goods,sausage,vegetables}    => {dairy produce} 0.03284189 0.7494200  1.691656 323  
# [7]  {vegetables,vinegar/oils}                      => {dairy produce} 0.03141840 0.7481840  1.688866 309  
# [8]  {bread and backed goods,fruit,non-alc. drinks} => {dairy produce} 0.03528216 0.7430407  1.677256 347  
# [9]  {frozen foods,fruit}                           => {dairy produce} 0.03070666 0.7401961  1.670835 302  
# [10] {fruit,vegetables}                             => {dairy produce} 0.07869853 0.7350427  1.659203 774  

# Reguły o wartości pewności = x oznacza, że za każdym razem, gdy zakupiono produkty z LHS, produkty z RHS były kupowane x przypadków 
# conf(X->Y) można intepretować jak prawdopodobieństwo warunkowe P(Y|X)) tzn.
# jeżeli zakupiono produkty z grup pieczywo i wypieki, owoce i kiełbasy to w 80% transakcji zakupiono produkty mleczne
# jeżeli zakupiono produkty z grup pieczywo i wypieki, owoce i warzywa to w 80% transakcji zakupiono produkty mleczne
# jeżeli zakupiono produkty z grup sery i owoce to w 77% transakcji zakupiono produkty mleczne
# Zwraca uwagę fakt, iż reguły powstały z elementów częstych, w porównaniu do reguł z 1. podejścia = następnik jest taki sam
# (produkty mleczne) natomiast zmieniła się cześć poprzedników (wymagane wyższe wsparcie), ale reguły mają mniejsze zaufanie
# (było powyżej 95% jest teraz mniej niż 80%)
# UWAGA ponieważ produkty mleczne występują w 44% transakcji - tworzą reguły o dużej pewności, gdy są w następniku

inspect(sort(rules_groc_lvl2_new, by = 'support')[1:10])
#      lhs                         rhs                      support   confidence lift     count
# [1]  {bread and backed goods} => {dairy produce}          0.1876970 0.5432607  1.226295 1846 
# [2]  {dairy produce}          => {bread and backed goods} 0.1876970 0.4236860  1.226295 1846 
# [3]  {vegetables}             => {dairy produce}          0.1704118 0.6242086  1.409018 1676 
# [4]  {dairy produce}          => {vegetables}             0.1704118 0.3846683  1.409018 1676 
# [5]  {fruit}                  => {dairy produce}          0.1563803 0.6277551  1.417024 1538 
# [6]  {dairy produce}          => {fruit}                  0.1563803 0.3529952  1.417024 1538 
# [7]  {non-alc. drinks}        => {dairy produce}          0.1519065 0.4777742  1.078474 1494 
# [8]  {dairy produce}          => {non-alc. drinks}        0.1519065 0.3428965  1.078474 1494 
# [9]  {non-alc. drinks}        => {bread and backed goods} 0.1242501 0.3907899  1.131083 1222 
# [10] {bread and backed goods} => {non-alc. drinks}        0.1242501 0.3596233  1.131083 1222 

# Reguła o wartości wsparcia względnego = x oznacza, że x transakcji zawiera wymienione w LHS i RHS produkty.
# reguła o wsparciu względnym 0,18 oznacza, iż w 18% transakcji występują produkty z działu produkty mleczne
# oraz pieczywo i wypieki (tu co jest następnikiem a co poprzednikiem nie ma znaczenia), itd.
# Reguły powyższe - z wyjątkiem, że tu nie występuje reguła ze zbiorem pustym - są takie same jak otrzymano w 0. podejściu 


# _3A model apriori produkty - 3.podejście ----
# parametry - trzecie podejście
ap_param_3 <- new('APparameter', 'confidence' = 0.5, 'support' = 0.003, 'minlen' = 2, maxtime = 20) 

# przyjęto minSup = 0.003, to oznacza że produkt musi mieć wsparcie bezwzględne powyżej 29 transakcji
# przyjęto minConf = 0.5 co oznacza, iż jeżeli w lhs są wskazane produkty to 50% rhs będzie zgodne z regułą

print(ap_param_3)
# confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target   ext
#        0.5    0.1    1 none FALSE            TRUE      20   0.003      2     10  rules FALSE

# budujemy model - reguły
rules_groc_new_3 <- apriori(Groceries, ap_param_3)

summary(rules_groc_new_3)
# set of 421 rules
# 
# rule length distribution (lhs + rhs):sizes
#  2   3   4   5 
#  5 281 128   7 
# 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  2.000   3.000   3.000   3.325   4.000   5.000 
# 
# summary of quality measures:
#     support           confidence          lift           count       
# Min.   :0.003050   Min.   :0.5000   Min.   :1.957   Min.   : 30.00  
# 1st Qu.:0.003355   1st Qu.:0.5238   1st Qu.:2.135   1st Qu.: 33.00  
# Median :0.003965   Median :0.5556   Median :2.426   Median : 39.00  
# Mean   :0.004754   Mean   :0.5715   Mean   :2.522   Mean   : 46.75  
# 3rd Qu.:0.005186   3rd Qu.:0.6094   3rd Qu.:2.766   3rd Qu.: 51.00  
# Max.   :0.022267   Max.   :0.8857   Max.   :5.804   Max.   :219.00  
# 
# mining info:
#      data ntransactions support confidence
# Groceries          9835   0.003        0.5

# PODSUMOWANIE 
# Znalezieno 421 reguł asocjacyjnych dla żądanych parametrów przeszukiwania
# tj. support = 0.003, confidence = 0.5
# większośc reguł - 281 z 421 (66,7%) - ma długość (lhs+rhs) równa 3 (3 elementy)
# spora część reguł - 128 z 421 (30%) - ma długość (lhs+rhs) równa 4 (4 elementy)

size(rules_groc_new_3)
barplot(table(size(rules_groc_new_3)), col = 'gold', las = 1,
        main = 'Length of the associate rules = LHS+RHS (products)')

inspect(sort(rules_groc_new_3, by = 'lift')[1:10])
#      lhs                                                           rhs                support     confidence lift     count
# [1]  {citrus fruit,tropical fruit,other vegetables,whole milk}  => {root vegetables}  0.003152008 0.6326531  5.804238 31   
# [2]  {citrus fruit,root vegetables,other vegetables,whole milk} => {tropical fruit}   0.003152008 0.5438596  5.183004 31   
# [3]  {herbs,whole milk}                                         => {root vegetables}  0.004168785 0.5394737  4.949369 41   
# [4]  {herbs,other vegetables}                                   => {root vegetables}  0.003863752 0.5000000  4.587220 38   
# [5]  {citrus fruit,tropical fruit,root vegetables,whole milk}   => {other vegetables} 0.003152008 0.8857143  4.577509 31   
# [6]  {tropical fruit,whole milk,curd}                           => {yogurt}           0.003965430 0.6093750  4.368224 39   
# [7]  {citrus fruit,tropical fruit,root vegetables}              => {other vegetables} 0.004473818 0.7857143  4.060694 44   
# [8]  {tropical fruit,other vegetables,butter}                   => {yogurt}           0.003050330 0.5555556  3.982426 30   
# [9]  {tropical fruit,whole milk,whipped/sour cream}             => {yogurt}           0.004372140 0.5512821  3.951792 43   
# [10] {tropical fruit,whole milk,butter}                         => {yogurt}           0.003355363 0.5409836  3.877969 33  

# Reguły o wartości lift = x ozancza, że elementy z LHS i RHS występują ze sobą x razy częściej w porównaniu do zakupów
# gdzie zakałda się, że nie mają ze sobą związku; tzn.
# cytrusy, owoce tropikalne, inne warzywa i pełne mleko oraz warzywa korzeniowe są kupowane 5,8 razy częsciej razem niż osobno, 
# cytrusy, warzywa korzeniowe, inne warzywa i pełne mleko oraz woce tropikalne są kupowane 5,2 razy częsciej razem niż osobno, 
# zioła i mleko pełne oraz warzywa korzeniowe są kupowane 4,9 razy częsciej razem niż osobno, 
# Zwraca uwagę fakt, iż w zasadzie reguły te są zbudowane głównie z produktów częstych, ale pojawiają się też produkty o znacznie 
# niższym wsparciu (ale z częstych grup produktów)

inspect(sort(rules_groc_new_3, by = 'confidence')[1:10])
#      lhs                                                         rhs                support     confidence lift     count
# [1]  {citrus fruit,tropical fruit,root vegetables,whole milk} => {other vegetables} 0.003152008 0.8857143  4.577509 31   
# [2]  {root vegetables,butter,yogurt}                          => {whole milk}       0.003050330 0.7894737  3.089723 30   
# [3]  {citrus fruit,tropical fruit,root vegetables}            => {other vegetables} 0.004473818 0.7857143  4.060694 44   
# [4]  {root vegetables,other vegetables,brown bread}           => {whole milk}       0.003152008 0.7750000  3.033078 31   
# [5]  {onions,butter}                                          => {whole milk}       0.003050330 0.7500000  2.935237 30   
# [6]  {tropical fruit,curd,yogurt}                             => {whole milk}       0.003965430 0.7500000  2.935237 39   
# [7]  {curd,domestic eggs}                                     => {whole milk}       0.004778851 0.7343750  2.874086 47   
# [8]  {tropical fruit,butter,yogurt}                           => {whole milk}       0.003355363 0.7333333  2.870009 33   
# [9]  {tropical fruit,root vegetables,whipped/sour cream}      => {other vegetables} 0.003355363 0.7333333  3.789981 33   
# [10] {butter,curd}                                            => {whole milk}       0.004880529 0.7164179  2.803808 48 

# Reguły o wartości pewności = x oznacza, że za każdym razem, gdy zakupiono produkty z LHS, produkty z RHS były również kupowane x przypadków 
# (conf(X->Y) można intepretować jak prawdopodobieństwo warunkowe P(Y|X)) tzn.
# gdy zakupiono cytrusy, owoce tropikalne, warzywa korzeniowe i pełne mleko to w 88% przypadków zakupiono też minne warzywa
# gdy zakupiono warzywa korzeniowe, masło i jogurt to w 79% przypadkach zakupiono również mleko pełne
# gdy zakupiono cytrusy, owoce tropikalne i warzywa korzeniowe to w 79% przypadkach zakupiono również mleko pełne
# W porównaniu reguł w 1. podejściu - otrzymaliśmy te same reguły 

inspect(sort(rules_groc_new_3, by = 'support')[1:10])
#      lhs                                      rhs                support    confidence lift     count
# [1]  {other vegetables,yogurt}             => {whole milk}       0.02226741 0.5128806  2.007235 219  
# [2]  {tropical fruit,yogurt}               => {whole milk}       0.01514997 0.5173611  2.024770 149  
# [3]  {other vegetables,whipped/sour cream} => {whole milk}       0.01464159 0.5070423  1.984385 144  
# [4]  {root vegetables,yogurt}              => {whole milk}       0.01453991 0.5629921  2.203354 143  
# [5]  {pip fruit,other vegetables}          => {whole milk}       0.01352313 0.5175097  2.025351 133  
# [6]  {root vegetables,yogurt}              => {other vegetables} 0.01291307 0.5000000  2.584078 127  
# [7]  {root vegetables,rolls/buns}          => {whole milk}       0.01270971 0.5230126  2.046888 125  
# [8]  {other vegetables,domestic eggs}      => {whole milk}       0.01230300 0.5525114  2.162336 121  
# [9]  {tropical fruit,root vegetables}      => {other vegetables} 0.01230300 0.5845411  3.020999 121  
# [10] {root vegetables,rolls/buns}          => {other vegetables} 0.01220132 0.5020921  2.594890 120

# Reguła o wartości wsparcia względnego = x oznacza, że x transakcji zawiera wymienione w LHS i RHS produkty.
# reguła inne warzywa i jogurt => pełne mleko o wsparciu względnym 0,022 oznacza, iż produkty te występują razem w 2,2% transakcji
# reguła owoce tropikalne i jogurt => pełne mleko o wsparciu względnym 0,015 oznacza, iż produkty te występują razem 1,5% transakcji, itd.
# CIEKAWOSTKA Powyższe reguły mają pewność > 50%, czyli są prawdziwe dla połowy zakupów zawierajcych lhs
# Reguły powyższe są zupełnie inne niż jak otrzymano w 1. podejściu i 2. podjeściu


# _3B model apriori grupy produkty - 3.podejście ----
# budujemy model - reguły
rules_groc_lvl2_new_3 <- apriori(Groceries_lvl2, ap_param_3)

summary(rules_groc_lvl2_new_3)
# set of 4417 rules
# 
# rule length distribution (lhs + rhs):sizes
# 2    3    4    5    6    7 
# 50  884 2007 1225  246    5 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   4.000   4.000   4.169   5.000   7.000 
# 
# summary of quality measures:
#   support           confidence          lift           count        
# Min.   :0.003050   Min.   :0.5000   Min.   :1.129   Min.   :  30.00  
# 1st Qu.:0.003559   1st Qu.:0.5541   1st Qu.:1.697   1st Qu.:  35.00  
# Median :0.004474   Median :0.6238   Median :1.899   Median :  44.00  
# Mean   :0.006637   Mean   :0.6523   Mean   :1.958   Mean   :  65.27  
# 3rd Qu.:0.006711   3rd Qu.:0.7381   3rd Qu.:2.149   3rd Qu.:  66.00  
# Max.   :0.187697   Max.   :1.0000   Max.   :4.293   Max.   :1846.00  
# 
# mining info:
#   data ntransactions support confidence
# Groceries_lvl2          9835   0.003        0.5

# PODSUMOWANIE 
# Znalezieno 4417 reguł asocjacyjnych dla żądanych parametrów przeszukiwania
# tj. support = 0.003, confidence = 0.5
# 44,5% reguł - 2007 z 4417 (45,4%- ma długość (lhs+rhs) równą 4 (4 elementy)
# 1225 (27,7%) i 884 (20%) m a - odpowiednie - długość 3 i 5   

summary(size(rules_groc_lvl2_new_3))
barplot(table(size(rules_groc_lvl2_new_3)), col = 'gold', 
        main = 'Length of the associate rules = LHS+RHS (products group)')

inspect(sort(rules_groc_lvl2_new_3, by = 'lift')[1:10])
#      lhs                                                                        rhs       support     confidence lift     count
# [1]  {dairy produce,fruit,sausage,vegetables,vinegar/oils}                   => {cheese}  0.003152008 0.5438596  4.292825 31   
# [2]  {fruit,sausage,vegetables,vinegar/oils}                                 => {cheese}  0.003457041 0.5151515  4.066224 34   
# [3]  {bread and backed goods,eggs,fruit,sausage}                             => {cheese}  0.003253686 0.5079365  4.009274 32   
# [4]  {dairy produce,frozen foods,fruit,non-alc. drinks,sausage}              => {cheese}  0.003558719 0.5000000  3.946629 35   
# [5]  {bread and backed goods,cheese,chocolate,fruit}                         => {sausage} 0.003863752 0.6333333  3.348835 38   
# [6]  {bread and backed goods,cheese,frozen foods,fruit}                      => {sausage} 0.004677173 0.6301370  3.331934 46   
# [7]  {bread and backed goods,cheese,chocolate,dairy produce,non-alc. drinks} => {sausage} 0.003253686 0.6274510  3.317731 32   
# [8]  {bread and backed goods,cheese,dairy produce,poultry}                   => {sausage} 0.003050330 0.6250000  3.304772 30   
# [9]  {bread and backed goods,chocolate,frozen foods,non-alc. drinks}         => {sausage} 0.003050330 0.6250000  3.304772 30   
# [10] {bread and backed goods,cheese,chocolate,dairy produce,fruit}           => {sausage} 0.003152008 0.6200000  3.278333 31   

# Reguły o wartości lift = x ozancza, że elementy z LHS i RHS występują ze sobą x razy częściej w porównaniu do zakupów
# gdzie zakałada się, że nie mają ze sobą związku; tzn.
# produkty z grup produkty mleczne, owoce, kiełabsy, warzywa oraz ocet i oleje wraz z serami są 4,3 raza częściej kupowane razem niż osobno (dotyczy LHS i RHS)
# owoce, kiełabsy, warzywa oraz ocet i oleje wraz z serami są 4 razy częściej kupowane niż osobno
# pieczywo i wypieki, jajka, owoce i kiełbasy wraz z serami są 4 razy częściej kupowane niż osobno

inspect(sort(rules_groc_lvl2_new_3, by = 'confidence')[1:10])
#      lhs                                                                rhs             support     confidence lift     count
# [1]  {chocolate,fruit,staple foods}                                  => {dairy produce} 0.003558719 1.0000000  2.257287 35   
# [2]  {eggs,fruit,staple foods}                                       => {dairy produce} 0.003660397 0.9729730  2.196279 36   
# [3]  {cheese,eggs,perfumery}                                         => {dairy produce} 0.003660397 0.9729730  2.196279 36   
# [4]  {eggs,fruit,non-alc. drinks,vinegar/oils}                       => {dairy produce} 0.003457041 0.9714286  2.192793 34   
# [5]  {frozen foods,fruit,non-alc. drinks,staple foods}               => {dairy produce} 0.003152008 0.9687500  2.186747 31   
# [6]  {frozen foods,fruit,non-alc. drinks,vinegar/oils}               => {dairy produce} 0.003050330 0.9677419  2.184471 30   
# [7]  {bread and backed goods,cheese,fruit,non-alc. drinks,perfumery} => {dairy produce} 0.003050330 0.9677419  2.184471 30   
# [8]  {bread and backed goods,fruit,pork,sausage}                     => {dairy produce} 0.004880529 0.9600000  2.166996 48   
# [9]  {bread and backed goods,cheese,non-alc. drinks,perfumery}       => {dairy produce} 0.004270463 0.9545455  2.154683 42   
# [10] {bread and backed goods,eggs,fruit,vinegar/oils}                => {dairy produce} 0.003863752 0.9500000  2.144423 38  

# Reguły o wartości pewności = x oznacza, że za każdym razem, gdy zakupiono produkty z LHS, produkty z RHS były kupowane x przypadków 
# conf(X->Y) można intepretować jak prawdopodobieństwo warunkowe P(Y|X)) tzn.
# jeżeli zakupiono produkty z grup czekolady, owoce i podstawowe produkty (mąka, ryż, kasze) to w 100% transakcji zakupiono produkty mleczne
# jeżeli zakupiono produkty z grup jajka, owoce i podstawowe produkty (mąka, ryż, kasze) to w 97% transakcji zakupiono produkty mleczne
# jeżeli zakupiono produkty z grup sery, jajka i perfumeria to w 97% transakcji zakupiono produkty mleczne
# Zwraca uwagę fakt, iż reguły powstały niekoniecznie z elementów częstych, w porównaniu do reguł z 1. podejścia i 1.podejścia
# - w zasadzie nastęþnik jest ten sam (produkty mleczne); reguły mają bardzo wysokie zaufanie - powyżej 95% 
# UWAGA ponieważ produkty mleczne występują w 44% transakcji - tworzą reguły o dużej pewności, gdy są w następniku

inspect(sort(rules_groc_lvl2_new_3, by = 'support')[1:10])
#      lhs                                    rhs                      support    confidence lift     count
# [1]  {bread and backed goods}            => {dairy produce}          0.18769700 0.5432607  1.226295 1846 
# [2]  {vegetables}                        => {dairy produce}          0.17041179 0.6242086  1.409018 1676 
# [3]  {fruit}                             => {dairy produce}          0.15638027 0.6277551  1.417024 1538 
# [4]  {sausage}                           => {dairy produce}          0.10737163 0.5677419  1.281557 1056 
# [5]  {sausage}                           => {bread and backed goods} 0.10360956 0.5478495  1.585668 1019 
# [6]  {cheese}                            => {dairy produce}          0.08459583 0.6677368  1.507274  832 
# [7]  {bread and backed goods,vegetables} => {dairy produce}          0.08195221 0.7051619  1.591753  806 
# [8]  {fruit,vegetables}                  => {dairy produce}          0.07869853 0.7350427  1.659203  774 
# [9]  {dairy produce,fruit}               => {vegetables}             0.07869853 0.5032510  1.843379  774 
# [10] {bread and backed goods,fruit}      => {dairy produce}          0.07727504 0.7183365  1.621492  760 

# Reguła o wartości wsparcia względnego = x oznacza, że x transakcji zawiera wymienione w LHS i RHS produkty.
# reguła o wsparciu względnym 0,18 oznacza, iż w 18% transakcji występują produkty z działu produkty mleczne
# reguła o wspraciu względnym 0,17 oznacza, iż w 17% transakcji zawiera warzywa i produkty mleczne,
# Reguły powyższe - w nastęþniku maja głównie  element produkty mleczne, w poprzedniku zaś - elementy częste.
# Dla pierwszych 5 reguł zauważyć należy, że przy wysokim wsparciu względnym >10% reguły mają też stosunkowo duże
# zaufanie - w granicach 0.55-0.63.

# WNIOSKI -----------------------------------------------------------------
# Powinny mieć oparcie w zaraportowanych eksperymentach.
# Powinny by nietrywialne (np. wskazanie w jakim eksperymencie 
#                          i dla jakich parametrów uzyskano najlepszy wynik nie wystarczy).

# ANALIZA BAZY - produkty i grupy produktów

# BAZOWE WNIOSKI dotyczące listy produktów (elementy ze wspaciem > 10% liczba transakcji (984))
# najczęściej występujące w listach zakupowych produkty to:
list_pop_products
#  [1] "whole milk"       "other vegetables" "rolls/buns"       "soda"             "yogurt"          
#  [6] "bottled water"    "root vegetables"  "tropical fruit" 

tab_freq[list_pop_products]
# whole milk other vegetables       rolls/buns             soda           yogurt    bottled water  root vegetables   tropical fruit 
#  0.2555160        0.1934926        0.1839349        0.1743772        0.1395018        0.1105236        0.1089985        0.1049314 

# BAZOWE WNIOSKI dotyczące grup produktów (grup produktów ze wspaciem > 10% liczba transakcji (984))
# najczęściej występujące grupy produktów (= działy sklepu) to:
list_pop_gr_products
#  [1] "dairy produce"          "bread and backed goods" "non-alc. drinks"        "vegetables"             
#  [5] "fruit"                  "sausage"                "beer"                   "cheese"          
#  [9] "frozen foods"           "chocolate" 
tab_freq_lvl2[list_pop_gr_products]
# dairy produce bread and backed goods        non-alc. drinks             vegetables                  fruit                sausage 
#     0.4430097              0.3455008              0.3179461              0.2730046              0.2491103              0.1891205 
#          beer                 cheese           frozen foods              chocolate 
#     0.1555669              0.1266904              0.1169293              0.1087951

# produkty w częstych grupach produktów
list_lvl2$`dairy produce`
# [1] "whole milk"         "butter"             "curd"               "dessert"            "butter milk"        "yogurt"            
# [7] "whipped/sour cream" "beverages" 
list_lvl2$`bread and backed goods`
# [1] "rolls/buns"          "white bread"         "brown bread"         "pastry"              "roll products "      "semi-finished bread"
# [7] "zwieback"  

# ANALIZA REGUŁ
# Przeprowadzono trzy eksperymenty - niezależnie dla produktów i grup produktów - dla parametrów
#   1.podejście   support = 0.003, confidence = 0.2
#   2.podejście   support = 0.03,  confidence = 0.2
#   3.podejście   support = 0.003, confidence = 0.5

# do dalszej analizy i ostatecznych wniosków wybrano zbiór reguł, które powstały w 3.podejściu, 
# ze względu na wysoką wartość zaufania.

# Analizując reguły dla produktów:
inspect(sort(rules_groc_new_3, by = 'confidence')[1:10])
inspect(sort(rules_groc_new_3, by = 'support')[1:10])
# Pierwsza zależność (reguły sortowane wg wsparcia), rzucająca się w oczy to, że jeżeli warzywa i owoce to 
# zakupiono również pełne mleko - są to reguły z zaufaniem > 78,5%; w regułach o niższym zaufaniu pojawiają się też
# produkty z innych grup porduktów, tj. pieczywo i inne rzadkie.
# Pośród reguł posortowanych wg wartości wsparcia zależność jest jeżeli warzywa lub owoce oraz produkt z grupy produkty mleczne 
# (jogurt lub bita śmietana i śmietana zwykła)  to pełne mleko lub inne warzywa - są to reguły ze wsparciem względnym > 0,012 
# (wsparcie bezwzględne > 118) oraz z zaufaniem > 50% (czyli 1 na 2 transakcje na pewno będą tak wyglądały).
# Zasadniczo pierwsze 30 reguł z tej listy opiera sie o produkty z grupy warzywa i owoce oraz produkty mleczne!
# Nie dziwi to, gdy uwzględni się bardzo wysokie wsparcie mleka pełnego (25,6% transakcji zawieraja ten produkt)
# oraz innych warzyw (19% transakcji); kolejne często występujące produkty to jogurt, warzywa korzeniowe oraz
# owoce cytrusowe (odpowiednio 14%, 10,9% i 10,5% transakcji) - stąd ich obecnośc w regułach o dużym zaufaniu
# nie dziwi.

# Wizualizacja wniosków
plot(rules_groc_new_3[quality(rules_groc_new_3)$confidence > 0.75], 
     method = 'graph', shading = 'confidence', measure = 'support')
plot(rules_groc_new_3[quality(rules_groc_new_3)$confidence > 0.73], 
     method = 'matrix', shading = 'confidence')
# [1] "{citrus fruit,tropical fruit,root vegetables}"            "{root vegetables,other vegetables,brown bread}"          
# [3] "{citrus fruit,tropical fruit,root vegetables,whole milk}" "{root vegetables,butter,yogurt}"                         
# Itemsets in Consequent (RHS)
# [1] "{whole milk}"       "{other vegetables}"

# Analizując reguły dla grup produktowych (level 2):
inspect(sort(rules_groc_lvl2_new_3, by = 'confidence')[1:10])
inspect(sort(rules_groc_lvl2_new_3, by = 'support')[1:10])
# Ciekawie wygląda reguła z 100% zaufaniem: jeżeli kupiono produkty z grup czekolady, owoce i podstawowe produkty 
# (mąka, ryż, kasze)  to produkty mleczne. Również ciekawe są kolejne reguły z zaufaniem graniczącym z 100% (>97%) 
# jakkolwiek ich wsparcie względne jest pomijalnie małe - wynosi ~0.003-0.004 (wsparcie bezwzględne 30-38).
# Reguły te zbudowane sa z produktów rzadkich, acz bardzo silne wsparcie skłania do przyjrzenia się im dokładnie
# - szczególnei w kontekście ewentualnej arażacje poszczególnych działów względem siebie.
# Natomiast reguły posortowane wg wsparcia dostarczają nam dwóch istotnych inforamacji - ogólna zasada
# że produkt z grupy częste w poprzedniku z ponad 55% pewnością wiąże się z zakupem kolejnego produktu z grupy częstej;
# a druga obserwacja - że będzie to najpewniej produkt z działu produkty mleczne; dla pierwszych pięciu reguł
# z tego zbioru wsparcie wynosi 10-18%, tzn. 10-18% transakcji zawiera te produkty (co pokrywa się z przyjęta definicją
# elementu częstego z eksperymencie dla produktów częstych)

# CIEKAWOSTKI
inspect(sort(subset(rules_groc_lvl2_new_3, subset = lhs %in% 'dairy produce' & confidence > 0.80), by = 'confidence'))
# jeżeli w lhs jest produkt z grupy produktów mlecznych to z 80% w rhs będą warzywa lub pieczywo i wypieki
inspect(sort(subset(rules_groc_lvl2_new_3, subset = lhs %in% 'bread and backed goods' & confidence > 0.80), by = 'confidence'))
# jeżeli w lhs jest produkt z grupy pieczywo i wypieki to z 80% w rhs będzie produkt z grupy produktów mlecznych (258 reguł)
inspect(sort(subset(rules_groc_lvl2_new_3, subset = lhs %in% 'vegetables' & confidence > 0.80), by = 'confidence'))  
# jeżeli w lhs jest produkt z grupy warzywa to z 80% w rhs będzie produkt z grupy produktów mlecznych (267 reguł)
## uwaga dwa wyjątki - 186 i 258 (pieczywo i wypieki)
inspect(sort(subset(rules_groc_lvl2_new_3, subset = rhs %in% 'dairy produce'), by = 'confidence'))
# produkty mleczne występują w rhs z pewnością ponad 50% (1434 reguły - 1/3 wszystkich utworzonych)
inspect(sort(subset(rules_groc_lvl2_new_3, subset = rhs %in% 'dairy produce'), by = 'count')[1:15])
# pierwsze trzy reguły są dwuelementowe i tworza je częste grupy produktów (jeżeli pieczywo i wypieki / warzywa / owoce
# to produkty mleczne) - reguły te mają zaufanie >54% oraz bardzo wysokie wsparcie >15,6%
inspect(sort(subset(rules_groc_lvl2_new_3, subset = lhs %in% 'non-alc. drinks' & confidence > 0.80), by = 'support')[1:10])
inspect(sort(subset(rules_groc_lvl2_new_3, subset = rhs %in% 'non-alc. drinks' & confidence > 0.30), by = 'support')[1:10])
# napoje niealkoholowe - mimo że sa częstą grupą produktów - tworzą reguły o bardzo małym wsparciu
inspect(sort(subset(rules_groc_lvl2_new_3, subset = rhs %in% 'vegetables' & confidence > 0.75), by = 'support'))  
# jeżeli w rhs są warzywa - reguły mają niskie wsparcie < 0,88% 
inspect(subset(subset(rules_groc_lvl2_new_3, items %in% 'dairy produce'), items %in% 'vegetables'))
# 17% (1676 z 9835) transakcji zawiera równocześnie produkty mleczne i warzywa
inspect(subset(subset(rules_groc_lvl2_new_3, items %in% 'bread and backed goods'), items %in% 'vegetables'))
# 11% (1143 z 9835) transakcji równoczęsnie pieczywo i bułeczki oraz warzywa


# Wizualizacja wniosków
plot(rules_groc_lvl2_new_3[quality(rules_groc_lvl2_new_3)$confidence >= 0.95], 
     method = 'graph', shading = 'confidence', measure = 'support')
plot(rules_groc_lvl2_new_3[quality(rules_groc_lvl2_new_3)$confidence >= 0.95], 
     method = 'grouped', shading = 'confidence', measure = 'support')

# PODSUMOWANIE
# Z powyższych analiz wynika, że najczęściej kupowanym produktem jest mleko pełne, natomiast 1/3 reguł w następniku
# ma produkty mleczne (1434/4417). Stąd zalecenia, aby dział produktów mlecznych był na samym końcu sklepu - tak, aby klient
# musiał przejść możliwie długa trasę. Mocną zależnością jest 'warzywa i owoce to zakupiono również pełne mleko', 
# stąd dział warzywny i owocowy powinny się znaleźć na poczatku sklepu (szczególnie, że reguły mające w następniku
# warzywa mają bardzo małe wsparcie). W regułach opartych na częstych grupach produktów # zaobserwoano silną zależność 
# pomiędzy produktami mlecznymi a pieczywem i wypiekami, co sugerowałoby, że dział # z pieczywme powinien być zlokalizowany 
# obok warzyw i owoców - ewentualnie rpzedzielony jakimś działem rzadkim (zakupy impulsowe). 
# Szczególnie popularnym produktem są bułki i bułeczki  (18% transakcji), stąd zaleca się szczególnie skupic na rozwoju oferty 
# tych produktów. Uwzględniając niezwykle duże wsparcie mleka pełnego (25%) zaleca się dołozenie wszelkich starań by go nie zabrakło
# w ofercie. Nadmienic należy, iż podobnie wysoką pozycje ma jogurt (14%).
# Warto zauważyć, iż o ile grupa produktów napoje niealkoholowe jest częsta, nei wytwarz ona częstych reguł - bardzo małe
# wsparcie tej grupy sugerowałoby zlokalizowanie tego działu z boku sklepu.
