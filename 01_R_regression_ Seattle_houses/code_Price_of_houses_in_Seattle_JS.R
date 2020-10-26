# copyright 2018 by Janina Smoła

# ___00_PACKAGES ----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(corrplot)
library(caTools)
library(e1071)
library(randomForest)
library(Metrics)
library(tseries)

par_def <- par()

# ___01_DATA --------------------------------------------------------------
# 01_01 source ----
# getting the data into R as an R object = dataframe

# backup copy of orginal data
df_house_0 <- read.csv('house.csv')

# object to work
df_house <- df_house_0

# exploraing of basic parametres of datafrem with data: size, colnames, data type
dim(df_house) ## 21613    21
colnames(df_house)
  # [1] "id"            "date"          "price"         "bedrooms"      "bathrooms"     "sqft_living"   "sqft_lot"      "floors"       
  # [9] "waterfront"    "view"          "condition"     "grade"         "sqft_above"    "sqft_basement" "yr_built"      "yr_renovated" 
  # [17] "zipcode"       "lat"           "long"          "sqft_living15" "sqft_lot15"  

str(df_house)
  # 'data.frame':	21613 obs. of  21 variables:
  # $ id           : num  7.13e+09 6.41e+09 5.63e+09 2.49e+09 1.95e+09 ...
  # $ date         : Factor w/ 372 levels "20140502T000000",..: 165 221 291 221 284 11 57 252 340 306 ...
  # $ price        : num  221900 538000 180000 604000 510000 ...
  # $ bedrooms     : int  3 3 2 4 3 4 3 3 3 3 ...
  # $ bathrooms    : num  1 2.25 1 3 2 4.5 2.25 1.5 1 2.5 ...
  # $ sqft_living  : int  1180 2570 770 1960 1680 5420 1715 1060 1780 1890 ...
  # $ sqft_lot     : int  5650 7242 10000 5000 8080 101930 6819 9711 7470 6560 ...
  # $ floors       : num  1 2 1 1 1 1 2 1 1 2 ...
  # $ waterfront   : int  0 0 0 0 0 0 0 0 0 0 ...
  # $ view         : int  0 0 0 0 0 0 0 0 0 0 ...
  # $ condition    : int  3 3 3 5 3 3 3 3 3 3 ...
  # $ grade        : int  7 7 6 7 8 11 7 7 7 7 ...
  # $ sqft_above   : int  1180 2170 770 1050 1680 3890 1715 1060 1050 1890 ...
  # $ sqft_basement: int  0 400 0 910 0 1530 0 0 730 0 ...
  # $ yr_built     : int  1955 1951 1933 1965 1987 2001 1995 1963 1960 2003 ...
  # $ yr_renovated : int  0 1991 0 0 0 0 0 0 0 0 ...
  # $ zipcode      : int  98178 98125 98028 98136 98074 98053 98003 98198 98146 98038 ...
  # $ lat          : num  47.5 47.7 47.7 47.5 47.6 ...
  # $ long         : num  -122 -122 -122 -122 -122 ...
  # $ sqft_living15: int  1340 1690 2720 1360 1800 4760 2238 1650 1780 2390 ...
  # $ sqft_lot15   : int  5650 7639 8062 5000 7503 101930 6819 9711 8113 7570 ...

# According to the instruction from Client - only columns with no 1:15 should be used for model building purposes
  # 'id':str – database identifier of the current row,
  # 'date':str – pricing date,
  # 'price':float – the price
  # 'bedrooms':float – number of bedrooms,
  # 'bathrooms':float – number of bathrooms,
  # 'sqft_living':float – living space area,
  # 'sqft_lot':int – lot area,
  # 'floors':float – number of floors,
  # 'waterfront':int – indicator of whether the property is facing water {0,1},
  # 'view':int – quality of view from the property (0:4),
  # 'condition':int – property condition (1:5),
  # 'grade':int – property grade (1:13),
  # 'sqft_above':int – living area above ground level,
  # 'sqft_basement':int – area of basement,
  # 'yr_built':int – year the building was built

colnames(df_house[1:15]) #-# checked
  # [1] "id"            "date"          "price"         "bedrooms"      "bathrooms"     "sqft_living"   "sqft_lot"      "floors"       
  # [9] "waterfront"    "view"          "condition"     "grade"         "sqft_above"    "sqft_basement" "yr_built"  

# stat of data
summary(df_house[1:15])
  #       id                         date           price            bedrooms        bathrooms      sqft_living       sqft_lot      
  # Min.   :1.000e+06   20140623T000000:  142   Min.   :  75000   Min.   : 0.000   Min.   :0.000   Min.   :  290   Min.   :    520  
  # 1st Qu.:2.123e+09   20140625T000000:  131   1st Qu.: 321950   1st Qu.: 3.000   1st Qu.:1.750   1st Qu.: 1427   1st Qu.:   5040  
  # Median :3.905e+09   20140626T000000:  131   Median : 450000   Median : 3.000   Median :2.250   Median : 1910   Median :   7618  
  # Mean   :4.580e+09   20140708T000000:  127   Mean   : 540088   Mean   : 3.371   Mean   :2.115   Mean   : 2080   Mean   :  15107  
  # 3rd Qu.:7.309e+09   20150427T000000:  126   3rd Qu.: 645000   3rd Qu.: 4.000   3rd Qu.:2.500   3rd Qu.: 2550   3rd Qu.:  10688  
  # Max.   :9.900e+09   20150325T000000:  123   Max.   :7700000   Max.   :33.000   Max.   :8.000   Max.   :13540   Max.   :1651359  
  #                     (Other)        :20833                                                                                       
  #      floors        waterfront            view          condition         grade          sqft_above   sqft_basement       yr_built   
  # Min.   :1.000   Min.   :0.000000   Min.   :0.0000   Min.   :1.000   Min.   : 1.000   Min.   : 290   Min.   :   0.0   Min.   :1900  
  # 1st Qu.:1.000   1st Qu.:0.000000   1st Qu.:0.0000   1st Qu.:3.000   1st Qu.: 7.000   1st Qu.:1190   1st Qu.:   0.0   1st Qu.:1951  
  # Median :1.500   Median :0.000000   Median :0.0000   Median :3.000   Median : 7.000   Median :1560   Median :   0.0   Median :1975  
  # Mean   :1.494   Mean   :0.007542   Mean   :0.2343   Mean   :3.409   Mean   : 7.657   Mean   :1788   Mean   : 291.5   Mean   :1971  
  # 3rd Qu.:2.000   3rd Qu.:0.000000   3rd Qu.:0.0000   3rd Qu.:4.000   3rd Qu.: 8.000   3rd Qu.:2210   3rd Qu.: 560.0   3rd Qu.:1997  
  # Max.   :3.500   Max.   :1.000000   Max.   :4.0000   Max.   :5.000   Max.   :13.000   Max.   :9410   Max.   :4820.0   Max.   :2015 

# 01_02 quick look on dataset ----
# a quick look at the nature of data (without 'data' - factor)
plot_histogram(df_house[1:15])  
plot_density(df_house[1:15])
plot_correlation(df_house[1:15])

# FIRST OBSERVATION ABOUT DATA SET
# - variables 'waterfront', 'view', 'condition' and 'grade' are the categorical type variable (Factor),
# - variable 'date' has to be change to date type,
# - to consideration: variable 'yr_built' has to be change to date type,
# - to consideration: variable 'id' has to be in normal not scientific format,
# - question: why variable 'bathrooms' - number of bathrooms - has not integer value?
# - question: why variable 'floors' - number of floor - has not integer value? (mezzanine?)
# - question: what describes variable 'condition'? is that the expert opinion (subjective)
#             or same evaluation made on an objective scale?
# - question: what has coded variable 'grade'?
# - quite good correlation (DV): price ~ sqft_living, price ~ grade, price ~ sqft_above
# - very strong correlation:  sqft_above ~ sqft_living (are they dependant?), 
# - quite good correlation: sqft_above ~ bathrooms, sqft_living ~ grade, sqft_above ~ grade, 
#             bathrooms ~ grade, bathrooms ~ sqft_above

# TYPES OF VARIABLES
# variable - object identifier: 'id'
# dependent variable - continuous variable: 'price'

# independent variable - numerical variable (discrete): 'date', 'bedrooms', 'bathrooms', 'floors', 'yr_built'
# independent variable - numerical variable (continuous): 'sqft_living', sqft_lot', 'sqft_above', 'sqft_basement' 
# independent variable - categorical variable (nominal): 'waterfront', 'view', 'grade'
# independent variable - categorical variable (probably ordinal): 'condition'

# 01_03 NAs check ----
# checking if any NAs are in the data
any(is.na(df_house)) ## FALSE
plot_missing(df_house)


# ___02_EDA ---------------------------------------------------------------
# checking mutual dependence of variables
#pairs(df_house[3:15], pch = '.')

corrplot.mixed(cor(df_house[3:15]), tl.cex = 0.6, tl.col = 'black',
               upper.col = colorRampPalette(c('darkgray', 'darkorange'))(10),
               lower.col = colorRampPalette(c('darkgray', 'darkorange'))(10)
)

# 02_01 price ----
summary(df_house$price)
  #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 75000  321950  450000  540088  645000 7700000
boxplot(df_house$price/1000, horizontal = TRUE, col = 'darkorange',
        main = 'Boxplot of house prices', xlab = 'price [k$]')

hist(df_house$price/1000, col = 'darkorange', breaks = 80,
     main = 'Histogram of house prices', xlab = 'price [k$]')

plot(density(df_house$price/1000), 
     main = 'Density of house prices', xlab = 'price [k$]')
grid()
polygon(density(df_house$price/1000), col = 'darkorange')
abline(v = c(mean(df_house$price)/1000, median(df_house$price)/1000), col = 'red')

# 02_02 date ----
# new variable - the proper format of date for variable 'date'
df_house$date_pro <- as.Date(df_house$date, '%Y%m%dT000000')
head(df_house$date_pro)
  # [1] "2014-10-13" "2014-12-09" "2015-02-25" "2014-12-09" "2015-02-18" "2014-05-12"

# list of unique data of pricing (help for plot)
ls_unique_data_pro <- sort(unique(df_house$date_pro))
length(ls_unique_data_pro) # 372
ls_unique_data_pro[1] # "2014-05-02"
ls_unique_data_pro[372] # "2014-05-27"
which(ls_unique_data_pro == '2014-12-31') ## 241

# when were the most homes priced?
max(table(df_house$date_pro))
  # 142 
which.max(table(df_house$date_pro))
  # 2014-06-23 
  #         53

# plot: number of pricing in the daily division
plot(table(df_house$date_pro), ylim = c(0, 150), col = 'darkorange', las = 1,
     xaxt = 'n', ylab = 'count',
     main = 'Plot of number of houses priced in the time division (days)')
abline(h = mean(table(df_house$date_pro)), col = 'red')
abline(h = median(table(df_house$date_pro)), col = 'green')
abline(h = max(table(df_house$date_pro)), col = 'blue')
text(x = 0, y = c(mean(table(df_house$date_pro)), median(table(df_house$date_pro)), max(table(df_house$date_pro))) + 5,
     col = c('red', 'green', 'blue'), labels = c('mean', 'median', 'max'),  cex = 0.8)
axis(side = 1, at = c(1, 241, 372), labels = ls_unique_data_pro[c(1, 241, 372)])

# REMARKS During summer months much more houses are valued than at the beginning of the year (winter).

# new variable - 'date_Ym' built only on information about year and month of pricing date
df_house$date_Ym <- strftime(as.Date(df_house$date, '%Y%m%dT000000'), '%Y-%m')
head(df_house$date_Ym)
  # [1] "2014-10" "2014-12" "2015-02" "2014-12" "2015-02" "2014-05"

mean(table(df_house$date_Ym))
  # 2014-05 2014-06 2014-07 2014-08 2014-09 2014-10 2014-11 2014-12 2015-01 2015-02 2015-03 2015-04 2015-05 
  #    1768    2180    2211    1940    1774    1878    1411    1471     978    1250    1875    2231     646 

# plot: number of pricing in the monthly division
barplot(table(df_house$date_Ym), las = 2, col = 'darkorange', ylim = c(0, 2500),
        main = 'Plot of number of houses priced in the time division (months)')
abline(h = mean(table(df_house$date_Ym)), col = 'red')
abline(h = median(table(df_house$date_Ym)), col = 'green')
abline(h = max(table(df_house$date_Ym)), col = 'blue')
text(x = 0, y = c(mean(table(df_house$date_Ym)), median(table(df_house$date_Ym)), max(table(df_house$date_Ym))) + 50,
     col = c('red', 'green', 'blue'), labels = c('mean', 'median', 'max'),  cex = 0.9)

# REMARKS last month 05-2015 has less pricing than even 01-2015 (begining of year, winter)

# 02_03 id ----
boxplot(df_house$id, horizontal = TRUE, col = 'darkorange',
        main = 'Boxplot of id', xlab = 'value')

format(df_house$id, scientific = FALSE)[1:10]
# changing scientific format of id to more user friendly character format
df_house$id <- format(df_house$id, scientific = FALSE) %>% stringr::str_trim()
class(df_house$id) ## "character"

# checking if there are no duplicated items
length(unique(df_house$id)) ## 21436
length(df_house$id) - length(unique(df_house$id)) ## 177

# ___ATTENTION there are some duplicated ids 
# example
sort(df_house$id)
filter(df_house, id == 1000102)
filter(df_house, id == 7200179)
filter(df_house, id == 109200390)
# REMARK Only one diffrence in duplicated item is variable 'date' (also new built based on it - 'date_pro' 
#        and 'date_Ym') and  variable 'price' (?)

# list of duplicated ids
(ls_dup_id <- df_house$id[duplicated(df_house$id)])
length(ls_dup_id) ## 177

# list of duplicate ids with 'price' and date of pricing 'date_pro'(for analysis what is different)
filter(df_house, id %in% ls_dup_id) %>% select(id, price, date_pro)
filter(df_house, id %in% ls_dup_id) %>% select(date_Ym) %>% table()
  # 2014-05 2014-06 2014-07 2014-08 2014-09 2014-10 2014-11 2014-12 2015-01 2015-02 2015-03 2015-04 2015-05 
  #      21      27      24      29      25      28      21      27      23      30      40      42      16 

# checking number of duplicates
filter(df_house, id %in% ls_dup_id) %>% select(id) %>% table() %>% plot(xaxt = 'n', col = c('darkgray', 'darkorange')[1+(. == max(.))])
  # Some items occure 3 times - checking:
filter(df_house, id %in% ls_dup_id) %>% select(id) %>% table() %>% which.max()
  # 795000620 
  #       144
filter(df_house[1:15], id == 795000620)
  #          id            date  price bedrooms bathrooms sqft_living sqft_lot floors waterfront view condition grade sqft_above sqft_basement yr_built
  # 1 795000620 20140924T000000 115000        3         1        1080     6250      1          0    0         2     5       1080             0     1950
  # 2 795000620 20141215T000000 124000        3         1        1080     6250      1          0    0         2     5       1080             0     1950
  # 3 795000620 20150311T000000 157000        3         1        1080     6250      1          0    0         2     5       1080             0     1950
# REMARK Each time the price is different!

# minor df: data frame with duplicated ids (forcomparision how the price changes)
df_dup_id <- filter(df_house, id %in% ls_dup_id, id != 795000620) %>% select(id, price, date_pro) 
str(df_dup_id)
  # 'data.frame':	350 obs. of  3 variables:
  # $ id      : chr  "6021501535" "6021501535" "4139480200" "4139480200" ...
  # $ price   : num  430000 700000 1384000 1400000 232000 ...
  # $ date_pro: Date, format: "2014-07-25" "2014-12-23" "2014-06-18" "2014-12-09" ...

# minor df: data frame with difference between price of houses with duplicate ids
df_delta_dup_id <- data.frame(id = filter(df_dup_id, row_number() %% 2 == 0)[ , 1],
                              delta = filter(df_dup_id, row_number() %% 2 == 0)[ , 2] - filter(df_dup_id, row_number() %% 2 == 1)[ , 2])

# plot: price difference for duplicates
plot(sort(df_delta_dup_id$delta)/1000, type = 'h', col = ifelse(sort(df_delta_dup_id$delta) > 0 , 'darkgray', 'darkorange'),
     ylab = 'delta [k$]', xlab = 'no of id', main = 'Plot of price difference for duplicates')
points(x = which(sort(df_delta_dup_id$delta) == 0), y = rep(0, 3), pch = 1, col = 'darkgreen', cex = 0.5)

str(df_delta_dup_id)
  # 'data.frame':	175 obs. of  2 variables:
  # $ id   : Factor w/ 175 levels "1000102","1036400200",..: 106 81 132 77 40 160 128 158 175 148 ...
  # $ delta: num  270000 16000 8500 74900 215000 ...

filter(df_delta_dup_id, delta == 0) %>% nrow() ## 3 
filter(df_delta_dup_id, delta == 0)[ , 1] ## 1825069031 6308000010 8648900110
# 3 items were pricing twice and the price of the house wasn't changed => should they be treated as duplicates?

filter(df_delta_dup_id, delta < 0) %>% nrow()  ## 6
filter(df_delta_dup_id, delta < 0)[ , 1] ## 8682262400 2726049071 4139420590 2767603612 7167000040 2619920170
# 6 items were pricing twice and the price of the house has dropped over time

filter(df_delta_dup_id, delta > 0) %>% nrow() ## 166
# 166 items were pricing twice and the price of the house has increased over time

filter(df_house[ , c(1:15,22:23)], id %in% filter(df_delta_dup_id, delta < 0)[ , 1])
  #            id            date   price bedrooms bathrooms sqft_living sqft_lot floors waterfront view condition grade sqft_above sqft_basement yr_built   date_pro date_Ym
  # 1  8682262400 20140718T000000  430000        2      1.75        1350     4003      1          0    0         3     8       1350             0     2004 2014-07-18 2014-07
  # 2  8682262400 20150513T000000  419950        2      1.75        1350     4003      1          0    0         3     8       1350             0     2004 2015-05-13 2015-05
  # 3  2726049071 20141211T000000  510000        2      1.00         820     4206      1          0    0         3     5        820             0     1949 2014-12-11 2014-12
  # 4  2726049071 20150408T000000  489950        2      1.00         820     4206      1          0    0         3     5        820             0     1949 2015-04-08 2015-04
  # 5  4139420590 20140520T000000 1212500        4      3.50        4560    16643      1          0    3         3    12       2230          2330     1995 2014-05-20 2014-05
  # 6  4139420590 20140827T000000 1200000        4      3.50        4560    16643      1          0    3         3    12       2230          2330     1995 2014-08-27 2014-08
  # 7  2767603612 20140512T000000  500000        2      2.25        1290     1334      3          0    0         3     8       1290             0     2007 2014-05-12 2014-05
  # 8  2767603612 20150113T000000  489000        2      2.25        1290     1334      3          0    0         3     8       1290             0     2007 2015-01-13 2015-01
  # 9  7167000040 20140813T000000  740000        4      3.00        3350   199253      2          0    0         3    10       3350             0     2004 2014-08-13 2014-08
  # 10 7167000040 20150305T000000  700000        4      3.00        3350   199253      2          0    0         3    10       3350             0     2004 2015-03-05 2015-03
  # 11 2619920170 20141001T000000  772500        4      2.50        3230     4290      2          0    0         3     9       3230             0     2004 2014-10-01 2014-10
  # 12 2619920170 20141219T000000  765000        4      2.50        3230     4290      2          0    0         3     9       3230             0     2004 2014-12-19 2014-12


# DUPLICATED? the same price - diffrent pricing date
filter(df_house[ , c(1:15,22:23)], id %in% filter(df_delta_dup_id, delta == 0)[ , 1])
  #           id            date  price bedrooms bathrooms sqft_living sqft_lot floors waterfront view condition grade sqft_above sqft_basement yr_built   date_pro date_Ym
  # 1 1825069031 20140814T000000 550000        4      1.75        2410     8447      2          0    3         4     8       2060           350     1936 2014-08-14 2014-08
  # 2 1825069031 20141016T000000 550000        4      1.75        2410     8447      2          0    3         4     8       2060           350     1936 2014-10-16 2014-10
  # 3 6308000010 20141208T000000 585000        3      2.50        2290     5089      2          0    0         3     9       2290             0     2001 2014-12-08 2014-12
  # 4 6308000010 20150423T000000 585000        3      2.50        2290     5089      2          0    0         3     9       2290             0     2001 2015-04-23 2015-04
  # 5 8648900110 20140505T000000 555000        3      2.50        1940     3211      2          0    0         3     8       1940             0     2009 2014-05-05 2014-05
  # 6 8648900110 20140826T000000 555000        3      2.50        1940     3211      2          0    0         3     8       1940             0     2009 2014-08-26 2014-08

# REMARKS
# In dataset are 177 items that have duplicated id (exception id = 795000620 listed 3times) - the diffrence is only in variable 'date' and 'price' 
# (with 3 exception wher the price is the same)
# In the model, the variable 'id' will not be used, so it is important to decide how to deal with these duplicates.

# PROPOSAL: cancel only second item with the same id from the ids' list with the same price and diffrent pricing date.

# new variable - indicators of whether to delete the given item
df_house$rm <- 0
# list of duplicated items - houses for that the price is the same on diffrent pricing date
(ls_rm_3id <- filter(df_house, id %in% filter(df_delta_dup_id, delta == 0)[ , 1]) %>% filter(row_number() %% 2 == 0))

df_house[df_house$id == ls_rm_3id$id, 'rm'] <- 1
#-# checked
filter(df_house, id %in% filter(df_delta_dup_id, delta == 0)[ , 1])


# 02_04 bedrooms ----
table(df_house$bedrooms)
  #  0    1    2    3    4    5    6    7    8    9   10   11   33 
  # 13  199 2760 9824 6882 1601  272   38   13    6    3    1    1 

summary(df_house$bedrooms)
  #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.000   3.000   3.000   3.371   4.000  33.000 

boxplot(df_house$bedrooms, horizontal = TRUE, col = 'darkorange',
        main = 'Boxplot of bedrooms', xlab = 'number of bedrooms')

# ATTENTION According to the american standards for houses appraisal (given data set is a list of houses located in area of Seattle,
#           state of Washington) bedroom must be of adequate size (100 square feet or more), have a closet, a window and a door. 
#           It must be heated/cooled and finished in the same quality as the rest of the house. It must also 
#           be above grade and have reasonable access to a full bathroom. Basement bedrooms (50% below adjacent 
#           grade level) may NOT be counted in the total bedrooms count.

# ___OUTLIER OR MISTAKE ? 
filter(df_house[ , c(1:15,22:24)], bedrooms == 33)
  #           id            date  price bedrooms bathrooms sqft_living sqft_lot floors waterfront view condition grade sqft_above sqft_basement yr_built   date_pro date_Ym rm
  # 1 2402100895 20140625T000000 640000       33      1.75        1620     6000      1          0    0         5     7       1040           580     1947 2014-06-25 2014-06  0

# analysis of similar items (only one parametres)
par(mfcol = c(2, 3))
filter(df_house, grade == 7) %>% select(bedrooms) %>% table() %>% barplot(., las = 1, main = 'Grade ~ bedrooms', col = c('gray', 'darkorange')[1+(. == max(.))])
  # 0    1    2    3    4    5    6    7    8    9   10   11   33 
  # 6   52 1205 4917 2177  501   98   11    6    4    2    1    1 
filter(df_house, bathrooms == 1.75) %>% select(bedrooms) %>% table() %>% barplot(., las = 1, main = 'Bathrooms ~ bedrooms', col = c('gray', 'darkorange')[1+(. == max(.))])
  # 1    2    3    4    5    6   33 
  # 4  304 1870  719  134   16    1 
filter(df_house, sqft_living == 1620) %>% select(bedrooms) %>% table() %>% barplot(., las = 1, main = 'Sqft_living ~ bedrooms', col = c('gray', 'darkorange')[1+(. == max(.))])
  #  2  3  4 33 
  # 14 64 20  1 
filter(df_house, floors == 1) %>% select(bedrooms) %>% table() %>% barplot(., las = 1, main = 'Floors ~ bedrooms', col = c('gray', 'darkorange')[1+(. == max(.))])
  # 0    1    2    3    4    5    6    7    8   10   33 
  # 4  162 1951 5455 2383  605  104    9    5    1    1 
filter(df_house, price == 640000) %>% select(bedrooms) %>% table() %>% barplot(., las = 1, main = 'Price ~ bedrooms', col = c('gray', 'darkorange')[1+(. == max(.))])
  # 2  3  4  5  6 33 
  # 5 21 21  3  1  1 
filter(df_house, yr_built == 1947) %>% select(bedrooms) %>% table() %>% barplot(., las = 1, main = 'Yr_built ~ bedrooms', col = c('gray', 'darkorange')[1+(. == max(.))])
  #  1  2  3  4  5  6 33 
  # 10 94 95 55  7  1  1 
par(mfcol = c(par_def$mfcol))
# REMARKS similar items (the same value for diffrent parameters: grade = 7, bathrooms == 1.75, sqft_living == 1620, floors == 1
# price == 640000, yr_built == 1947) show that the most popular value for badrooms is 3.

# analysis of similar items (all parametres)
filter(df_house, grade == 7,
                 bathrooms >= 1.75 - 0.75, bathrooms <= 1.75 + 0.75, # +/1 0.75 of 1.75
                 sqft_living >= 0.9 * 1620, sqft_living <= 1.1 * 1620,  # +/- 10% of 1620
                 price >= 0.85 * 640000, price <= 1.15 * 640000,   #+/- 15% of 640000
                 yr_built >= 1947 - 5, yr_built <= 1947 + 5   # +/- 5yrs to 1947
                 ) %>% select(bedrooms) %>% table() %>% barplot(., main = 'Similar to outlier 33', col = c('gray', 'darkorange')[1+(. == max(.))])

# DECISION: it is a mistake => 33 has to be change into 3
df_house[df_house$bedrooms == 33, 'bedrooms'] <- 3

# re-analysis
table(df_house$bedrooms)
  #  0    1    2    3    4    5    6    7    8    9   10   11 
  # 13  199 2760 9825 6882 1601  272   38   13    6    3    1

summary(df_house$bedrooms)
  #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.000   3.000   3.000   3.369   4.000  11.000 

boxplot(df_house$bedrooms, horizontal = TRUE, col = 'darkorange',
        main = 'Boxplot of bedrooms', xlab = 'number of bedrooms')

barplot(table(df_house$bedrooms), las = 1, col = 'darkorange',
        main = 'Plot of number of bedrooms')
abline(h = mean(table(df_house$bedrooms)), col = 'red')

# ARE THESE DATA PROPER? to consideration: removing these items 
filter(df_house[ , c(1:15,22:24)], bedrooms == 0)
# REMARKS Basement bedrooms (50% below adjacent grade level) may NOT be counted in the total bedrooms count, but none
# of these items have basement. It's also possible that some of them do not meet with the requirements.

# 02_05 bathrooms ----
summary(df_house$bathrooms)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.750   2.250   2.115   2.500   8.000

boxplot(df_house$bathrooms, horizontal = TRUE, col = 'darkorange',
        main = 'Boxplot of bathroom', xlab = 'number of bathrooms')

barplot(table(df_house$bathrooms), las = 1, col = 'darkorange', horiz = TRUE,
        main = 'Plot of number of bedrooms')
abline(v = mean(table(df_house$bathrooms)), col = 'red')

# ATTENTION According to the american standards for houses appraisal (given data set is a list of houses located in area of Seattle,
#           state of Washington) a full bathroom is made up of four parts: a sink, a shower, a bathtub, and a toilet. Each utility is counted as one-quarter. 
#           Basement bathroom (50% below adjacent grade level) may NOT be counted in the total bathroom count.
#           So the data - even if the number of bathroom isn't integer - should be considered as correct. 

# max number of bathrooms
filter(df_house[ , c(1:15, 22:24)], bathrooms == 8)
  #           id            date   price bedrooms bathrooms sqft_living sqft_lot floors waterfront view condition grade sqft_above sqft_basement yr_built   date_pro date_Ym rm
  # 1 6762700020 20141013T000000 7700000        6         8       12050    27600    2.5          0    3         4    13       8570          3480     1910 2014-10-13 2014-10  0
  # 2 1225069038 20140505T000000 2280000        7         8       13540   307752    3.0          0    4         3    12       9410          4130     1999 2014-05-05 2014-05  0

# ARE THESE DATA PROPER? to consideration: removing these items
filter(df_house[ , c(1:15,22:23)], bathrooms == 0)
  #            id            date   price bedrooms bathrooms sqft_living sqft_lot floors waterfront view condition grade sqft_above sqft_basement yr_built   date_pro date_Ym
  # 1  6306400140 20140612T000000 1095000        0         0        3064     4764    3.5          0    2         3     7       3064             0     1990 2014-06-12 2014-06
  # 2  3421079032 20150217T000000   75000        1         0         670    43377    1.0          0    0         3     3        670             0     1966 2015-02-17 2015-02
  # 3  3918400017 20150205T000000  380000        0         0        1470      979    3.0          0    2         3     8       1470             0     2006 2015-02-05 2015-02
  # 4  5702500050 20141104T000000  280000        1         0         600    24501    1.0          0    0         2     3        600             0     1950 2014-11-04 2014-11
  # 5  2954400190 20140624T000000 1295650        0         0        4810    28008    2.0          0    0         3    12       4810             0     1990 2014-06-24 2014-06
  # 6  3374500520 20150429T000000  355000        0         0        2460     8049    2.0          0    0         3     8       2460             0     1990 2015-04-29 2015-04
  # 7  7849202190 20141223T000000  235000        0         0        1470     4800    2.0          0    0         3     7       1470             0     1996 2014-12-23 2014-12
  # 8   203100435 20140918T000000  484000        1         0         690    23244    1.0          0    0         4     7        690             0     1948 2014-09-18 2014-09
  # 9  9543000205 20150413T000000  139950        0         0         844     4269    1.0          0    0         4     7        844             0     1913 2015-04-13 2015-04
  # 10 3980300371 20140926T000000  142000        0         0         290    20875    1.0          0    0         1     1        290             0     1963 2014-09-26 2014-09

# REMARKS Basement bathrooms (50% below adjacent grade level) may NOT be counted in the total bathrooms count, but none
# of these items have basement, some of them - don't have even bedroom.

# contingency table: bathrooms and bedrooms
table(df_house$bedrooms, df_house$bathrooms)
# REMARKS There are 7 items with no bedrooms and no bathrooms.

# DECISION It is possible to imagine, that exist houses which bedrooms doesn't meet bedroom requiraments (no cooling) so 
# the number of bedroom will be equal 0, but number of bathroom should not be equal 0 (it could be equal 0 only when the house
# has basement). 
# The case when number of bedrooms and number of bathrooms, both are equal 0 and there is no basement are strange. It is considered 
# as some mistake and will be removed.

# list of items with no bedrooms, no bathrooms and no basement
(ls_rm_0bbb <- filter(df_house, bathrooms == 0, bedrooms == 0, sqft_basement == 0))
dim(ls_rm_0bbb)  ## 7  24

df_house[df_house$id %in% ls_rm_0bbb$id, 'rm'] <- 1
#-# checked
filter(df_house, id %in% filter(df_house, bathrooms == 0, bedrooms == 0, sqft_basement == 0)[ , 1])

# WHAT WITH THE REST 0 BEDROOMS OR 0 BATHROOMS HOUSES?
filter(df_house, bathrooms == 0, rm != 1)
# DECISION These items should be remove - explanation: no bathrooms when there are bedrooms is a mistake (one of rquiraments
# for bedroom is reasonable access to a full bathroom)

# list of items with no bathrooms
(ls_rm_0b1b <- filter(df_house, bathrooms == 0, bedrooms != 0))
dim(ls_rm_0b1b)  ##  3 24

df_house[df_house$id %in% ls_rm_0b1b$id, 'rm'] <- 1
#-# checked
filter(df_house, id %in% filter(df_house, bathrooms == 0, bedrooms != 0)[ , 1])

# items - no bedrooms, but with some bathrooms
filter(df_house, bedrooms == 0, rm != 1)
# DECISION These items will stay - explanation: bedrooms doesn't meet bedroom requiraments (for example: no cooling) 
# but there is bathroom.

#-# checked
filter(df_house, bathrooms == 0)
filter(df_house, bedrooms == 0)

# 02_06 floors ----
table(df_house$floors)
  #     1   1.5     2   2.5     3   3.5 
  # 10680  1910  8241   161   613     8 

boxplot(df_house$floors, horizontal = TRUE, col = 'darkorange',
        main = 'Boxplot of floors', xlab = 'number of floors')

# ATTENTION According to the american standards for houses appraisal (given data set is a list of houses located in area of Seattle,
#           state of Washington) attics and mezzanine should be count as half floor.
#           So the data - even if the number of floor isn't integer - should be considered as correct. 
#           Basement should NOT be counted in the total floor count

barplot(table(df_house$floors), las = 1, col = 'darkorange', 
        main = 'Plot of floors\' number')
abline(h = mean(table(df_house$floors)), col = 'red')
abline(h = median(table(df_house$floors)), col = 'green')
text(x = 0.35, y = c(mean(table(df_house$floors)), median(table(df_house$floors))) + 150,
     col = c('red', 'green'), labels = c('mean', 'median'))

# 02_07 waterfront ----
table(df_house$waterfront)
  #     0     1 
  # 21450   163 

barplot(table(df_house$waterfront), las = 1, col = 'darkorange',
        names.arg = c('no facing water', 'facing water'),
        main = 'Plot of waterfront', xlab = 'indicator of whether the property is facing water')

# REMARKS Almost no house (only 0,7% of all) has a view of the water, what is quite interesting in the Seattle (port city).

# 02_08 view ----
table(df_house$view)
  #     0     1     2     3     4 
  # 19489   332   963   510   319 

barplot(table(df_house$view), las = 1, col = 'darkorange',
        main = 'Plot of view', xlab = 'quality of view from the property')

# 02_09 condition ----
table(df_house$condition)
  #  1     2     3     4     5 
  # 30   172 14031  5679  1701 

barplot(table(df_house$condition), las = 1, col = 'darkorange',
        main = 'Plot of condition', xlab = 'property condition')

# 02_10 grade ----
table(df_house$grade)
  # 1    3    4    5    6    7    8    9   10   11   12   13 
  # 1    3   29  242 2038 8981 6068 2615 1134  399   90   13

barplot(table(df_house$grade), las = 1, col = 'darkorange',
        main = 'Plot of grade', xlab = 'property grade')

# analysis: is the grade dependant of floors?
table(df_house_0$floors, df_house_0$grade)
corrplot.mixed(cor(df_house[ , c('price', 'floors', 'grade')]),
               upper.col = colorRampPalette(c('darkgray', 'darkorange'))(10),
               lower.col = colorRampPalette(c('darkgray', 'darkorange'))(10)
)
# no really strong (0,46)

# 02_11 yr_built ----
table(df_house$yr_built)
  # 1900 1901 1902 1903 1904 1905 1906 1907 1908 1909 1910 1911 1912 1913 1914 1915 1916 1917 1918 1919 1920 1921 1922 1923 
  #   87   29   27   46   45   74   92   65   86   94  134   73   79   59   54   64   79   56  120   88   98   76   95   84 
  # 1924 1925 1926 1927 1928 1929 1930 1931 1932 1933 1934 1935 1936 1937 1938 1939 1940 1941 1942 1943 1944 1945 1946 1947 
  #  139  165  180  115  126  114   90   61   38   30   21   24   40   68   52  106  156  161  223  170  140   95  126  263 
  # 1948 1949 1950 1951 1952 1953 1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 
  #  235  195  250  229  220  223  305  271  198  198  224  334  248  224  312  256  172  187  250  350  381  280  132  104 
  # 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 
  #  149  149  162  189  253  417  387  343  240  199  105  212  229  228  215  294  270  290  320  224  198  202  249  169 
  # 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 
  #  195  177  239  265  218  305  222  422  433  450  454  417  367  230  143  130  170  201  559   38 

length(unique(sort(df_house$yr_built))) ## 116

barplot(table(df_house$yr_built), las = 2, col = 'darkorange',
        main = 'Plot of yr_built', xlab = 'year the building was built', cex.names = 0.65)

# analysis: is the price dependant of 'yr_built' and 'condition'?
corrplot.mixed(cor(df_house[ , c('price', 'yr_built', 'condition')]),
               upper.col = colorRampPalette(c('darkgray', 'darkorange'))(10),
               lower.col = colorRampPalette(c('darkgray', 'darkorange'))(10)
               )
# no really strong (<0.05)

# curio: how often happened the renovation?
df_house[df_house$yr_renovated - df_house$yr_built > 0, ] %>% select(yr_built) %>% table() 
# not really often: only 914/21613 (4,2%) of houses was renovated
filter(df_house, yr_renovated != 0) %>% nrow() ## 914


# new variable: house_age - to solve a problem with difference of house age that is bind with different pricing date
# the price will be a function of one variable 'house_age' and not of two variables 'date' (pricing) and 'yr_built'
# ATTENTION That has also its consequences: after canceling 'date' (pricing) and 'yr_built' there will be not longer
# possible to analysis the price change according the month of pricing or differnce during the year
strftime(as.Date(as.character(df_house$yr_built), '%Y'), '%Y')
df_house$house_age <- as.numeric(strftime(as.Date(df_house$date, '%Y%m%dT000000'), '%Y')) - df_house$yr_built

# stat
summary(df_house$house_age)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # -1.00   18.00   40.00   43.32   63.00  115.00 
table(df_house$house_age)

boxplot(df_house$house_age, horizontal = TRUE, col = 'darkorange',
        main = 'Boxplot of house age', xlab = 'house age [yrs]')

barplot(table(df_house$house_age), las = 1, col = 'darkorange',
        main = 'Plot of house age', xlab = 'house age')

# how many houses was priced in the year of its built?
filter(df_house, house_age == 0) %>% nrow() ## 430

filter(df_house, house_age < 0) 
# There are 12 items with the house age with value equal -1. It's possible if investor asks about pricing 
# based on design - not based on really existing building. Not to put 'strange' data into model and because it's
# only 12 (0.06% of all) items the value -1 will be change into 0 (the houses that were priced in the same 
# year that were also built).

df_house[df_house$house_age == -1, 'house_age'] <- 0
#-# checked
filter(df_house, house_age == -1) %>% select(house_age) 

# re-analysis
boxplot(df_house$house_age, horizontal = TRUE, col = 'darkorange',
        main = 'Boxplot of house age', xlab = 'house age [yrs]')

barplot(table(df_house$house_age), las = 1, col = 'darkorange',
        main = 'Plot of house age', xlab = 'house age')

# new minor variable 'date_Y' to check how the price is correlated with the pricing date?
df_house$date_Y <- as.numeric(strftime(as.Date(df_house$date, '%Y%m%dT000000'), '%Y'))
corrplot.mixed(cor(df_house[ , c('price', 'yr_built', 'house_age', 'date_Y')]),
               upper.col = colorRampPalette(c('darkgray', 'darkorange'))(10),
               lower.col = colorRampPalette(c('darkgray', 'darkorange'))(10)
)

# REMARKS The price is not correlated with the pricing date so it was a good idea to build new variable 'house_age',
# it's possible to cancle 'yr_built' and 'date' (with all group of variable based on it) and leave only 'house_age'


# 02_12 sqft_living ----
summary(df_house$sqft_living) ## 100sf = 9.29m2
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 290    1427    1910    2080    2550   13540 

summary(df_house$sqft_living)/(100/9.29) # m2
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 26.94  132.57  177.44  193.22  236.90 1257.87

boxplot(df_house$sqft_living, horizontal = TRUE, col = 'darkorange',
        main = 'Boxplot of living space area', xlab = 'living space area [sqft]')

hist(df_house$sqft_living, las = 1, col = 'darkorange', breaks = 70,
     main = 'Histogram of living space area', xlab = 'living space area [sqft]')

# max (> 750 m2 ~ 8070sf)
filter(df_house, sqft_living == 13540)
filter(df_house, sqft_living > 8070) %>% select(bedrooms, bathrooms) %>% table()
  #         bathrooms
  # bedrooms 4.5 5.75 6.25 6.75 7.75 8
  #        5   1    1    1    1    0 0
  #        6   0    0    0    0    1 1
  #        7   0    0    0    0    0 1

# min (< 50 m2 ~ 540sf)
filter(df_house, sqft_living < 540) %>% select(bedrooms, bathrooms) %>% table()
  #         bathrooms
  # bedrooms  0 0.75  1
  #        0  1    1  1
  #        1  0   10 10
  #        2  0    2  1
  #        3  0    1  0

# 02_13 sqft_lot ----
summary(df_house$sqft_lot)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 520    5040    7618   15107   10688 1651359 

summary(df_house$sqft_lot)/(100/9.29) # m2
  #  Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
  # 48.31    468.22    707.71   1403.44    992.92 153411.25 

summary(df_house$sqft_lot)/1076.39 # ar
  #   Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
  # 0.4831    4.6823    7.0774   14.0348    9.9295 1534.1642 

boxplot(df_house$sqft_lot, horizontal = TRUE, col = 'darkorange',
        main = 'Boxplot of lot area', xlab = 'lot area [sqft]')

hist(df_house$sqft_lot, col = 'darkorange', breaks = 100,
     main = 'Histogram of lot area', xlab = 'lot area [sqft]')

filter(df_house, sqft_lot > 100000) %>% select(sqft_lot) %>% boxplot(horizontal = TRUE, col = 'darkorange',
                                                                     main = 'Boxplot of living space area >10^5sqft', xlab = 'living space area [sqft]')

# REMARKS there are big difference between the smallest and the biggest lot

# 02_14 sqft_above ----
summary(df_house$sqft_above)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 290    1190    1560    1788    2210    9410 

summary(df_house$sqft_above)/(100/9.29) # m2
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 26.94  110.55  144.92  166.14  205.31  874.19 

boxplot(df_house$sqft_above, horizontal = TRUE, col = 'darkorange',
        main = 'Boxplot of living area above ground level', xlab = 'living area above ground level [sqft]')

hist(df_house$sqft_above, col = 'darkorange', las = 1, breaks = 80,
     main = 'Histogram of space area above', xlab = 'space area above [sqft]')

table(df_house$sqft_above, df_house$floors)

# average floor area
summary(round(df_house$sqft_above/df_house$floors, 2))
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 253.3   940.0  1185.0  1237.0  1465.0  5400.0 
hist(round(df_house$sqft_above/df_house$floors, 2), las = 1, col = 'darkorange', breaks = 50,
     main = 'Histogram of  average floor area', xlab = ' average floor area [sqft]')

# 02_15 sqft_basement ----
summary(df_house$sqft_basement)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.0     0.0     0.0   291.5   560.0  4820.0 

summary(df_house$sqft_basement)/(100/9.29) # m2
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.00    0.00    0.00   27.08   52.02  447.78 

# stat: the most popular basement area
sort(table(df_house$sqft_basement), decreasing = TRUE)[1:10]
  #     0   600   700   500   800   400  1000   900   300   200 
  # 13126   221   218   214   206   184   149   144   142   108 

boxplot(df_house$sqft_basement, horizontal = TRUE, col = 'darkorange',
        main = 'Boxplot of area of basement,', xlab = 'area of basement [sqft]')

hist(df_house$sqft_basement, las = 1, col = 'darkorange', breaks = 50,
     main = 'Histogram of basement area', xlab = 'basement area [sqft]')

filter(df_house, sqft_basement == 0) %>% nrow() ## 13126
# REMARK 60% (13126/21613) of houses do not have basement

# stat: which houses according to the number of floor do and do not have basement?
filter(df_house, sqft_basement == 0) %>% select(floors) %>% table()
  #    1  1.5    2  2.5    3  3.5 
  # 5156 1172 6195   80  516    7 
filter(df_house, sqft_basement != 0) %>% select(floors) %>% table()
  #    1  1.5    2  2.5    3  3.5 
  # 5524  738 2046   81   97    1 
# REMARK more-less: houses with floor number between 1.5 to 3.5 floors more often do not have than have the basement

# new variable: is_basement indicator of the house have basement - to consideration (as bottom)
filter(df_house, sqft_basement == 0) %>% nrow() ## 13126
filter(df_house, sqft_basement != 0) %>% nrow() ## 8487

df_house[df_house$sqft_basement == 0, 'is_basement'] <- 0
df_house[df_house$sqft_basement != 0, 'is_basement'] <- 1

table(df_house$is_basement)
  #     0     1 
  # 13126  8487 

barplot(table(df_house$is_basement), las = 1, col = 'darkorange',
        names.arg = c('without basement', 'with basement'),
        main = 'Plot of basement indicator', xlab = 'does house have basement?')

# ATTENTION One variable can be removed.
table(df_house$sqft_basement + df_house$sqft_above == df_house$sqft_living) ## TRUE 21613
table(round((df_house$sqft_above/df_house$sqft_living), 2))
corrplot.mixed(cor(df_house[ , c('price', 'sqft_lot', 'sqft_basement', 'sqft_above', 'sqft_living', 'is_basement', 'floors')]),
               tl.cex = 0.8, tl.col = 'black',
               upper.col = colorRampPalette(c('darkgray', 'darkorange'))(10),
               lower.col = colorRampPalette(c('darkgray', 'darkorange'))(10)
)
# REMARKS Because variables 'sqft_above' and 'sqft_living' are in strong correlation (0,88) - the best solution will be
# to cancle one of them. 
# TO CONSIDERATION: pairs of variables: 'sqft_basement'&'sqft_above' or 'sqft_living'&'is_basement'



# ___03_FINAL DATASET -----------------------------------------------------
# 03_01 proper dataset ----
dim(df_house) ## 21613    27
colnames(df_house) # all columns
  # [1] "id"            "date"          "price"         "bedrooms"      "bathrooms"     "sqft_living"   "sqft_lot"      "floors"        "waterfront"   
  # [10] "view"          "condition"     "grade"         "sqft_above"    "sqft_basement" "yr_built"      "yr_renovated"  "zipcode"       "lat"          
  # [19] "long"          "sqft_living15" "sqft_lot15"    "date_pro"      "date_Ym"       "rm"            "house_age"     "date_Y"        "is_basement" 
colnames(df_house[c(3:14, 25, 27)]) # columns selected for further analysis
  # [1] "price"         "bedrooms"      "bathrooms"     "sqft_living"   "sqft_lot"      "floors"        "waterfront"    "view"          "condition"    
  # [10] "grade"         "sqft_above"    "sqft_basement" "rm"            "house_age"     "is_basement" 

# removed: 3 because of duplicated id; 3 because of no bathrooms; 7 because of no bedrooms, no bathrooms and no basement
df_house[df_house$rm == 1, ]  
dim(df_house[df_house$rm == 1, ]) ## 13 27

df_house_an <- df_house[df_house$rm == 0, c(3:14, 25, 27)]
dim(df_house_an) ##  21600    14
str(df_house_an)
  # 'data.frame':	21600 obs. of  14 variables:
  # $ price        : num  221900 538000 180000 604000 510000 ...
  # $ bedrooms     : num  3 3 2 4 3 4 3 3 3 3 ...
  # $ bathrooms    : num  1 2.25 1 3 2 4.5 2.25 1.5 1 2.5 ...
  # $ sqft_living  : int  1180 2570 770 1960 1680 5420 1715 1060 1780 1890 ...
  # $ sqft_lot     : int  5650 7242 10000 5000 8080 101930 6819 9711 7470 6560 ...
  # $ floors       : num  1 2 1 1 1 1 2 1 1 2 ...
  # $ waterfront   : int  0 0 0 0 0 0 0 0 0 0 ...
  # $ view         : int  0 0 0 0 0 0 0 0 0 0 ...
  # $ condition    : int  3 3 3 5 3 3 3 3 3 3 ...
  # $ grade        : int  7 7 6 7 8 11 7 7 7 7 ...
  # $ sqft_above   : int  1180 2170 770 1050 1680 3890 1715 1060 1050 1890 ...
  # $ sqft_basement: int  0 400 0 910 0 1530 0 0 730 0 ...
  # $ house_age    : num  59 63 82 49 28 13 19 52 55 12 ...
  # $ is_basement  : num  0 1 0 1 0 1 0 0 1 0 ...

# 03_02 correlation ----
corrplot.mixed(cor(df_house_an), tl.cex = 0.6, tl.col = 'black',
               upper.col = colorRampPalette(c('darkgray', 'darkorange'))(10),
               lower.col = colorRampPalette(c('darkgray', 'darkorange'))(10)
)

# 03_04 selection of variable with reggresion ----
# checking pairs: ‘sqft_basement’ + ‘sqft_above’ or ’is_basement’ + ‘sqft_living
model_lm_1 <- lm(price ~ . - sqft_living - is_basement , data = df_house_an)
summary(model_lm_1)
  # Call:
  # lm(formula = price ~ . - sqft_living - is_basement, data = df_house_an)
  # 
  # Residuals:
  #      Min       1Q   Median       3Q      Max 
  # -1325826  -109480    -9556    89948  4232797 
  # 
  # Coefficients:
  #                   Estimate Std. Error t value Pr(>|t|)    
  #   (Intercept)   -9.988e+05  1.740e+04 -57.389  < 2e-16 ***
  #   bedrooms      -4.254e+04  2.113e+03 -20.131  < 2e-16 ***
  #   bathrooms      4.825e+04  3.479e+03  13.868  < 2e-16 ***
  #   sqft_lot      -2.608e-01  3.659e-02  -7.129 1.04e-12 ***
  #   floors         2.450e+04  3.735e+03   6.560 5.50e-11 ***
  #   waterfront     5.749e+05  1.861e+04  30.900  < 2e-16 ***
  #   view           4.483e+04  2.255e+03  19.878  < 2e-16 ***
  #   condition      1.831e+04  2.463e+03   7.435 1.09e-13 ***
  #   grade          1.247e+05  2.171e+03  57.447  < 2e-16 ***
  #   sqft_above     1.716e+02  3.522e+00  48.730  < 2e-16 ***
  #   sqft_basement  1.731e+02  4.620e+00  37.474  < 2e-16 ***
  #   house_age      3.644e+03  6.715e+01  54.264  < 2e-16 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 216200 on 21588 degrees of freedom
  # Multiple R-squared:  0.6535,	Adjusted R-squared:  0.6533 
  # F-statistic:  3701 on 11 and 21588 DF,  p-value: < 2.2e-16

model_lm_2 <- lm(price ~ . - sqft_above  - sqft_basement , data = df_house_an)
summary(model_lm_2)
# Call:
#   lm(formula = price ~ . - sqft_above - sqft_basement, data = df_house_an)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1327809  -109406    -9334    89679  4240270 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -1.000e+06  1.739e+04 -57.507  < 2e-16 ***
#   bedrooms    -4.252e+04  2.113e+03 -20.118  < 2e-16 ***
#   bathrooms    4.736e+04  3.506e+03  13.507  < 2e-16 ***
#   sqft_living  1.715e+02  3.321e+00  51.656  < 2e-16 ***
#   sqft_lot    -2.571e-01  3.653e-02  -7.037 2.02e-12 ***
#   floors       2.567e+04  3.617e+03   7.099 1.30e-12 ***
#   waterfront   5.757e+05  1.861e+04  30.938  < 2e-16 ***
#   view         4.462e+04  2.238e+03  19.934  < 2e-16 ***
#   condition    1.831e+04  2.458e+03   7.450 9.70e-14 ***
#   grade        1.248e+05  2.147e+03  58.112  < 2e-16 ***
#   house_age    3.633e+03  6.737e+01  53.917  < 2e-16 ***
#   is_basement  4.976e+03  3.445e+03   1.444    0.149    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 216200 on 21588 degrees of freedom
# Multiple R-squared:  0.6535,	Adjusted R-squared:  0.6533 
# F-statistic:  3702 on 11 and 21588 DF,  p-value: < 2.2e-16


stats::AIC(model_lm_1, model_lm_2)
  #            df      AIC
  # model_lm_1 13 591973.5
  # model_lm_2 13 591971.5

stats::BIC(model_lm_1, model_lm_2)
  #            df      BIC
  # model_lm_1 13 592077.2
  # model_lm_2 13 592075.2

# REMARKS better is model_lm_2 with ’is_basement’ + ‘sqft_living however it's a minimal lead.
# ATTENTION In the model_lm_2 the variable 'is_basement' seems be not important

# models for procedur of stepwise selection by AIC 
model_start <- lm(price ~ 1 , data = df_house_an) 
summary(model_start)
  # Call:
  #   lm(formula = price ~ 1, data = df_house_an)
  # 
  # Residuals:
  #     Min      1Q  Median      3Q     Max 
  # -462127 -218127  -90127  104873 7159873 
  # 
  # Coefficients:
  #             Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)   540128       2498   216.2   <2e-16 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 367100 on 21599 degrees of freedom

model_full <- lm(price ~ . , data = df_house_an)
summary(model_full)
# Call:
#   lm(formula = price ~ ., data = df_house_an)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1332961  -109484    -9466    89324  4249178 
# 
# Coefficients: (1 not defined because of singularities)
#                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   -9.992e+05  1.740e+04 -57.411  < 2e-16 ***
#   bedrooms      -4.247e+04  2.114e+03 -20.096  < 2e-16 ***
#   bathrooms      4.740e+04  3.507e+03  13.518  < 2e-16 ***
#   sqft_living    1.642e+02  6.592e+00  24.903  < 2e-16 ***
#   sqft_lot      -2.598e-01  3.659e-02  -7.100 1.29e-12 ***
#   floors         2.447e+04  3.735e+03   6.551 5.84e-11 ***
#   waterfront     5.755e+05  1.861e+04  30.930  < 2e-16 ***
#   view           4.499e+04  2.257e+03  19.936  < 2e-16 ***
#   condition      1.858e+04  2.467e+03   7.533 5.17e-14 ***
#   grade          1.243e+05  2.184e+03  56.884  < 2e-16 ***
#   sqft_above     9.373e+00  7.241e+00   1.294   0.1956    
#   sqft_basement         NA         NA      NA       NA    
#   house_age      3.633e+03  6.737e+01  53.924  < 2e-16 ***
#   is_basement    1.070e+04  5.604e+03   1.909   0.0563 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 216200 on 21587 degrees of freedom
# Multiple R-squared:  0.6535,	Adjusted R-squared:  0.6533 
# F-statistic:  3393 on 12 and 21587 DF,  p-value: < 2.2e-16

# REMARKS NA for 'sqft_basement' shows because of dependence with 'sqft_above' and 'sqft_living' 
# However variable 'sqft_living' has the strongest influence for 'price'; it seems also that
# 'sqft_above' and 'is_basement' probabaly can be cancled.

# __backward stepwise model selection by AIC ----
reg_final_b <- MASS::stepAIC(model_full, direction = 'backward', steps = 20) 

reg_final_b$anova
  # Stepwise Model Path 
  # Analysis of Deviance Table
  # 
  # Initial Model:
  #   price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + 
  #   waterfront + view + condition + grade + sqft_above + sqft_basement + 
  #   house_age + is_basement
  # 
  # Final Model:
  #   price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + 
  #   waterfront + view + condition + grade + house_age + is_basement
  # 
  # 
  #              Step Df    Deviance Resid. Df   Resid. Dev      AIC
  # 1                                    21587 1.008639e+15 530671.7
  # 2 - sqft_basement  0           0     21587 1.008639e+15 530671.7
  # 3    - sqft_above  1 78275905693     21588 1.008717e+15 530671.4

# __forward stepwise model selection by AIC ----
reg_final_f <- MASS::stepAIC(model_start, 
                     scope = list(lower = model_start, upper = model_full), 
                     direction = 'forward') 

reg_final_f$anova
  # Stepwise Model Path 
  # Analysis of Deviance Table
  # 
  # Initial Model:
  #   price ~ 1
  # 
  # Final Model:
  #   price ~ sqft_living + view + grade + house_age + waterfront + 
  #   bedrooms + bathrooms + sqft_lot + condition + floors + is_basement
  # 
  # 
  #             Step Df     Deviance Resid. Df   Resid. Dev      AIC
  # 1                                    21599 2.911277e+15 553543.5
  # 2  + sqft_living  1 1.434346e+15     21598 1.476931e+15 538887.2
  # 3         + view  1 1.236904e+14     21597 1.353241e+15 536999.9
  # 4        + grade  1 1.090752e+14     21596 1.244166e+15 535186.7
  # 5    + house_age  1 1.540512e+14     21595 1.090114e+15 532333.6
  # 6   + waterfront  1 4.814445e+13     21594 1.041970e+15 531359.9
  # 7     + bedrooms  1 1.265112e+13     21593 1.029319e+15 531098.1
  # 8    + bathrooms  1 1.345831e+13     21592 1.015860e+15 530815.8
  # 9     + sqft_lot  1 2.711210e+12     21591 1.013149e+15 530760.1
  # 10   + condition  1 2.040291e+12     21590 1.011109e+15 530718.5
  # 11      + floors  1 2.294252e+12     21589 1.008815e+15 530671.4
  # 12 + is_basement  1 9.748537e+10     21588 1.008717e+15 530671.4

# __both direction stepwise model selection by AIC ----
reg_final_b <- MASS::stepAIC(model_start, 
                     scope = list(lower = model_start, upper = model_full),
                     direction = 'both')

reg_final_b$anova
  # Stepwise Model Path 
  # Analysis of Deviance Table
  # 
  # Initial Model:
  #   price ~ 1
  # 
  # Final Model:
  #   price ~ sqft_living + view + grade + house_age + waterfront + 
  #   bedrooms + bathrooms + sqft_lot + condition + floors + is_basement
  # 
  # 
  #             Step Df     Deviance Resid. Df   Resid. Dev      AIC
  # 1                                    21599 2.911277e+15 553543.5
  # 2  + sqft_living  1 1.434346e+15     21598 1.476931e+15 538887.2
  # 3         + view  1 1.236904e+14     21597 1.353241e+15 536999.9
  # 4        + grade  1 1.090752e+14     21596 1.244166e+15 535186.7
  # 5    + house_age  1 1.540512e+14     21595 1.090114e+15 532333.6
  # 6   + waterfront  1 4.814445e+13     21594 1.041970e+15 531359.9
  # 7     + bedrooms  1 1.265112e+13     21593 1.029319e+15 531098.1
  # 8    + bathrooms  1 1.345831e+13     21592 1.015860e+15 530815.8
  # 9     + sqft_lot  1 2.711210e+12     21591 1.013149e+15 530760.1
  # 10   + condition  1 2.040291e+12     21590 1.011109e+15 530718.5
  # 11      + floors  1 2.294252e+12     21589 1.008815e+15 530671.4
  # 12 + is_basement  1 9.748537e+10     21588 1.008717e+15 530671.4


# 03_05 final dataset ----
colnames(df_house_an)
  # [1] "price"         "bedrooms"      "bathrooms"     "sqft_living"   "sqft_lot"      "floors"        "waterfront"    "view"          "condition"    
  # [10] "grade"         "sqft_above"    "sqft_basement" "house_age"     "is_basement"  
df_house_fin <- df_house_an[ , c(1:10, 13:14)]

dim(df_house_fin) ## 21600    12
colnames(df_house_fin)
  # [1] "price"       "bedrooms"    "bathrooms"   "sqft_living" "sqft_lot"    "floors"      "waterfront"  "view"        "condition"  
  # [10] "grade"       "house_age"   "is_basement"

# correlation in final dataset
corrplot.mixed(cor(df_house_fin), tl.cex = 0.6, tl.col = 'black',
               upper.col = colorRampPalette(c('darkgray', 'darkorange'))(10),
               lower.col = colorRampPalette(c('darkgray', 'darkorange'))(10)
)

# last check of final detaset
anova(lm(price ~ . , data = df_house_fin))
  # Analysis of Variance Table
  # 
  # Response: price
  #                  Df     Sum Sq    Mean Sq    F value Pr(>F)    
  #   bedrooms        1 2.9084e+14 2.9084e+14  6224.2996 <2e-16 ***
  #   bathrooms       1 5.2038e+14 5.2038e+14 11136.8262 <2e-16 ***
  #   sqft_living     1 6.6845e+14 6.6845e+14 14305.8916 <2e-16 ***
  #   sqft_lot        1 5.3058e+12 5.3058e+12   113.5522 <2e-16 ***
  #   floors          1 2.2603e+10 2.2603e+10     0.4837 0.4867    
  #   waterfront      1 9.9192e+13 9.9192e+13  2122.8617 <2e-16 ***
  #   view            1 5.2435e+13 5.2435e+13  1122.1762 <2e-16 ***
  #   condition       1 1.8762e+13 1.8762e+13   401.5381 <2e-16 ***
  #   grade           1 1.0743e+14 1.0743e+14  2299.1793 <2e-16 ***
  #   house_age       1 1.3965e+14 1.3965e+14  2988.6209 <2e-16 ***
  #   is_basement     1 9.7485e+10 9.7485e+10     2.0863 0.1486    
  #   Residuals   21588 1.0087e+15 4.6726e+10                      
  # ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# ATTENTION According to the analysis-of-variance for the final data probably it should be also considered
# model without varaibles 'floors' and 'is_basement'.


# 03_05 train-test ----
split_house <- sample.split(df_house_fin, SplitRatio = 0.8)
train_house <- subset(df_house_fin, split_house == TRUE)
dim(train_house) ## 16200    12

test_house <- subset(df_house_fin, split_house == FALSE)
dim(test_house) ## 5400   12


# ___04_MODEL: REGRESSION -------------------------------------------------
# 04_01 model ----
model_lm <- lm(formula = price ~ . , data = train_house)

summary(model_lm)
  # Call:
  #   lm(formula = price ~ ., data = train_house)
  # 
  # Residuals:
  #   Min       1Q   Median       3Q      Max 
  # -1336518  -110813    -9108    90858  4163397 
  # 
  # Coefficients:
  #                 Estimate Std. Error t value Pr(>|t|)    
  #   (Intercept) -1.008e+06  2.040e+04 -49.411  < 2e-16 ***
  #   bedrooms    -4.335e+04  2.477e+03 -17.501  < 2e-16 ***
  #   bathrooms    5.023e+04  4.118e+03  12.197  < 2e-16 ***
  #   sqft_living  1.778e+02  3.891e+00  45.694  < 2e-16 ***
  #   sqft_lot    -3.245e-01  4.608e-02  -7.044 1.95e-12 ***
  #   floors       2.441e+04  4.276e+03   5.709 1.15e-08 ***
  #   waterfront   5.711e+05  2.187e+04  26.107  < 2e-16 ***
  #   view         4.664e+04  2.656e+03  17.560  < 2e-16 ***
  #   condition    1.931e+04  2.906e+03   6.643 3.16e-11 ***
  #   grade        1.232e+05  2.533e+03  48.644  < 2e-16 ***
  #   house_age    3.715e+03  7.905e+01  47.003  < 2e-16 ***
  #   is_basement  2.402e+03  4.058e+03   0.592    0.554    
  # ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 220200 on 16188 degrees of freedom
  # Multiple R-squared:  0.6549,	Adjusted R-squared:  0.6547 
  # F-statistic:  2793 on 11 and 16188 DF,  p-value: < 2.2e-16

summary(model_lm)$adj.r.squared  ## 0.6546991


# 04_02 prediction ----
# test set
y_pred_lm <- predict(model_lm, newdata = test_house)
res_lm <- test_house$price - y_pred_lm

summary(y_pred_lm)
  #     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # -245341  334718  474505  535341  668198 3252922 
length(y_pred_lm[y_pred_lm <= 0]) ## 12

# train set
y_pred_lm_0 <- predict(model_lm, newdata = train_house)
res_lm_0 <- train_house$price - y_pred_lm_0

summary(y_pred_lm_0)
  #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # -355008  335926  479927  542027  676694 3536603
length(y_pred_lm_0[y_pred_lm_0 <= 0]) ## 34

# ATTENTION model is giving value of 'price' less then 0

summary(test_house$price)
  #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 82000  320000  452000  534430  635000 4489000

# 04_03 model's test ----
# sufficiency of the functional part of the model: scatter plots of residuals versus predictors
par(mfcol = c(2, 1))
# test set
plot(res_lm ~ y_pred_lm,
     pch = 4, cex = 0.8, main = 'Multiple Linear Regression: Residuals vs. predictors', 
     xlab = 'predictors', ylab = 'residuals')
abline(h = 0, col = 'red')

# train set
plot(res_lm_0 ~ y_pred_lm_0,
     pch = 4, cex = 0.8, main = 'Multiple Linear Regression: Residuals vs. real', 
     xlab = 'real', ylab = 'residuals')
abline(h = 0, col = 'red')
par(mfcol = c(par_def$mfcol))

# normality of errors
# test set
summary(res_lm)
  #       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
  # -1328918.8  -107563.0    -8012.0     -910.9    91579.5  2751291.7 
boxplot(res_lm, horizontal = TRUE, col = 'darkorange', main = 'Multiple Linear Regression - residuals')

hist(res_lm, breaks = 100, col = 'darkorange',
     main = 'Multiple Linear Regression: Histogram of residual (test)')
tseries::jarque.bera.test(res_lm) # residuals don't have a normal distribution
qqnorm(res_lm, cex = 0.8, main = 'QQplot - Multiple Linear Regression (test)')  #-# checed
qqline(res_lm, col = 'red', lwd = 2)
#
qqplot(y_pred_lm, test_house$price)
abline(a = 0, b = 1, col = 'red')

# train set
summary(res_lm_0)
  #     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  # -1336518  -110813    -9108        0    90858  4163397 
boxplot(res_lm_0, horizontal = TRUE, col = 'darkorange', main = 'Multiple Linear Regression - residuals')

hist(res_lm_0, breaks = 100, col = 'darkorange',
     main = 'Multiple Linear Regression: Histogram of residual (train)')
tseries::jarque.bera.test(res_lm_0) # residuals don't have a normal distribution
qqnorm(res_lm_0, cex = 0.8, main = 'QQplot - Multiple Linear Regression (train)')  #-# checed
qqline(res_lm_0, col = 'red', lwd = 2)
#
qqplot(y_pred_lm_0, test_house$price)
abline(a = 0, b = 1, col = 'red')


# ___05_MODEL: SUPPORT VECTOR REGGRESION ----------------------------------
# 05_01 model ----
model_svr <- svm(formula = price ~ . , data = train_house,
                 type = 'eps-regression', kernel = 'radial')

summary(model_svr)
  # Call:
  # svm(formula = price ~ ., data = train_house, type = "eps-regression", kernel = "radial")
  # 
  # Parameters:
  #   SVM-Type:  eps-regression 
  # SVM-Kernel:  radial 
  #       cost:  1 
  #      gamma:  0.09090909 
  #    epsilon:  0.1 
  # 
  # Number of Support Vectors:  12451

# 05_02 prediction ----
# test set
y_pred_svr <- predict(model_svr, newdata = test_house)
res_svr <- test_house$price - y_pred_svr

summary(y_pred_svr)
  #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 125568  341104  445071  517016  603632 2913415

# train set
y_pred_svr_0 <- predict(model_svr, newdata = train_house)
res_svr_0 <- train_house$price - y_pred_svr_0

# 05_03 model's test ----
# sufficiency of the functional part of the model: scatter plots of residuals versus predictors
par(mfcol = c(2, 1))
# test set
plot(res_svr ~ y_pred_svr,
     pch = 4, cex = 0.8, main = 'SVR: Residuals vs. predictors', 
     xlab = 'predictors', ylab = 'residuals')
abline(h = 0, col = 'red')

# train set
plot(res_svr_0 ~ y_pred_svr_0,
     pch = 4, cex = 0.8, main = 'SVR: Residuals vs. real', 
     xlab = 'real', ylab = 'residuals')
abline(h = 0, col = 'red')
par(mfcol = c(par_def$mfcol))

# normality of errors
# test set
summary(res_svr)
  #      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  # -1222037   -79060     1188    17414    90857  2805200 
boxplot(res_svr, horizontal = TRUE, col = 'darkorange', main = 'SVR - residuals')

hist(res_svr, breaks = 100, col = 'darkorange',
     main = 'SVR: Histogram of residual (test)')
tseries::jarque.bera.test(res_svr) # residuals don't have a normal distribution
qqnorm(res_svr, cex = 0.8, main = 'QQplot - SVR (test)')  #-# checed
qqline(res_svr, col = 'red', lwd = 2)
#
qqplot(y_pred_svr, test_house$price)
abline(a = 0, b = 1, col = 'red')

# train set
summary(res_svr_0)
  #     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  # -1315647   -75613    -2735    19048    82725  6193582 
boxplot(res_svr_0, horizontal = TRUE, col = 'darkorange', main = 'SVR - residuals')

hist(res_svr_0, breaks = 100, col = 'darkorange',
     main = 'SVR: Histogram of residual (train)')
tseries::jarque.bera.test(res_svr_0) # residuals don't have a normal distribution
qqnorm(res_svr_0, cex = 0.8, main = 'QQplot - SVR (train)')  #-# checed
qqline(res_svr_0, col = 'red', lwd = 2)
#
qqplot(y_pred_svr_0, test_house$price)
abline(a = 0, b = 1, col = 'red')


# ___06_MODEL: RANDOM FOREST ----------------------------------------------
# 06_01 model ----
model_rf <- randomForest(x = train_house[-1], y = train_house$price, ntree = 100)

print(model_rf)
  # Call:
  #   randomForest(x = train_house[-1], y = train_house$price, ntree = 100) 
  #                Type of random forest: regression
  #                      Number of trees: 100
  # No. of variables tried at each split: 4
  # 
  #            Mean of squared residuals: 2161741059
  #                      % Var explained: 98.46

model_rf$mse[which.max(model_rf$rsq)] ## 37469476971


# 06_02 prediction ----
# test set
y_pred_rf <- predict(model_rf, newdata = test_house)
res_rf <- test_house$price - y_pred_rf

summary(y_pred_rf)
  #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 108125  323411  453416  535260  640105 4361218 

# train set
y_pred_rf_0 <- predict(model_rf, newdata = train_house)
res_rf_0 <- train_house$price - y_pred_rf_0

summary(y_pred_rf_0)
  #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 89954  323570  450396  541968  647730 6424563 

# 06_03 model's test ----
# sufficiency of the functional part of the model: scatter plots of residuals versus predictors
# test set
par(mfcol = c(2, 1))
plot(res_rf ~ y_pred_rf,
     pch = 4, cex = 0.8, main = 'Random Forest: Residuals vs. predictors', 
     xlab = 'predictors', ylab = 'residuals')
abline(h = 0, col = 'red')

# train set
plot(res_rf_0 ~ y_pred_rf_0,
     pch = 4, cex = 0.8, main = 'Random Forest: Residuals vs. real', 
     xlab = 'real', ylab = 'residuals')
abline(h = 0, col = 'red')
par(mfcol = c(par_def$mfcol))

# normality of errors - test data
summary(res_rf)
  #       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
  # -2081218.2    -3518.1     -142.9     -829.5     2451.5  1284820.9 
boxplot(res_rf, horizontal = TRUE, col = 'darkorange', main = 'Random Forest - residuals')

hist(res_rf, breaks = 100, col = 'darkorange',
     main = 'Random Forest: Histogram of residual (test)')
tseries::jarque.bera.test(res_rf) # residuals don't have a normal distribution
qqnorm(res_rf, cex = 0.8, main = 'QQplot - Random Forest (test)')  #-# checed
qqline(res_rf, col = 'red', lwd = 2)
#
qqplot(y_pred_rf, test_house$price)
abline(a = 0, b = 1, col = 'red')

# normality of errors - train data
summary(res_rf_0)
  #      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
  # -785597.7   -1981.9     -95.7      58.7    1460.7 1275437.5 
boxplot(res_rf_0, horizontal = TRUE, col = 'darkorange', main = 'Random Forest - residuals')

hist(res_rf_0, breaks = 100, col = 'darkorange',
     main = 'Random Forest: Histogram of residual (train)')
tseries::jarque.bera.test(res_rf_0) # residuals don't have a normal distribution
qqnorm(res_rf_0, cex = 0.8, main = 'QQplot - Random Forest (train)')  #-# checed
qqline(res_rf_0, col = 'red', lwd = 2)
#
qqplot(y_pred_rf_0, test_house$price)
abline(a = 0, b = 1, col = 'red')


# ___07_COMPARISION OF MODELS ---------------------------------------------
# 07_01 stat 
(n_data <- nrow(test_house))     # number of items in test set 5400
(p_data <- ncol(test_house) - 1) # number of independent variable 11

#function to calculate quality metrics for models
tab_stat <- function(y_pred, y_real, n_data, p_data){
  tab_stat_x <- NULL
  # Mean Squared Error
  tab_stat_x$mse <- mse(y_pred, y_real)
  # Root Mean Squared Error
  tab_stat_x$rmse <- rmse(y_pred, y_real)
  # Relative Squared Error
  tab_stat_x$rse <- sqrt(sse(y_real, y_pred)/(n_data - 2))
  # R-squared (R2) -  coefficient of determination
  R2_x <-  1 - rse(y_real, y_pred) #sse(y_real, y_pred) / sse(y_real, mean(y_real))
  tab_stat_x$R2 <- R2_x
  # adjusted R-squared 
  adjR2_x <- 1 - (1 - R2_x) * (n_data- 1)/(n_data - p_data - 1)
  tab_stat_x$adjR2 <- adjR2_x
  # return
  as.data.frame(tab_stat_x)
}

# metrics table for each model (test set)
tab_lm <- tab_stat(y_pred_lm, test_house$price, n_data, p_data)
tab_svr <- tab_stat(y_pred_svr, test_house$price, n_data, p_data)
tab_rf <- tab_stat(y_pred_rf, test_house$price, n_data, p_data)

(tab_all <- rbind(tab_lm, tab_svr, tab_rf))
rownames(tab_all) <- c('tab_lm', 'tab_svr', 'tab_rf')

colnames(tab_all)
  # "mse"   "rmse"  "rse"   "R2"    "adjR2"

tab_all
  #                 mse      rmse       rse        R2     adjR2
  # tab_lm  41566486128 203878.61 203916.37 0.6471001 0.6463796
  # tab_svr 35577424719 188619.79 188654.73 0.6979473 0.6973306
  # tab_rf   1812389856  42572.17  42580.06 0.9846128 0.9845814

# checking the diffrence between R2 and adj R2
(tab_all$R2 - tab_all$adjR2) / tab_all$R2 
  # 1.113384e-03 8.835379e-04 3.190505e-05
# diffrence between R2 and adjR2 negligibly small = > we can use only R2

# summary of metrics table
summary(tab_all)
  #       mse                 rmse             rse               R2             adjR2       
  # Min.   :1.812e+09   Min.   : 42572   Min.   : 42580   Min.   :0.6471   Min.   :0.6464  
  # 1st Qu.:1.869e+10   1st Qu.:115596   1st Qu.:115617   1st Qu.:0.6725   1st Qu.:0.6719  
  # Median :3.558e+10   Median :188620   Median :188655   Median :0.6979   Median :0.6973  
  # Mean   :2.632e+10   Mean   :145024   Mean   :145050   Mean   :0.7766   Mean   :0.7761  
  # 3rd Qu.:3.857e+10   3rd Qu.:196249   3rd Qu.:196286   3rd Qu.:0.8413   3rd Qu.:0.8410  
  # Max.   :4.157e+10   Max.   :203879   Max.   :203916   Max.   :0.9846   Max.   :0.9846

summary(test_house$price)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 82000  320000  452000  534430  635000 4489000

# 07_02 metrics plot ----
# plot of metrics value - to comparision was used RSE, RMSE and R2
plot(tab_all[ , 2]/10^5, type = 'b', col = 'darkblue', lwd = 2,
     las = 2, xaxt = 'n', ylim = c(-0.5, 4),
     main = 'Plot of quality metrics for all models',
     ylab = 'metrics value', xlab = 'model')
points(1:3, tab_all[ , 4], type = 'b', col = 'darkorange', lwd = 2, pch = 2)
points(1:3, tab_all[ , 3]/min(summary(test_house$price)), type = 'b', col = 'chartreuse', 
       lwd = 2, pch = 0, lty = 2)
points(1:3, tab_all[ , 3]/max(summary(test_house$price)), type = 'b', col = 'chartreuse', 
       lwd = 2, pch = 0, lty = 2)
axis(1, at = seq(1, 3, 1), labels = c('Multiple Linear Regression', 'SVR', 'Random Forest'))
legend('topleft', legend = c('RSE[10^-6]', 'R2', 'RMSE [10^-5]'),
       col = c('chartreuse', 'darkorange', 'darkblue'),
       pch = c(0, 2, 1), lwd = 2, lty = c(2, 1, 1),
       box.lty = 0, cex = 0.9, horiz = TRUE)
grid()
abline(h = 0, col = 'black')

# REMARKS
# Lower values of RMSE indicate better fit.
# The smaller diffrence between RSE/min and RSE/max the better model is.
# The greater R2 is the better the model fits given data.

# CONCLUSION Random forest is the best.

# 07_03 plots prediction~real ----
par(mfcol = c(3, 1))
plot(y_pred_lm ~ test_house$price, col = 'chartreuse', ylim = c(0, 5350000), xlim = c(0, 5350000), cex = 0.8,
     main = 'Multiple Linear Regression', xlab = 'predictors', ylab = 'real', las = 1, cex.axis = 0.75)
abline(a = 0, b = 1, col = 'darkgrey')
plot(y_pred_svr ~ test_house$price, col = 'darkorange', ylim = c(0, 5350000), xlim = c(0, 5350000), cex = 0.8,
     main = 'SVR', xlab = 'predictors', ylab = 'real', las = 1, cex.axis = 0.75)
abline(a = 0, b = 1, col = 'darkgrey')
plot(y_pred_rf ~ test_house$price, col = 'blue3', ylim = c(0, 5350000), xlim = c(0, 5350000), cex = 0.8,
     main = 'Random Forest', xlab = 'predictors', ylab = 'real', las = 1, cex.axis = 0.75)
abline(a = 0, b = 1, col = 'darkgrey')
par(mfcol = c(par_def$mfcol))

# Random forest have good precison of prediction - the relation predicted value to the real lies on diagonal
# that means that the error of prediction is small.

