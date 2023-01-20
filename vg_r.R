library('dplyr')
library('ggplot2')
library('tidyverse')

#1. import data
datavg <- read.csv('vgsales.csv')
# 2, Mengetahui sifat datanya
summary(datavg)
str(datavg)
head(datavg)

# mengetahui banyak data yg berbeda
distinct(datavg, Publisher)
# mengetahui missing value
sum(is.na(datavg))

# Menslicing sesuai dengan keuntungan terbesar North America
TopSales_NA <- arrange(datavg, desc(NA_Sales))
TopSales_NA <- TopSales_NA %>%
                select(Name, Platform, Year, Genre, Publisher, NA_Sales) %>%
                head(n = 10)

# Menslicing sesuai dengan keuntungan terbesar Europe
TopSales_EU <- arrange(datavg, desc(EU_Sales))
TopSales_EU <- TopSales_EU %>%
  select(Name, Platform, Year, Genre, Publisher, EU_Sales) %>%
  head(n = 10)

# Menslicing sesuai dengan keuntungan terbesar Japan
TopSales_JP <- arrange(datavg, desc(JP_Sales))
TopSales_JP <- TopSales_JP %>%
  select(Name, Platform, Year, Genre, Publisher, JP_Sales) %>%
  head(n = 10)

# Menslicing sesuai dengan keuntungan terbesar Other
TopSales_Other <- arrange(datavg, desc(Other_Sales))
TopSales_Other <- TopSales_Other %>%
  select(Name, Platform, Year, Genre, Publisher, Other_Sales) %>%
  head(n = 10)

# Menslicing sesuai dengan keuntungan terbesar Global
TopSales_Global <- arrange(datavg, desc(Global_Sales))
TopSales_Global <- TopSales_Global %>%
  select(Name, Platform, Year, Genre, Publisher, Global_Sales) %>%
  head(n = 10)

# Menghitung jumlah game berdasarkan genre
countgame_bygenre <- datavg %>%
  count(Genre, sort = TRUE, name = 'Jumlah')


#menghilangkan N/A dengan rata2
datavg$Year = ifelse(datavg$Year == 0,
                     ave(datavg$Year, FUN = function(x) mean(x, na.rm = TRUE)),
                     datavg$Year)

#menghitung game yg diluncurkan by year
createbyyear <- datavg %>%
                  count(Year, name = 'Jumlah') %>%
                  head(n = 39)

#menghitung game sesuai publisher
countpublisher <- datavg %>%
  count(Publisher, sort = T, name = 'Jumlah') %>%
  head(n=10)

#menghitung game sesuai platfotm
countbyplatform <- datavg %>%
  count(Platform, sort = T, name = 'Jumlah')

# Pendapatan berdasarkan Genre
tapply(datavg$Global_Sales, INDEX = datavg$Genre, FUN = sum)

datavg %>%
  group_by(Genre) %>%
  summarise(sum(Global_Sales))

# mencari total pendapatan Publisher
Total_byP <- datavg %>%
  group_by(Publisher) %>%
  summarise(NA_Sales = sum(NA_Sales),EU_Sales = sum(EU_Sales), JP_Sales = sum(JP_Sales), Other_Sales = sum(Other_Sales), Global_Sales = sum(Global_Sales)) %>%
  arrange(desc(Global_Sales)) %>%
  head(n=10)

#contoh diagram pie
pie(TopSales_EU$EU_Sales, TopSales_EU$Name, main = "Urutan Game Terlari", col = rainbow(length(TopSales_EU$EU_Sales)))

ggplot(TopSales_EU, aes(x = TopSales_EU$EU_Sales, y = TopSales_EU$Name))

# Visualisasi TopSales_EU
TopSales_EU %>%
  ggplot(aes(x = reorder(Name, EU_Sales), y = EU_Sales)) + 
  geom_col(fill = '#A4A4A4', color = "darkred") +
  scale_x_discrete(labels = function(y) str_wrap(y, width = 23)) +
  theme(axis.text.y = element_text(size = 8)) +
  geom_text(aes(label = EU_Sales), nudge_y = -2.5) +
  labs(title = "10 Games Terlaris di Eropa", caption = "source: https://www.kaggle.com/datasets/gregorut/videogamesales",
       x = 'Game', y = 'Dollars ($M)') +
  coord_flip()

# Visualisasi TopSales_NA
TopSales_NA %>%
  ggplot(aes(x = reorder(Name, NA_Sales), y = NA_Sales)) + 
  geom_col(fill = 'skyblue', color = "darkred") +
  scale_x_discrete(labels = function(y) str_wrap(y, width = 23)) +
  theme(axis.text.y = element_text(size = 8)) +
  geom_text(aes(label = NA_Sales), nudge_y = -2.5) +
  labs(title = "10 Games Terlaris di Amerika Utara", caption = "source: https://www.kaggle.com/datasets/gregorut/videogamesales",
       x = 'Game', y = 'Dollars ($M)') +
  coord_flip()

# Visualisasi TopSales_JP
TopSales_JP %>%
  ggplot(aes(x = reorder(Name, JP_Sales), y = JP_Sales)) + 
  geom_col(fill = '#f24646', color = "darkred") +
  scale_x_discrete(labels = function(y) str_wrap(y, width = 27)) +
  theme(axis.text.y = element_text(size = 8)) +
  geom_text(aes(label = JP_Sales), nudge_y = -2.5) +
  labs(title = "10 Games Terlaris di Jepang", caption = "source: https://www.kaggle.com/datasets/gregorut/videogamesales",
       x = 'Game', y = 'Dollars ($M)') +
  coord_flip()

# Visualisasi TopSales_Global
TopSales_Global %>%
  ggplot(aes(x = reorder(Name, Global_Sales), y = Global_Sales)) + 
  geom_col(fill = '#31e8c3', color = "darkred") +
  scale_x_discrete(labels = function(y) str_wrap(y, width = 27)) +
  theme(axis.text.y = element_text(size = 8)) +
  geom_text(aes(label = Global_Sales), nudge_y = -4.9) +
  labs(title = "10 Games Terlaris di Dunia", caption = "source: https://www.kaggle.com/datasets/gregorut/videogamesales",
       x = 'Game', y = 'Dollars ($M)') +
  coord_flip()

# Visualisasi genre game
pie(countgame_bygenre$Jumlah,main = 'Banyak Game Berdasarkan Genre',
    labels = countgame_bygenre$Jumlah, radius = 1, col = rainbow(12), border = NA, cex = 0.7) +
    legend('right', legend = countgame_bygenre$Genre, fill = rainbow(7), cex = 0.57)

#visualisasi game/tahun
createbyyear %>%
ggplot(aes(Year, y = Jumlah, group =1), col = 'red') +
geom_point(color = "blue") +
geom_line() +
theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Jumlah Games yang Dirilis Tiap Tahunnya", caption = "source: https://www.kaggle.com/datasets/gregorut/videogamesales")

# Visualisasi Keuntungan Publisher
sales <- data.frame('NA_Sales', 'EU_Sales','JP_Sales','Other_Sales','Global_Sales')

class(sales)

Total_byP %>%
  ggplot(aes(x=Publisher)) +
  geom_col(aes(y=Global_Sales), fill = '#57f281') +
  geom_col(aes(y=NA_Sales), fill = '#ded828') +
  geom_col(aes(y=EU_Sales),fill = '#5ac8ed') +
  geom_col(aes(y=JP_Sales), fill = '#ed5a7a', alpha = 0.2) +
  theme(axis.text.x = element_text(angle = 0, size = 6.37))+ #mengatur posisi.ukuran
  scale_x_discrete(labels = function(x) str_wrap(x, width = 1))+ #mewrap hurus
  labs(title = "Publisher Dengan Pendapatan Tertinggi", caption = "source: https://www.kaggle.com/datasets/gregorut/videogamesales",
       x = 'Publisher', y = 'Pendapatan')
  