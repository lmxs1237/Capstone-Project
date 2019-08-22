library(leafletCN)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(rgdal)
# demomap('china')

df_all = read_csv('./data/after_clean.csv')
nrow(df_all)  # 139772

df = df_all[df_all['OrderStatus']=='交易成功',]
#nrow(df_success) #10775
### 1 Overall Success Sales vs Province -- Map Analysis
agg = df %>% 
  group_by(Province) %>% 
  summarise(total_sale = sum(Total)) %>% 
  mutate(namevar = Province, valuevar = total_sale)

agg = agg[order(agg$total_sale, decreasing=TRUE),]
map = leaflet('china') %>% addTiles()
map = leafletGeo('china',as.data.frame(agg)) 

max(agg$total_sale)
bins = seq(0, 500, 100)
pal = colorBin("YlOrRd", domain = agg$total_sale)

rc <- colorRampPalette(colors = c("red", "white"), space = "Lab")(40)
mypal <- colorNumeric(palette = rc, domain = agg$total_scale)
previewColors(colorNumeric(palette = rc, domain = NULL), values = 0:100)

leaflet(map) %>% addTiles() %>% 
  addPolygons(stroke = TRUE,
              smoothFactor = 1,
              fillOpacity = 0.7,
              weight = 1,
              fillColor = ~mypal(agg$total_sale)
  ) %>% addLegend(
    position = 'topleft',
    pal = pal,
    values = ~agg$total_sale,
    title = 'Total Sales'
  )
agg
###-------------------------

myfun = function(year){
  agg = df %>% 
    group_by(OrderYear, Province) %>% 
    summarise(total_sale = sum(Total)) %>% 
    mutate(namevar = Province, valuevar = total_sale)
  agg = agg%>%filter(OrderYear == year)
  agg
  agg = agg[order(agg$total_sale, decreasing=TRUE),]
  
  map = leaflet('china') %>% addTiles()
  map = leafletGeo('china',as.data.frame(agg)) 
  
  max(agg$total_sale)
  bins = seq(0, 500, 100)
  pal = colorBin("YlOrRd", domain = agg$total_sale)
  
  rc <- colorRampPalette(colors = c("red", "white"), space = "Lab")(40)
  mypal <- colorNumeric(palette = rc, domain = agg$total_sale)
  previewColors(colorNumeric(palette = rc, domain = NULL), values = 0:100)
  
  leaflet(map) %>% addTiles() %>% 
    addPolygons(stroke = TRUE,
                smoothFactor = 1,
                fillOpacity = 0.7,
                weight = 1,
                fillColor = ~mypal(agg$total_sale)
    ) %>% addLegend(
      position = 'topleft',
      pal = pal,
      values = ~agg$total_sale,
      title = 'Sales'
    )
  return()
}
myfun(2015)

###--------------------------


agg = df %>% 
  group_by(OrderYear, Province) %>% 
  summarise(total_sale = sum(Total)) %>% 
  mutate(namevar = Province, valuevar = total_sale)
agg = agg%>%filter(OrderYear == 2015)
agg
agg = agg[order(agg$total_sale, decreasing=TRUE),]

map = leaflet('china') %>% addTiles()
map = leafletGeo('china',as.data.frame(agg)) 

max(agg$total_sale)
bins = seq(0, 500, 100)
pal = colorBin("YlOrRd", domain = agg$total_sale)

rc <- colorRampPalette(colors = c("red", "white"), space = "Lab")(40)
mypal <- colorNumeric(palette = rc, domain = agg$total_sale)
previewColors(colorNumeric(palette = rc, domain = NULL), values = 0:100)

leaflet(map) %>% addTiles() %>% 
  addPolygons(stroke = TRUE,
              smoothFactor = 1,
              fillOpacity = 0.7,
              weight = 1,
              fillColor = ~mypal(agg$total_sale)
  ) %>% addLegend(
    position = 'topleft',
    pal = pal,
    values = ~agg$total_sale,
    title = 'Sales 2015'
  )

agg
###--------------------------



### 2_Overall Sales Bar Plot vs. Province (top 10)

ggplot(data=agg[0:10,], aes(x=reorder(Province, -total_sale), y=total_sale, fill=Province)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=total_sale))+
  theme_minimal() +
  theme(legend.position="top", axis.text.x = element_text(angle = 60, hjust = 1,family='Kai')) 



## It is reasonable to believe that Total payment/order < 400 are retail sales

Retail_df=df[df$Total<600 & df$OrderItemNo!=0,] 
nrow(Retail_df) # 105242
unique(df$OrderStatus) # 107775 success, 31977 cancelled
table(df$OrderStatus)


library(plyr)
mu <- ddply(Retail_df, "OrderStatus", summarise, grp.mean=mean(Total))
head(mu)


ggplot(Retail_df, aes(x = Retail_df$Total, fill=Retail_df$OrderStatus, color=Retail_df$OrderStatus)) + 
  geom_histogram(aes(y=..density..),colour="black", fill="white", bins = 20) +
  labs( title= "2010-2019 Order Amount Distribution", y="Order Amount Density", x = "Order Amount") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=mu$OrderStatus),
             linetype="dashed", size=1) +
  geom_density(alpha=.2, fill="#FF6666") +
  theme(legend.position="top", legend.text = element_text(family='Kai'))


mean(Retail_df$'Total')
t = sum(Retail_df$'Total')
s = sum(Retail_df[Retail_df$'Total'<400,]$Total)
s/t
#### Amoung 105242 of retail orders, the average amount is $171.5/order. 90% are under $400/order
#### The cancelled orders (green line) follows the same distribution of success orders. Average is $173.4



head(df)
## Historical Performance:

df$MonthYear <- format(as.Date(df$OrderTime), "%Y-%m")

df_Month = df %>% 
  select(MonthYear, OrderStatus, Total)%>%
  group_by(MonthYear, OrderStatus) %>%
  summarise(TotalSale=sum(Total))
  
  
unique(df_Month$MonthYear)

ggplot(data = df, aes(x = OrderTime, y = Total))+
  geom_line(color = "#00AFBB", size = 2)
# # Plot a subset of the data
# ss <- subset(economics, date > as.Date("2006-1-1"))
# ggplot(data = ss, aes(x = date, y = pop)) + 
#   geom_line(color = "#FC4E07", size = 2)



qplot(df$Total,
      geom="histogram",
      binwidth = 5,  
      xlab = "Total Payment",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(20,50))
### 2_Yearly Sales Bar Plot vs. Province

library(plotly)
library(tidyverse)
library(htmlwidgets)
library(dplyr)

dim(df)
summary(df)
str(df)
head(df)




## Order counts vs Province

df %>% 
  count(day) %>% 
  mutate(perc = n / nrow(tips)) -> tips2

ggplot(tips2, aes(x = day, y = perc)) + geom_bar(stat = "identity")


print(regionNames('china'))


