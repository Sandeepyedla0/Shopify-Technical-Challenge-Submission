#Libraries
# install.packages('googlesheets4')
library("readxl")
library(dplyr)
library(plotly)
library(googlesheets4)
library(skimr)

library(Hmisc)
setwd("~/Desktop/Shopify_challenege")
Shopify_Df <-read_xlsx('2019_Winter_Data_Science_Intern_Challenge_DataSet.xlsx')
#Shopify_Df <- read_sheet('https://docs.google.com/spreadsheets/d/16i38oonuX1y1g7C_UAmiK9GkY7cS-64DfiDMNiR41LM/edi#t#gid=0')

glimpse(Shopify_Df)

Sub_df<-select(Shopify_Df,shop_id, order_amount, total_items, payment_method,created_at)
skim(Sub_df) # Direct Average is showing  an average of 3145 for Order_amount
#skim(mtcars) %>% skimr::kable(format = "latex", booktabs = T)

summary(Shopify_Df$order_amount)

mean(Shopify_Df$order_amount)

## Visualize the Data

# library
library(ggplot2)

options(scipen=999) # removing scientfic notation
# use options!
ggplot(Shopify_Df, aes(x=shop_id, y=order_amount)) + 
  geom_point(
    color="orange",
    fill="#69b3a2",
    shape=21,
    alpha=0.9,
    size=5,
    stroke = 1
  )+ggtitle("Outlier plot for Shop_id Vs Order Amount")

#total_items

ggplot(Shopify_Df, aes(y=order_amount, x=total_items)) + 
  geom_point(
    color="orange",
    fill="#69b3a2",
    shape=21,
    alpha=0.9,
    size=5,
    stroke = 1
  )+ ggtitle("Outlier plot for total_items ")


# Building a histogram (Second Visualization for order amount)
hist(Shopify_Df$order_amount, 
     breaks = 10,
     col = "lightgreen",
     main = "Histogram of order_amount Variable",
     xlab = "order_amount")

# Building a histogram (Second Visualization for )
hist(Shopify_Df$total_items, 
     breaks = 5,
     col = "lightgreen",
     main = "Histogram of total_items Variable",
     xlab = "total_items")


## Calculating thr Average by shop_id and arranging in descending order to look at the outlier shop_id that
## affecting the total avaerage.
AOV_By_Shop <- Shopify_Df %>%
  group_by(shop_id) %>%
  summarise(averagebyshop = sum(order_amount)/sum(total_items)) %>%
  arrange(desc(averagebyshop))
AOV_By_Shop

# Ans) We can see that the Shopid 78 has 25725 as its average which is manipulating the overal avaerage.

## Idea to check on Order amount and total_items

## Grouby operation on total-items and arranging in descending order to check the outlier 
##  and arranging it in descending order to find out the number of occurances.

Iterm_Count <-Shopify_Df %>% group_by(total_items) %>% summarise(Number_of_occurances_In_DF=n())
Iterm_Count %>%arrange(desc(total_items))


# ANS) Among the total items ordered there is 2000 an outlier in the data which is affecting the order_amount dractically due to more items wrongly ordered comparitively.

## Finding out the shop_id that has the order amount of 2000
view_ShopID_2000<- subset(Shopify_Df, select = c(shop_id,total_items), Shopify_Df$total_items==2000)
view_ShopID_2000

#Ans) After summerizing it can be seen that the 2000 items are ordered from the shop_id 42. SHop_id 42 is effecting the Average Cost.


# Removing the orders with 2000

cleaned_df <-subset(Shopify_Df, Shopify_Df$total_items!=2000)
cleaned_df


Num_Orders <-Shopify_Df %>% group_by(shop_id) %>% summarise(Num_Orders_on_ShopID=n(), Items_ordered=total_items) %>% arrange(desc(Num_Orders_on_ShopID))
Num_Orders

## Ans) After cleaning the order_items and total_items with for equal distribution of data.


Orders_outliers <-cleaned_df %>% filter( order_amount>20000) 

Orders_outliers%>%group_by(shop_id) %>% summarise(Num_Orders_on_ShopID=n())

cleaned_shopify_df <-subset(cleaned_df, cleaned_df$shop_id!=78)
cleaned_shopify_df

cleaned_shopify_df['Amount_per_Order']= (cleaned_shopify_df$order_amount /cleaned_shopify_df$total_items)
cleaned_shopify_df

describe(cleaned_shopify_df$order_amount)


summary(cleaned_shopify_df$order_amount)
mean(cleaned_shopify_df$order_amount)

describe(cleaned_shopify_df$Amount_per_Order)

summary(cleaned_shopify_df$Amount_per_Order)
#Mean

MeanOfAPO<-mean(cleaned_shopify_df$Amount_per_Order)
meadianAPO<-median(cleaned_shopify_df$Amount_per_Order)

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Create the vector with numbers.
result <- cleaned_shopify_df$Amount_per_Order

# Calculate the mode using the user function.
modeAPO <- getmode(result)
print("Mode Value")
print(modeAPO)

print("meadian Value")
print(meadianAPO)

print("Mean Value")
print( MeanOfAPO)

# Ans) It can be Concluded that the mean average can be around $152
## Visualizing the data after removing the potential outliers

ggplot(cleaned_shopify_df, aes(x=shop_id, y=order_amount)) + 
  geom_point(
    color="orange",
    fill="#69b3a2",
    shape=21,
    alpha=0.9,
    size=5,
    stroke = 1
  )

ggplot(cleaned_shopify_df, aes(x=shop_id, y=total_items)) + 
  geom_point(
    color="orange",
    fill="#69b3a2",
    shape=21,
    alpha=0.9,
    size=5,
    stroke = 1
  )
# Q) Which type of payment is done more

fig <- plot_ly(Payment_Menthod_count, labels = ~payment_method, values = ~Num_Of_Payments, type = 'pie')
fig
fig <- fig %>% layout(title = 'Percentage of Payment types',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
  layout(xaxis = list(tickfont = list(size = 15)), yaxis = list(tickfont = list(size = 5)));

fig

