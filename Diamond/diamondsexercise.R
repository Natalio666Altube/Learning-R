library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
data()
data("diamonds")  
head(diamonds, 10)

filter(diamonds, price < '400')
filter(diamonds, price < '300')

diamonds %>%
  filter(cut=="Very Good",price <= "1000")


diamonds %>%
  filter(cut=="Premium",price <= "4000")

str(diamonds)

names(diamonds)

diamonds %>% 
  group_by(clarity) %>% 
  summarize(m = mean(price)) %>% 
  ungroup() 

diamonds %>% 
  mutate(JustOne = 1,
         Values = "something",
         Simple = TRUE)

diamonds %>% 
  mutate(price200 = price - 200)


diamonds %>% 
  mutate(price200 = price - 200,        
         price20perc = price * 0.20,    
         price20percoff = price * 0.80, 
         pricepercarat = price / carat, 
         pizza = depth ^ 2)     
diamonds.new <- 
  diamonds %>%  
  mutate(price200 = price - 200,        
         price20perc = price * .20,    
         price20percoff = price * 0.80, 
         pricepercarat = price / carat, 
         pizza = depth ^ 2)
diamonds.new

diamonds.new1 <- %>%
  diamonds %>%
  mutate(m = mean(price),     # calculates the mean price
         sd = sd(price),      # calculates standard deviation
         med = median(price))

diamonds %>% 
  summarize(avg.price = mean(price))

diamonds %>% 
  summarize(avg.price = mean(price),     # average price of all diamonds
            dbl.price = mean(price) * 2, # calculating double the average price
            random.add = 1 + 2,          # a math operation without an existing variable 
            avg.carat = mean(carat),     # average carat size of all diamonds
            stdev.price = sd(price))

# agrupando esperanza de vida promedio por año
plot(
  diamonds %>% 
    group_by(cut) %>% 
    summarise(priceX = (price)))


diamonds %>% 
  filter(table <= 55) %>% 
  group_by(color, table) %>% 
  summarise(ave_price = mean(price), max_price = max(price)) %>% 
  ggplot(aes(color, ave_price)) + 
  geom_col() +
  transition_states(table, transition_length = 3, state_length = 1) +
  ggtitle("{next_state} is a number: {is.numeric(next_state)}",
          subtitle = "{next_state} is a character: {is.character(next_state)}")


library(tidyverse)
library(gganimate)

diamonds %>% 
  filter(table <= 55) %>% 
  group_by(color, table) %>% 
  summarise(ave_price = mean(price), max_price = max(price)) %>% 
  ggplot(aes(color, ave_price)) + 
  geom_col() +
  transition_states(table, transition_length = 3, state_length = 1) +
  ggtitle("{next_state} is a number: {is.numeric(next_state)}",
          subtitle = "{next_state} is a character: {is.character(next_state)}")


library(viridis)
library(ggplot2)

gg1 <- ggplot(diamonds)+
  geom_point(aes(x = cut, y = price, color = clarity), size = 3)+
  scale_color_viridis(option = "B")+
  theme_minimal()+
  theme(legend.position = c(.8,.8))

gg2 <- ggplot(mtcars)+
  geom_violin(aes(x = factor(cyl), y = hp, fill = factor(cyl)))+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  theme(legend.position = 'none')

library(cowplot)
output <- plot_grid(gg1,gg2, labels = c('B','D'),label_size = 20)
print(output)


library(tidyverse)
library(gganimate)

diamonds %>% 
  filter(table <= 55) %>% 
  group_by(color,price) %>% 
  summarise(ave_price = mean(price), max_price = max(price)) %>% 
  ggplot(aes(color, ave_price)) + 
  geom_col() +
  transition_states(price, transition_length = 3, state_length = 1) +
  ggtitle("{next_state} is a number: {is.numeric(next_state)}",
          subtitle = "{next_state} is a character: {is.character(next_state)}")

diamonds



