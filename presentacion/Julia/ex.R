
dat <- data.frame(
  meal = factor(c("Breakfast", "Lunch", "Dinner", "Dinner", "Lunch")),
  total_bill = c(4.89, 7.23, 15.34, 21.23, 11.32) )


dat %>%
  ggplot(aes(x=meal)) +
  geom_bar() + theme_minimal()




dat %>%
  ggplot(aes(x=reorder(meal, total_bill), y=total_bill, fill=meal)) +
  geom_bar(stat="identity") +
  guides(fill=FALSE) +
  labs(title = "Precio medio por comida", 
       subtitle = "Ajustado para dos personas",
       x = "Tipo de comida",
       y = "Precio") + theme_minimal()


dat <- data.frame(
  diet = factor(c("Vegan", "Carnivore", "Carnivore","Carnivore","Vegan", "Vegan", "Vegan")),
  meal = factor(c("Breakfast", "Dinner", "Breakfast","Lunch", "Dinner", "Dinner", "Lunch")),
  total_bill = c(5.32, 19.22, 4.89, 7.23, 15.34, 21.23, 11.32))

# Stacked bar graph -- queremos esto?
dat %>% 
  ggplot(aes(x=meal, y=total_bill, fill=diet)) +
  geom_bar(stat="identity")


dat %>%
  ggplot(aes(x=meal, y=total_bill, fill=diet)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#72874EFF", "#FED789FF")) + 
  theme_minimal() 
