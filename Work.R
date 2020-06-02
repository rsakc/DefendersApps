
test <- data.all %>% 
  group_by(Medicine, Virus) %>%
  summarize(SumDestroyed = sum(Destroyed), SumMissed = sum(Missed)) %>%
  gather("SumDestroyed", "SumMissed", key = "A", value = "B")

ggplot(test, aes(x = Virus, y = B)) +
  geom_bar(stat = "identity", aes(fill = A))


test2 <- test %>% group_by(Medicine) %>%
 summarize(Total = sum(B))

test3 <- inner_join(test, test2)

test3 <- test3 %>%
  mutate(Percentage = B/Total)
