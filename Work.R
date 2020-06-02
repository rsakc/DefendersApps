
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

test4 <- data.all %>%
  group_by(Virus, Medicine) %>%
  summarize(Destroyed = sum(Destroyed), Missed = sum(Missed)) %>%
  gather("Destroyed", "Missed", key = "Indicator", value = "ShotDestroyed")


total <- test4 %>%
  group_by(Virus, Medicine) %>%
  summarize(Total = sum(ShotDestroyed))

test5 <- inner_join(test4, total) %>%
  mutate(Percent = ShotDestroyed/Total)



  
  ggplot(test4, aes(x = Virus, y = ShotDestroyed)) + 
    geom_bar(stat = "identity", aes(fill = Indicator)) +
    facet_wrap(. ~ Medicine)
  
  
  test6 <- data.all %>%
    group_by(Virus) %>%
    summarize(Destroyed = sum(Destroyed), Missed = sum(Missed))
  
  
  
  
  