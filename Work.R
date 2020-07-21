
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
  summarize(Missed = sum(Missed), Destroyed = sum(Destroyed)) %>%
  gather("Missed", "Destroyed", key = "Indicator", value = "ShotDestroyed")


total <- test4 %>%
  group_by(Virus, Medicine) %>%
  summarize(Total = sum(ShotDestroyed))

test5 <- inner_join(test4, total) %>%
  mutate(Percent = ShotDestroyed/Total)

test_7 <- as.matrix(test4)

  
  ggplot(test4, aes(x = Virus, y = ShotDestroyed)) + 
    geom_bar(stat = "identity", aes(fill = Indicator)) +
    facet_wrap(. ~ Medicine)
  
  
  test6 <- data.all %>%
    group_by(Virus) %>%
    summarize(Destroyed = sum(Destroyed), Missed = sum(Missed))
  
  test8 <- data.all %>%
    group_by(Virus) %>%
    summarize(Missed = sum(Missed), Destroyed = sum(Destroyed))
  
  
  test9 <- as.matrix.data.frame(test8)
  
#D M
B <- c(2344, 787)
R <- c(3438, 1239)
  
D <- rep("D", 2344 + 3438)
M <- rep("M", 787 + 1239)

Total <- c(D,M)

propb <- B[1]/(B[1] + B[2])
propr <- R[1]/(R[1] + R[2])

propdiff <-  propb - propr

prop <- numeric()
for(i in 1:10000){
  
  samp <- sample(Total, length(Total), replace = F)
  sampB <- samp[1:(2344 + 787)]
  sampR <- samp[(2344 + 787 + 1):length(samp)]
  
  propB <- sum(sampB == "D")/length(sampB)
  propR <- sum(sampR == "D")/length(sampR)
  
  prop[i] <- propB - propR
  
}

upper <- sum(prop >= abs(propdiff))
lower <- sum(prop <= -abs(propdiff))

pvalue <- (1 + upper + lower) / 10001





  
  

  
  
  
  
  
  