library(dplyr)
PATH <- "https://raw.githubusercontent.com/guru99-edu/R-Programming/master/poisons.csv"
df <- read.csv(PATH) %>%
  select(-X) %>% 
  mutate(poison = factor(poison, ordered = TRUE))
glimpse(df)
head(df)


levels(df$poison)
df %>% group_by(poison) %>% summarise(count_poison = n(), mean_time = mean(time, na.rm = T), sd_time=sd(time,na.rm = T))
summary(df)

ggplot(df, aes(x=poison, y = time, fill = poison))+ geom_boxplot() + geom_jitter(shape=12,color='steelblue', position = position_jitter(0.21))+theme_classic()

anova_one_way <- aov(time~poison, data = df)
summary(anova_one_way)

#para saber que grupo de hecho es el que mas difiere pues en el Anova no se ve claramente se aplica Tukey
#apareciendo los p value que de hecho son significativos
TukeyHSD(anova_one_way)
