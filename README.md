# First_Repo
getting comfortable with Git

library(tidyverse)
#boxplot of survival rate based on embarked
titanic %>%
  filter(Embarked != "") %>%
  ggplot(aes(Survived)) +
  geom_bar(aes(fill = Survived)) +
  facet_grid(.~ Embarked) +
  labs( title = 'Amount of People Who Survived Based on Embarked Location',
        subtitle = 'Note: S = Southampton (First), C = Cherbourg (Second), & Q = Queenstown (Third)',
        caption = '0 = Perished, 1 = Survived')

