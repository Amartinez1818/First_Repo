options(digits = 3)    # report 3 significant digits

library(dslabs)
library(tidyverse)
library(titanic)
titanic::titanic_train

?titanic_train

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked,
         Cabin, Ticket, Name) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
str(titanic)
head(titanic)

?labs

#density graph of ages by sex
titanic %>%
  group_by(Sex) %>%
  ggplot(aes(Age, fill=Sex)) +
  geom_density(alpha=.2, bw=.8, position="stack") +
  labs(title = 'Density Graph of Ages by Sex')

#density of ages
titanic %>%
  ggplot(aes(Age)) +
  geom_density(fill = 'Tan') +
  labs(title = "Density of Ages")

#barplot of amount of peolpe that survived/perished by sex         
titanic %>%
  filter(Survived==1)%>%
  ggplot(aes(Sex)) +
  geom_bar() +
  labs(title = "Amount that Survived by Sex")

titanic %>%
  filter(Survived==0) %>%
  ggplot(aes(Sex)) +
  geom_bar() +
  labs(title = 'Amount that Perished by Sex')

table(titanic$Survived)
table(titanic$Sex)

#density plot of people who survived/perished by age         
titanic %>%
  filter(Survived==1, !is.na(Age)) %>%
  ggplot(aes(Age, alpha=.2)) +
  geom_density(fill="red") +
  labs(title = 'Density of People who Survived by Age')

titanic %>%
  filter(Survived==0, !is.na(Age)) %>%
  ggplot(aes(Age, alpha=.2)) +
  geom_density(fill="blue") +
  labs(title = 'Density of People who Perished by Age')

head(titanic)

#boxplot of amount of people who survived base on their fare         
titanic %>%
  filter(Fare != 0) %>%
  ggplot(aes(Survived, Fare, color="yellow"))+
  geom_boxplot() +
  geom_jitter(width = .2,alpha=.2) +
  scale_y_continuous(trans = "log2") +
  labs(title = 'Amount of People that Survived based on Their Fare Price',
       caption = '0 = Perished and 1 = Survived')

#barplot of amount of people in each class         
titanic %>%
  ggplot(aes(Pclass)) +
  geom_bar() +
  labs(title = 'Amount of People in Each Class')

#barplot of amount of people who perished based on class         
titanic %>%
  filter(Survived==0) %>%
  ggplot(aes(Pclass)) +
  geom_bar() +
  labs(title = 'Amount of People Who Perished Based on Class Type')

head(titanic)

titanic %>%
  filter(Survived==0) %>%
  ggplot(aes(Age)) +
  geom_density(alpha=.2, legend=TRUE, fill = "Blue")+
  facet_grid(Sex~Pclass) +
  labs(title = 'Density of People Who Perished Based on Age, Sex, and Class Type')

#Density plot of people who survived based on age & sex in each class
titanic %>%
  filter(Survived==1) %>%
  ggplot(aes(Age)) +
  geom_density(alpha=.2, legend=TRUE, fill = 'Red')+
  facet_grid(Sex~Pclass) +
  labs(title = 'Density of People Who Survived Based on Age, Sex, and Class Type')

#boxplot of survival rate based on embarked
titanic %>%
  filter(Embarked != "") %>%
  ggplot(aes(Survived)) +
  geom_bar(aes(fill = Survived)) +
  facet_grid(.~ Embarked) +
  labs( title = 'Amount of People Who Survived Based on Embarked Location',
        subtitle = 'Note: S = Southampton (First), C = Cherbourg (Second), & Q = Queenstown (Third)',
        caption = '0 = Perished, 1 = Survived')
