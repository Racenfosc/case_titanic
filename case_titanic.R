options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
library(dslabs)
library(dplyr)

# Obtendo os dados referentas à tragédia to titanic. Referente a classe de viagem, 
# sexo, idade, parentes, preço do ticket e sobrevivência do passageiros.

titanic <- titanic_train %>% 
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))


#O seguinte gráfico mostra a distribuição dos passeigos por Idade, agrupando entre os sexos.
titanic %>% ggplot(aes(Age, y = ..count.., col = Sex, fill = Sex))+geom_density(alpha = 0.1)
#Conclusões: a maioria dos passageiros eram homens entre 18 e 40 anos.


titanic_over40m <- titanic_train %>% select(Age, Sex) %>%
  filter(Age >= 40) %>% filter(Sex == "male")

titanic_over40f <- titanic_train %>% select(Age, Sex) %>%
  filter(Age >= 40) %>% filter(Sex == "female")

# Quantidade de passageiros homens(m) e mulheres (f) acima de 40 anos.
count(titanic_over40f)
count(titanic_over40m)


# Filtro para remover passageiros cuja idade não foi relatada. Também obtendo os valores de média
# e desvio padrão da distribuição de idade.
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

# Gráfico QQ para as idades.
titanic %>% ggplot(aes(sample = Age))+geom_qq(dparams = params)+geom_abline()

# Gráfico de barras divido entre sexo para sobreviventes e não sobreviventes.
titanic %>% ggplot(aes(Survived, fill = Sex))+geom_bar(position = position_dodge())

# Gráfico de densidades das idades, para sobreviventes e ão sobreviventes.
titanic %>% ggplot(aes(Age, y = ..count.., col = Survived, fill = Survived))+geom_density(alpha = 0.2)

# Conclusões: crianças e mulheres tiveram preferência de salvamento dado às maiores taxa de sobrevivência.
# Dentre as faixas de idade, aqueles com mais de 70 tiveram menor taxa de sobrevivência.


# Filtro para buscar passageiros cujo preço do ticket foi maior que 0.
fare <- titanic %>%
  filter(Fare != 0) %>% ggplot(aes(log2(Fare), Survived))

# Gráfico BoxPlot da distribuição do preço do ticket, agrupado por sobrevivência.
fare+geom_boxplot()+geom_jitter()
# Conclusões: passageiros com tickets mais caros tiveram maior taxa de sobrevivância.
# O preço mediano foi menor para não sobreviventes e a maioria dos que pagaram em torno de
# $8 não sobreviveram.


# Gráfico de barras da contagem de passageiros por classe, agrupados por sobrevivência.
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar()

# Gráfico de barras da contagem de passageiros por classe, agrupados por sobrevivência.
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_dodge())

# Gráfico de barras relativo à sobrevivência separados por classe.
titanic %>%
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill())
# Conclusões: haviam mais passageiros na terceira classe do que nas outras duas juntas.
# A taxa de sobrevivência foi maior para a primeira classe. A maioria dos passageiros da
# teceira classe não sobreviveu.


# Gráfico de densidade das idades, agrupado por sobrevivência e facetado por sexo e classe.
titanic %>% ggplot(aes(Age, y = ..count.., fill = Survived))+geom_density(alpha = 0.1)+facet_grid(Sex ~ Pclass)
# Conclusões: a maioria dos passageiros da terceira classe eram homens e não sobreviveu.
# A maioria das mulheres da primeira e segunda classe sobreviveu.
# Com exceção de crianças, a maioria dos homens da segunda classe não sobreviveu.
