options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
library(dslabs)
library(dplyr)

# Obtendo os dados referentas � trag�dia to titanic. Referente a classe de viagem, 
# sexo, idade, parentes, pre�o do ticket e sobreviv�ncia do passageiros.

titanic <- titanic_train %>% 
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))


#O seguinte gr�fico mostra a distribui��o dos passeigos por Idade, agrupando entre os sexos.
titanic %>% ggplot(aes(Age, y = ..count.., col = Sex, fill = Sex))+geom_density(alpha = 0.1)
#Conclus�es: a maioria dos passageiros eram homens entre 18 e 40 anos.


titanic_over40m <- titanic_train %>% select(Age, Sex) %>%
  filter(Age >= 40) %>% filter(Sex == "male")

titanic_over40f <- titanic_train %>% select(Age, Sex) %>%
  filter(Age >= 40) %>% filter(Sex == "female")

# Quantidade de passageiros homens(m) e mulheres (f) acima de 40 anos.
count(titanic_over40f)
count(titanic_over40m)


# Filtro para remover passageiros cuja idade n�o foi relatada. Tamb�m obtendo os valores de m�dia
# e desvio padr�o da distribui��o de idade.
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

# Gr�fico QQ para as idades.
titanic %>% ggplot(aes(sample = Age))+geom_qq(dparams = params)+geom_abline()

# Gr�fico de barras divido entre sexo para sobreviventes e n�o sobreviventes.
titanic %>% ggplot(aes(Survived, fill = Sex))+geom_bar(position = position_dodge())

# Gr�fico de densidades das idades, para sobreviventes e �o sobreviventes.
titanic %>% ggplot(aes(Age, y = ..count.., col = Survived, fill = Survived))+geom_density(alpha = 0.2)

# Conclus�es: crian�as e mulheres tiveram prefer�ncia de salvamento dado �s maiores taxa de sobreviv�ncia.
# Dentre as faixas de idade, aqueles com mais de 70 tiveram menor taxa de sobreviv�ncia.


# Filtro para buscar passageiros cujo pre�o do ticket foi maior que 0.
fare <- titanic %>%
  filter(Fare != 0) %>% ggplot(aes(log2(Fare), Survived))

# Gr�fico BoxPlot da distribui��o do pre�o do ticket, agrupado por sobreviv�ncia.
fare+geom_boxplot()+geom_jitter()
# Conclus�es: passageiros com tickets mais caros tiveram maior taxa de sobreviv�ncia.
# O pre�o mediano foi menor para n�o sobreviventes e a maioria dos que pagaram em torno de
# $8 n�o sobreviveram.


# Gr�fico de barras da contagem de passageiros por classe, agrupados por sobreviv�ncia.
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar()

# Gr�fico de barras da contagem de passageiros por classe, agrupados por sobreviv�ncia.
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_dodge())

# Gr�fico de barras relativo � sobreviv�ncia separados por classe.
titanic %>%
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill())
# Conclus�es: haviam mais passageiros na terceira classe do que nas outras duas juntas.
# A taxa de sobreviv�ncia foi maior para a primeira classe. A maioria dos passageiros da
# teceira classe n�o sobreviveu.


# Gr�fico de densidade das idades, agrupado por sobreviv�ncia e facetado por sexo e classe.
titanic %>% ggplot(aes(Age, y = ..count.., fill = Survived))+geom_density(alpha = 0.1)+facet_grid(Sex ~ Pclass)
# Conclus�es: a maioria dos passageiros da terceira classe eram homens e n�o sobreviveu.
# A maioria das mulheres da primeira e segunda classe sobreviveu.
# Com exce��o de crian�as, a maioria dos homens da segunda classe n�o sobreviveu.
