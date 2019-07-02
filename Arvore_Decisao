# Criando arvore de decisao no R
# Usando arquivo https://www.kaggle.com/uciml/german-credit

install.packages("rpart");
install.packages("ROCR"
library("rpart");
library("ROCR");
 
## Le a base de dados
dados = read.csv("../database/german_credit_2.csv");
 
## Discretiza as variaveis
Creditability_index1 = which(dados$Creditability == 1);
Creditability_index2 = which(dados$Creditability == 0); dados$Creditability[Creditability_index1] = 'good'; dados$Creditability[Creditability_index2] = 'bad';
 
## ifelse avalia antes de executar
dados$CreditAmount = ifelse(dados$CreditAmount <= 2500, "0-2500", ifelse(dados$CreditAmount < 5000, "5000+", "2500-5000")) ;
 
## Gera indices da base treino e teste
train_index = sample(1:nrow(dados), 0.6*nrow(dados), replace = FALSE);
 
## Gera base treino e teste
train = data.frame(); train = dados[train_index,]; test = data.frame(); test = dados[-train_index,];
 
## Usa rpart para decision tree
train_tree = rpart(Creditability~., data = train);
 
## Plota a árvore de decisão
plot(train_tree);
 
## Insere a legenda dos galhos
text(train_tree, pretty = 0, cex = 0.6);
 
## Predict como funcao para trazer a probabilidade do cliente ser mau/bom
test_tree_predict = predict(train_tree, newdata = test);
 
## Predict com tipo 'classe' retorna se é bom ou mau
test_tree_predict = predict(train_tree, newdata = test, type = "class");
 
## confusion matrix
table(test_tree_predict, test$Creditability);
