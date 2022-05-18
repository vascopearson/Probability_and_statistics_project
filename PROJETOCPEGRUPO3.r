####PROJETO CPE####GRUPO 3####
##############################
##Alexandre Silva n.90004####
##Diogo Santos n.93635#######
##Leandro Duarte n.93112#####
##Vasco Pearson n.97015######

####QUESTAO 1####
##ALINEA A##
#metodo 1

set.seed(3) #definir a seed

TempoInicialPR <- Sys.time() #tempo inicial


UniformValues<-runif(10000,0,1)

PoissonRecursive <- function(u){
  k<-0
  p<-exp(-3)
  F<-p
  while (u>F) {
    p<-(3*p) / (k+1)
    F<-F+p
    k<-k+1
  }
  return(k)
}

#gerar as observações do método 1
GeneratePoissonRecursive<-unlist(lapply(UniformValues,PoissonRecursive))

TempoFinalPR <- Sys.time()

TempoTotalPR <- TempoFinalPR - TempoInicialPR


#metodo 2

set.seed(3)

TempoInicialPS <- Sys.time()

#função que realiza o algoritmo
PoissonSum<-function(kk){
  counter<-0
  m<-1
  p<-exp(-3)
  while (m >= p) {
    unif<-runif(1,0,1)
    m<-m*(unif)
    counter<-counter+1
  }
  return(counter-1)
}

#dummy vector
v<-c(rep(0,10000))

#Criação das observações do método 2
GeneratePoissonSum<-unlist(lapply(v,PoissonSum))

TempoFinalPS <- Sys.time()

TempoTotalPS <- TempoFinalPS - TempoInicialPS


#R algoritmo
TempoInicialT <- Sys.time()

set.seed(3)

#criação das observações do método 3
RPoissonAlg <- rpois(10000,3)

TempoFinalT <- Sys.time()
TempoTotalT<- TempoFinalT-TempoInicialT



#comparar os tempos
print(TempoTotalPR)
print(TempoTotalPS)
print(TempoTotalT)

#ver valores
table(GeneratePoissonRecursive)
table(GeneratePoissonSum)
table(RPoissonAlg)

#valores da média e quartis
summary(GeneratePoissonRecursive)
summary(GeneratePoissonSum)
summary(RPoissonAlg)


#P(2<X<8) para alg 1
loopcount<-3
TotalSum<-0

while(loopcount<8){
  TotalSum<-TotalSum + length(which(GeneratePoissonRecursive==loopcount))
  loopcount<-loopcount+1
}

Prob1<-TotalSum/10000

#P(2<X<8) para alg 2
loopcount<-3
TotalSum<-0

while(loopcount<8){
  TotalSum<-TotalSum + length(which(GeneratePoissonSum==loopcount))
  loopcount<-loopcount+1
}

Prob2<-TotalSum/10000

#P(2<X<8) para alg 3
loopcount<-3
TotalSum<-0

while(loopcount<8){
  TotalSum<-TotalSum + length(which(RPoissonAlg==loopcount))
  loopcount<-loopcount+1
}

Prob3<-TotalSum/10000

#print dos resultados de P(2<X<8)
print(Prob1)
print(Prob2)
print(Prob3)

#comparação dos dados dos vetores
all.equal(GeneratePoissonRecursive,RPoissonAlg)

xvalues <- seq(0,11,1)

#juntar dados relativos aos 3 algoritmos e ao teorico
bindgraphs <- rbind(dpois(xvalues,3),prop.table(table(GeneratePoissonRecursive)),prop.table(table(GeneratePoissonSum)),prop.table(table(RPoissonAlg)))

#gráfico de barras
barplot(bindgraphs,beside=T,main="Distribuição relativista dos valores",ylab = "Frequência relativa/Probabilidade",col = c("deepskyblue4","deepskyblue2","cyan3","cyan"))
legend("topright",c("Poisson Teórico","Alg. Recursividade","Alg. Soma de Exp.","Gerador do R"),fill = c("deepskyblue4","deepskyblue2","cyan3","cyan"))

#soma auxiliar
sum((prop.table(table(GeneratePoissonRecursive))-dpois(xvalues,3))^2)
sum((prop.table(table(GeneratePoissonSum))-dpois(xvalues,3))^2)
sum((prop.table(table(RPoissonAlg))-dpois(xvalues,3))^2)
##ALINEA B##



#500 amostras
#Sem redução
UniformValues1<-{}
for(i in 1:500){
  set.seed(3*i)
  UniformValues1<-rbind(UniformValues1,runif(50,0,1))
}

GeneratePoissonRecursive1<-{}
for(i in 1:500){
  GeneratePoissonRecursive1<-rbind(GeneratePoissonRecursive1,unlist(lapply(UniformValues1[i, ],PoissonRecursive)))
}

#Com redução
UniformValues2<-{}
for(i in 1:500){
  set.seed(3*i)
  Unif<-runif(25,0,1)
  UniformValues2<-rbind(UniformValues2,c(Unif,1-Unif))
}

GeneratePoissonRecursive2<-{}
for(i in 1:500){
  GeneratePoissonRecursive2<-rbind(GeneratePoissonRecursive2,unlist(lapply(UniformValues2[i, ],PoissonRecursive)))
}

#P(msdia de X>2)
#sem redução
Media1<-c()
P1<-0
for(i in 1:500){
  t1<-0
  for(j in 1:50){
    t1<-t1+GeneratePoissonRecursive1[i,j]
  }
  m1<-t1/50
  Media1<- append(Media1,m1)
  if (m1>=3){P1<-P1+1}
}
P1<-P1/500

#com redução
Media2<-c()
P2<-0
for(i in 1:500){
  t2<-0
  for(j in 1:50){
    t2<-t2+GeneratePoissonRecursive2[i,j]
  }
  m2<-t2/50
  Media2<- append(Media2,m2)
  if (m2>=3) {P2<-P2+1}
}
P2<-P2/500

#Estimativa Pontual
#sem redução
EstPontual1<-0
for(i in 1:500){
  EstPontual1<-EstPontual1 + Media1[i] 
}
EstPontual1<-EstPontual1/500

#com redução
EstPontual2<-0
for(i in 1:500){
  EstPontual2<-EstPontual2 + Media1[i] 
}
EstPontual2<-EstPontual2/500

#IC90%
#sem redução
sd(Media1)   #desvio-padrão das médias sem redução

IC1=c()
IC1<-append(IC1,EstPontual1-qnorm(.95)*sd(Media1)/sqrt(500))
IC1<-append(IC1,EstPontual1+qnorm(.95)*sd(Media1)/sqrt(500))

#com redução
sd(Media2)   #desvio-padrão das médias com redução

IC2=c()
IC2<-append(IC2,EstPontual2-qnorm(.95)*sd(Media2)/sqrt(500))
IC2<-append(IC2,EstPontual2+qnorm(.95)*sd(Media2)/sqrt(500))

#Variancia Media1
var(Media1)

#Variancia Media2
var(Media2)
####QUESTAO 2####

#Definir as funcoes
f <- function(x){
  (3/8)*(1-x)^2
}
range <- seq(-1,1,by=0.01)
g <- function(x){
  return(rep(1/2,length(x))) #para devolver um vetor no mesmo comprimento do que o dominio
}
range2 <- seq(-1,1,by=0.01)

#Desenhar o grafico das funcoes
plot(range, f(range), type="l", main="f.d.p de X e de Y", xlab="x", ylab="y", col="blue")
lines(range2,g(range2),col="red")

#Confirmar o valor de c
max(f(range)/g(range))

#Desenhar o grafico de f(x) e cg(x)
c <- f(-1)/g(-1)
plot(range, f(range), type="l", xlab="x", ylab="y", col="blue")
lines(range2,c*g(range2), col="red")

#Algoritmo de aceitacao-rejeicao
set.seed(3)
f_x <- c();

for(i in 1:500){
  y <- 2*(runif(1))-1 #gera um numero da U(-1,1) pelo metodo da transformacao inversa, podemos usar tambem runif(1,-1,1)
  u <- runif(1)
  x <- f(y)/(c*g(y))
  if(u<x) {
    f_x <- c(f_x,y)
  }	
}
length(f_x)

#Histogramas dos resultados
hist(f_x, xlim=c(-1,1), main="Histograma da frequência", xlab="x",ylab='Frequência')
hist(f_x, prob=T, xlim=c(-1,1), main="Histograma da Probabilidade", xlab="x",ylab = 'Probabilidade')

#Histograma com a fdp
hist(f_x, prob=T, xlim=c(-1,1), main="Histograma da probabilidade com a f.d.p. de X", xlab="x",ylab = 'Probabilidade') # eixo dos y Ã© a densidade
lines(range,f(range),col="blue")

####QUESTAO 3####
# Gerar 500 amostras de dimensão 6 da v.a. X~Gama(3,0.1)
set.seed(3)
N={}
for(k in 1:500){
  N<-rbind(N,rgamma(6,3,0.1))#Com rbind colocamos 6 v.a. Gama por linha em 500 linhas
}

#Calculo das 500 Estatisticas de Teste
Mdn<-26.7406 #Valor da mediana por hipótese
EstTst<-c()
for (r in 1:500){
  soma=0
  for (c in 1:6){
    if(N[r,c]>Mdn){
      soma=soma+1
    }
  }
  s=c(soma) #valor da estatistica da linha r, soma~Bin(6,0.5)
  EstTst<-append(EstTst,s)
}

#Calculo dos 500 valores-p usuais
pvaluesu<-c()
for(s in EstTst){
  p=2*min(0.5,pbinom(s,6,0.5),dbinom(s,6,0.5)+pbinom(s,6,0.5,F)) #dbinom e f.prob. pbinom e f.distribuicao
  pvaluesu<-append(pvaluesu,p)
}

#Calculo dos 500 valores-p PVM
pvaluespvm<-c()
for(s in EstTst){
  p=0
  ps0=dbinom(s,6,0.5)
  for(n in 0:6){
    prob<-dbinom(n,6,0.5)
    if(prob<ps0 || abs(prob-ps0)<10^(-10)){
      p=p+prob
    }
  }
  pvaluespvm<-append(pvaluespvm,p)
}

#Frequências numéricas interessantes
table(EstTst)
table(pvaluespvm)
table(pvaluesu)
#Histogramas que mostram a tendencias dos valores-p
hist(pvaluesu,main='Processo Usual',xlab='valores-p',ylab = 'Frequência')
hist(pvaluespvm, main='Princípio de Verosimilhança Mínima', xlab='valores-p',ylab = 'Frequência')

####FIM####