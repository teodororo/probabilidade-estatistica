#RStudio>Environment>Import Dataset>From Excel...>Browse...>BD_Alunos.xlsx

#instalação de pacotes
install.packages("descr")
install.packages("tidyverse",dependencies = TRUE)
install.packages("readxl")
install.packages("xlsx")

#verificando a ausência de erros na instalação/utillizando pacotes
library(descr)
library(tidyverse)
library(readxl)
library(xlsx)

BD_Alunos <- read_excel("BD_Alunos.xlsx")
View(BD_Alunos)

# a) Descrever passo a passo a metodologia utilizada na seleção da amostra, indicando qual método utilizado:

#tamanho da amostra estabelecida previamente = 80

#criando os grupos para a amostragem
meteoro <- BD_Alunos[BD_Alunos$Curso=="Meteorologia",]
licenciatura <- BD_Alunos[BD_Alunos$Curso=="Licenciatura em Computação",]
eng <- BD_Alunos[BD_Alunos$Curso=="Engenharia",]

#amostragem sistemática precisa do número de elementos que virá de cada grupo, no final a soma tem que dar 80 
tamanhoeng <- round((80/294)*(length(eng$Registros)),digits=0)
tamanhometeoro <- round((80/294)*(length(meteoro$Registros)),digits=0)
tamanholic <- round((80/294)*(length(licenciatura$Registros)),digits=0)

#digits=0 é para transformar em um número inteiro, é como dizer quantas casas decimais existirão

#agora que temos o tamanho, precisamos retirar aleatoriamente essa quantidade de pessoas dos cursos
amostrameteoro <- meteoro[sample(nrow(meteoro),tamanhometeoro),]
amostralic <- licenciatura[sample(nrow(licenciatura),tamanholic),]
#antes de fazer a amostra de eng, verificou-se que a soma dos tamanhos (66+8+7) dá 81, então:
tamanhoeng <- tamanhoeng - 1
#agora sim, 65+8+7 = 80
amostraeng <-  eng[sample(nrow(eng),tamanhoeng),]

#unindo as amostras
superamostra <- rbind(amostraeng,amostralic,amostrameteoro)

# b) Construir tabelas e gráficos apropriados para as variáveis: “Curso”, “Cursou_EnsinoMedio”, “Genero” e “Trabalha”;

#Curso - tabela e gráfico

frequencias <- c(tamanhoeng,tamanholic,tamanhometeoro)
#100/80 = 1.25, tudo isso para usar na função:
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(1.25 * x, format = format, digits = digits, ...), "%")
}
porcentagem <- percent(frequencias)
tabelacurso <- data.frame(Cursos=c("Engenharia","Lic. em Computação","Meteorologia"),Frequencias=c(tamanhoeng,tamanholic,tamanhometeoro),Porcentagem=porcentagem)
View(tabelacurso)
barplot(table(superamostra$Curso), ylim = c(0,80), main="Gráfico 1 - Cursos", ylab="Quantidade de alunos")

#Cursou_EnsinoMedio - tabela e gráfico
tabelacursou <- freq(superamostra$Cursou_EnsinoMedio)
View(tabelacursou)
barplot(table(superamostra$Cursou_EnsinoMedio), ylim = c(0,40), main="Gráfico 2 - Instituição de ensino do ensino médio", ylab="Quantidade de alunos")

#Genero - tabela e gráfico
tabelagenero <- freq(superamostra$Genero)
View(tabelagenero)
pie(table(superamostra$Genero),main = "Gráfico 3 - Gênero")

#Trabalho - tabela e gráfico
tabelatrabalho <- freq(superamostra$Trabalha)
View(tabelatrabalho)
pie(table(superamostra$Trabalha),main = "Gráfico 4 - Trabalho")

# c) Construir tabelas e gráficos apropriados para as variáveis: “Horas_EstudoSemana” e “Conhecimento_Matematica”;

#Horas_EstudoSemana - tabela e gráfico 
#numero de classes pela formula de sturges:
k <- round(1+3.3*log10(length(superamostra$Horas_EstudoSemana)))
amp <- max(as.numeric(superamostra$Horas_EstudoSemana)) - min(as.numeric(superamostra$Horas_EstudoSemana))
h <- round(amp/k)
int <- seq(1,60,8)
classes <- c("1|-9","9|-17","17|-25","25|-33","33|-41","41|-49","49|-60")
tabelahoras <- table(cut(as.numeric(superamostra$Horas_EstudoSemana),breaks = int,right=FALSE,labels = classes))
View(tabelahoras)
hist(as.numeric(superamostra$Horas_EstudoSemana),breaks = h,main="Gráfico 5 - Horas de estudo semanal",col="grey",xlab="Horas",ylab="Alunos")

#Conhecimento_Matematica - tabela e gráfico
tabelamatematica <- freq(superamostra$Conhecimento_Matematica) 
View(tabelamatematica)
hist(as.numeric(superamostra$Conhecimento_Matematica),breaks = 5,main="Gráfico 6 - Conhecimento em matemática",col="grey",xlab="Nota",ylab="Alunos")

# d) Calcular a média, mediana, moda, desvio-padrão, coeficiente de variação e simetria das variáveis: “Idade” e “Horas_EstudoSemana”;

#IDADE
idademedia <- mean(superamostra$Idade)
#r = 18.7
idademediana <- median(superamostra$Idade)
#r = 18
#função para conseguir a moda
getmode <- function(v) {
     uniqv <- unique(v)
     uniqv[which.max(tabulate(match(v, uniqv)))]
 }
idademoda <- getmode(superamostra$Idade)
#r = 18
idadedesvio <-  round(sd(superamostra$Idade),digits=1)
#r = 2.8
#CV = SD/MEAN*100
idadecv <- round((idadedesvio/idademedia)*100,digits=1)
#r = 14.9
#coeficiente de assimetria de pearson: (media - moda)/desviopadrao
idadeassimetria <- round((idademedia - idademoda)/idadedesvio,digits=1)
#r = 0.3

#HORAS 
horamedia <-  mean(as.numeric(superamostra$Horas_EstudoSemana))
horamediana <- median(as.numeric(superamostra$Horas_EstudoSemana))
#r = 8
horamedia <- round(mean(as.numeric(superamostra$Horas_EstudoSemana)),digits=1)
#r = 13.2
horamoda <- as.numeric(getmode(superamostra$Horas_EstudoSemana))
#r = 3
horadesvio <- sd(superamostra$Horas_EstudoSemana)
horadesvio <- round(sd(superamostra$Horas_EstudoSemana),digits=1)
#r = 12.9
horacv <- round((horadesvio/horamedia)*100,digits=1)
#r = 97.7
horaassimetria <- round((horamedia - horamoda)/horadesvio,digits=1)
#r = 0.8

# e) Na análise da “Idade”, “Horas_EstudoSemana” e “Conhecimento_Matematica”, qual variável tem maior variabilidade? Justifique;
matcv <- ((sd(superamostra$Conhecimento_Matematica))/(mean(superamostra$Conhecimento_Matematica)))*100
maior <- max(c(idadecv,horacv,matcv))
#maior = 97.7 = hora de estudo é o que mais varia 

# f) Comparar a média de “Horas_EstudoSemana” em relação a variável “Trabalha” e descreva qual a conclusão da análise;
trabalha_sim <- superamostra[superamostra$Trabalha=="Sim",]
trabalha_nao <- superamostra[superamostra$Trabalha=="Não",]
horamedia_sim <-  round(mean(as.numeric(trabalha_sim$Horas_EstudoSemana)), digits = 1)
horamedia_nao <-  round(mean(as.numeric(trabalha_nao$Horas_EstudoSemana)), digits = 1)


# A média de horas de estudo de quem não trabalha é maior que a de quem trabalha. 11.2 > 8


# g) Relacione a variável “Genero” em relação ao “Cursou_EnsinoMedio” e 
#    responda: Qual a probabilidade de sortear um aluno que estudou em escola particular e
#    ser mulher? Dado que estuda em Escola Pública, qual a probabilidade de ser do gênero
#    masculino?


# A = {Aluno ter estudado em escola particular}
# B = {Aluno ser mulher}
total_alunos <- 80
# Estratificando o espaço amostral na variável gênero. 
homem <- superamostra[superamostra$Genero=="Masculino", ]
mulher <- superamostra[superamostra$Genero=="Feminino", ]

# Verificando quem estudou em escola particular
homem_em_esc_part <- homem[homem$Cursou_EnsinoMedio=="Escola Particular", ]
mulher_em_esc_part <- mulher[mulher$Cursou_EnsinoMedio=="Escola Particular", ]
total_alunos_em_part <- as.numeric(length(homem_em_esc_part$Registros) + length(mulher_em_esc_part$Registros))

# Calculando a probabilidade de A
prob_esc_particular <- total_alunos_em_part / total_alunos

# Calculando a probabilidade de B
total_mulher <- as.numeric(length(mulher$Registros))
prob_mulher <- total_mulher / total_alunos

# Calculando a probabilidade de A|B
prob_mulher_em_particular <- round((prob_mulher * prob_esc_particular * 100), digits = 1)

# Verificando em escola públic
homem_em_esc_pub <- homem[homem$Cursou_EnsinoMedio=="Escola Pública Normal", ]
mulher_em_esc_pub <- mulher[mulher$Cursou_EnsinoMedio=="Escola Pública Normal", ]

# C = {Probabilidade de ser de escola pública}
# D = {Probabilidade de ser homem}

# Calculando probabilidade de D
total_alunos_em_pub <- as.numeric(length(homem_em_esc_pub$Registros) + length(mulher_em_esc_pub$Registros))
total_homem <- as.numeric(length(homem$Registros))
prob_homem <- total_homem / total_alunos

# Calculando probabilidade de C
prob_esc_publica <- total_alunos_em_pub / total_alunos

# Calculando probabilidade de C|D
prob_homem_em_publica <- round((prob_homem * prob_esc_publica * 100), digits = 1)

# r = 5.9% para mulher em escola particular, 27.7% para homem em escola pública

# h) Relacione a variável “Trabalha” com “Curso” e responda: Qual a
#    probabilidade de sortear um aluno de Licenciatura em Computação que não trabalha?
#    Sabendo-se que o aluno é de Engenharia, qual a probabilidade de ele trabalhar?


# A = {Aluno de licenciatura de computação}
# B = {Aluno que não trabalha}

# Verificando alunos que não trabalham
aluno_q_nao_trabalha <- superamostra[superamostra$Trabalha=="Não", ]
qnt_aluno_q_nao_trabalha <- as.numeric(length(aluno_q_nao_trabalha$Registros))
qnt_aluno_q_nao_trabalha_lic <- as.numeric(length(amostralic$Trabalha=="Não"))

# Probabilidade de A
prob_licenciatura <- 1 / 3

# Calculando probabilidade de B
prob_nao_trabalha <- qnt_aluno_q_nao_trabalha / total_alunos

# Calculando probabilidade de A|B
prob_aluno_nao_trabalha <- round((prob_licenciatura * prob_nao_trabalha * 100), digits = 1)

#_______________________________________________________________________________________

# C = {Aluno que trabalha}
# D = {Aluno de engenharia}

# Calculando probabilidade de C
aluno_q_sim_trabalha <- superamostra[superamostra$Trabalha=="Sim", ]
qnt_total_sim <- as.numeric(length(aluno_q_sim_trabalha$Registros))
qnt_total_nao <- qnt_aluno_q_nao_trabalha
prob_trab_sim <- qnt_total_sim / total_alunos

# Calculando probabilidade de D
prob_eng <- 1 / 3

# Calculando probabilidade de C|D
prob_aluno_eng_trab_sim <- round((((prob_trab_sim * prob_eng) / ((prob_trab_sim * prob_eng) + (prob_nao_trabalha * prob_eng)))*100), digits = 1)

# r = aluno de licenciatura que não trabalha = 27.5%
# r = aluno de eng que trabalha = 17.5%

# i) Sabendo que o aluno estuda mais de 10 horas por semana, qual a
#    probabilidade de ter conhecimento em matemática maior ou igual a 4?


 # i) Sabendo que o aluno estuda mais de 10 horas por semana, qual a
#    probabilidade de ter conhecimento em matemática maior ou igual a 4?

 hrs_por_semana <- superamostra[superamostra$Horas_EstudoSemana > 10] 
# Do espaço amostral, retiramos os dados a serem analisados.
hrs_por_semana_maior_10 <- superamostra[as.numeric(superamostra$Horas_EstudoSemana) > 10, ]
conhecimento_mat_maior_igual_4 <- superamostra[as.numeric(superamostra$Conhecimento_Matematica)>= 4, ]

 # A = {conhecimento maior igual a 4}
# B = {horas maior que 10}

 # Pelo enunciado, verifica-se que o evento B já ocorreu. Dessa forma, o cálculo a ser seguido aqui é o de intersecção: 

 # Primeiro, calculamos a probabilidade do evento A intersecção B
prob_aluno_conhec_maior_igual_4 <- length(conhecimento_mat_maior_igual_4$Registros) / total_alunos
# Depois, a probabilidade do evento B
prob_hrs_por_semana_maior_10 <- length(hrs_por_semana_maior_10$Registros) / total_alunos

 # Finalmente é calculada a probabilidade de (A|B)
prob_aluno_mais_dez_hrs_conh_maior_igual_4 <- round(((prob_aluno_conhec_maior_igual_4/prob_hrs_por_semana_maior_10)*100), digits = 1) 


