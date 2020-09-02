#Lendo o arquivo .rds
morador<-as_tibble(readRDS("MORADOR.rds"))

#Primeiro vou trabalhar o df. "morador" que contém todas as informações de cada
#Morador, selecionando as variáveis de interesse

MORADOR_UC_1<-morador%>%
  distinct(UF,ESTRATO_POF,TIPO_SITUACAO_REG,COD_UPA,NUM_DOM,NUM_UC,
           PESO_FINAL,.keep_all =TRUE)

MORADOR_UC<-MORADOR_UC_1[,c("UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC","COD_INFORMANTE","V0306","V0403","V0404","V0405","V0425","ANOS_ESTUDO","PESO","PESO_FINAL","RENDA_TOTAL")]

#Alterando o cabeçalho das colunas para facilitar a navegação
colnames(MORADOR_UC)<-c("UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC","COD_INFORMANTE","SUBORDINAÇÃO","IDADE","SEXO","COR_RACA","CURSO_MAX","ANOS_ESTUDO","PESO","PESO_FINAL","RENDA_TOTAL")

#Criando uma variável com o total populacional estimado pela pesquisa
SOMA_TOTAL_PESOS <- sum(MORADOR_UC$PESO_FINAL )


#Criando diferentes níveis de UF e indexando no df. "VAR_ESTUDO_MORADOR"
UF_NIVEIS<-as_tibble(factor(findInterval(MORADOR_UC$UF,
                                         c(11,12,13,14,15,16,17,21,22,23,24
                                           ,25,26,27,28,29,31,32,33,35,41,
                                           42,43,50,51,52,53)),levels = 1:27,
                            labels = c("Rondônia","Acre","Amazonas","Roraima"
                                       ,"Pará","Amapá","Tocantins","Maranhão"
                                       ,"Piauí","Ceará","Rio Grande do Norte"
                                       ,"Paraíba","Pernambuco","Alagoas","Sergipe"
                                       ,"Bahia","Minas Gerais","Espírito Santo"
                                       ,"Rio de Janeiro","São Paulo","Paraná"
                                       ,"Santa Catarina","Rio Grande do Sul",
                                       "Mato Grosso do Sul","Mato Grosso","Goiás"
                                       ,"Distrito Federal")))
colnames(UF_NIVEIS)<-c("NIVEIS_UF")


#Criando diferentes níveis regionais (Urbano e Rural) e indexando no df. "VAR_ESTUDO_MORADOR"
REG_NIVEIS<-as_tibble(factor(findInterval(MORADOR_UC$TIPO_SITUACAO_REG,
                                          c(1,2)),levels = 1:2,
                             labels = c("Urbano","Rural")))
colnames(REG_NIVEIS)<-c("NIVEIS_REG")


#Criando diferentes níveis de UF e indexando no df. "VAR_ESTUDO_MORADOR"
REG_UF_NIVEIS<-as_tibble(cut(MORADOR_UC$UF,breaks=c(0,18,30,36,44,53),
                             include.lowest=TRUE,
                             labels=c("Norte","Nordeste","Sudeste","Sul"
                                      ,"Centro-oeste")))

colnames(REG_UF_NIVEIS)<-c("NIVEIS_REG_UF")


#Criando diferentes níveis de idade e indexando no df. "VAR_ESTUDO_MORADOR"
IDADE_NIVEIS<-as_tibble(cut(MORADOR_UC$IDADE
                          ,breaks=c(0,10,15,20,25,30,35,40,45,50,55,60
                                    ,65,70,75,Inf),
                          include.lowest=TRUE
                          ,labels = c("Até 10 anos",
                                      "Mais de 10 a 15",
                                      "Mais de 15 a 20",
                                      "Mais de 20 a 25",
                                      "Mais de 25 a 30",
                                      "Mais de 30 a 35",
                                      "Mais de 35 a 40",
                                      "Mais de 40 a 45",
                                      "Mais de 45 a 50",
                                      "Mais de 50 a 55",
                                      "Mais de 55 a 60",
                                      "Mais de 60 a 65",
                                      "Mais de 65 a 70",
                                      "Mais de 70 a 75",
                                      "Mais de 75"
                          )))
colnames(IDADE_NIVEIS)<-c("NIVEIS_IDADE")


#Criando diferentes níveis de gênero e indexando no df. "VAR_ESTUDO_MORADOR"
SEXO_NIVEIS<-as_tibble(factor(findInterval(MORADOR_UC$SEXO,
                                           c(1,2)),levels = 1:2,
                              labels = c("Homem","Mulher")))
colnames(SEXO_NIVEIS)<-c("NIVEIS_SEXO")


#Criando diferentes níveis de cor ou raça e indexando no df. "VAR_ESTUDO_MORADOR"
CR_NIVEIS<-as_tibble(factor(findInterval(MORADOR_UC$COR_RACA,
                                         c(1,2,3,4,5,9)),levels = 1:6,
                            labels = c("Branca","Preta","Amarela","Parda",
                                       "Indígena","Sem declaração")))
colnames(CR_NIVEIS)<-c("NIVEIS_CR")


#Criando diferentes níveis de nível educacional e indexando no df. "VAR_ESTUDO_MORADOR"
EDU_NIVEIS<-as_tibble(factor(findInterval(MORADOR_UC$CURSO_MAX,
                                          c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)),levels = 1:15,
                             labels = c("Creche","Pré-Escola",
                                        "Classe de alfabetização","Alfabetização de jovens e adultos",
                                        "Antigo primário","Antigo ginasial",
                                        "Primeiro grau","EJA - Primeiro grau",
                                        "Antigo ciêntífico","Segundo grau",
                                        "EJA - Segundo grau","Superior",
                                        "Especialização","Mestrado","Doutorado"
                             )))

colnames(EDU_NIVEIS)<-c("NIVEIS_EDU")


#Criando diferentes níveis de renda e indexando no df. "VAR_ESTUDO_MORADOR"
RENDA_NIVEIS<-as_tibble(cut(MORADOR_UC$RENDA_TOTAL
                            ,breaks=c(0,954,1908,2862,5724,9540,14310,23850,Inf),
                            include.lowest=TRUE
                            ,labels = c("Até 1","Mais de 1 a 2","Mais de 2 a 3",
                                        "Mais de 3 a 6","Mais de 6 a 10",
                                        "Mais de 10 a 15","Mais de 15 a 25",
                                        "Mais de 25")))
colnames(RENDA_NIVEIS)<-c("NIVEIS_RENDA")


#Adicionado novas colunas
MORADOR_UC<-MORADOR_UC%>%
  mutate(NIVEIS_UF=UF_NIVEIS$NIVEIS_UF
         ,NIVEIS_REG_UF=REG_UF_NIVEIS$NIVEIS_REG_UF
         ,NIVEIS_REG=REG_NIVEIS$NIVEIS_REG
         ,NIVEIS_IDADE=IDADE_NIVEIS$NIVEIS_IDADE
         ,NIVEIS_SEXO=SEXO_NIVEIS$NIVEIS_SEXO
         ,NIVEIS_CR=CR_NIVEIS$NIVEIS_CR
         ,NIVEIS_EDU=EDU_NIVEIS$NIVEIS_EDU
         ,NIVEIS_RENDA=RENDA_NIVEIS$NIVEIS_RENDA)

rm(morador,UF_NIVEIS,REG_UF_NIVEIS,REG_NIVEIS,IDADE_NIVEIS,SEXO_NIVEIS,CR_NIVEIS
   ,EDU_NIVEIS,RENDA_NIVEIS)
