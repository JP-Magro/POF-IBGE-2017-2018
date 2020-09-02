#Lendo os RDS
caderneta_coletiva <- as_tibble(readRDS("CADERNETA_COLETIVA.rds"))
despesa_individual<- as_tibble(readRDS("DESPESA_INDIVIDUAL.rds"))

#Cad_coletiva
CAD_COL<-caderneta_coletiva%>%
  mutate(codigo = round(V9001/100))%>%
  subset(codigo < 86001 | codigo > 89999)%>%
  mutate(valor_mensal=((V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12))

colnames(CAD_COL)<-c("UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA"
                                ,"NUM_DOM","NUM_UC","QUADRO","SEQ","TIPO_AQUISICAO"
                                ,"FORMA_AQUISIÇÃO","VALOR_R$","DEFLATOR","V8000_DEFLA"
                                ,"COD_IMPUT_VALOR","FATOR_ANUALIZACAO","PESO"
                                ,"PESO_FINAL","RENDA_TOTAL","QTD_ADQ","UND_1","UND_2"
                                ,"QTD_FINAL","codigo","valor_mensal")

rm(caderneta_coletiva)

#Criando diferentes níveis de UF e indexando no df. "CAD_COL"
UF_NIVEIS<-as_tibble(factor(findInterval(CAD_COL$UF,
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


#Criando diferentes níveis regionais (Urbano e Rural) e indexando no df. "CAD_COL"
REG_NIVEIS<-as_tibble(factor(findInterval(CAD_COL$TIPO_SITUACAO_REG,
                                          c(1,2)),levels = 1:2,
                             labels = c("Urbano","Rural")))
colnames(REG_NIVEIS)<-c("NIVEIS_REG")


#Criando diferentes níveis de UF e indexando no df. "CAD_COL"
REG_UF_NIVEIS<-as_tibble(cut(CAD_COL$UF,breaks=c(0,18,30,36,44,53),
                             include.lowest=TRUE,
                             labels=c("Norte","Nordeste","Sudeste","Sul"
                                      ,"Centro-oeste")))
colnames(REG_UF_NIVEIS)<-c("NIVEIS_REG_UF")


#Criando diferentes níveis de UF e indexando no df. "CAD_COL"
FORMA_AQUISIÇÃO_NIVEIS<-as_tibble(factor(findInterval(CAD_COL$FORMA_AQUISIÇÃO,
                                             c(1,2,3,4,5,6,7,8,9,10,11))
                                ,levels = 1:11
                                ,labels = c("Monetária à vista p/ a UC"
                                            ,"Monetária à vista p/ outra UC"
                                            ,"Monetária a prazo p/ a UC"
                                            ,"Monetária a prazo p/ outra UC"
                                            ,"Cartão de crédito à vista p/ a UC"
                                            ,"Cartão de crédito à vista p/ outra UC"
                                            ,"Doação"
                                            ,"Retirada do negócio"
                                            ,"Troca"
                                            ,"Produção própria"
                                            ,"Outra")))
colnames(FORMA_AQUISIÇÃO_NIVEIS)<-c("NIVEIS_FORMA_AQUISIÇÃO")

#Criando diferentes níveis de renda e indexando no df. "CAD_COL"
RENDA_NIVEIS<-as_tibble(cut(CAD_COL$RENDA_TOTAL
                            ,breaks=c(0,954,1908,2862,5724,9540,14310,23850,Inf),
                            include.lowest=TRUE
                            ,labels = c("Até 1","Mais de 1 a 2","Mais de 2 a 3",
                                        "Mais de 3 a 6","Mais de 6 a 10",
                                        "Mais de 10 a 15","Mais de 15 a 25",
                                        "Mais de 25")))
colnames(RENDA_NIVEIS)<-c("NIVEIS_RENDA")

#Indexando todos os níveis no df. "CAD_COL"
CAD_COL<-CAD_COL%>%
  mutate(NIVEIS_UF=UF_NIVEIS$NIVEIS_UF
         ,NIVEIS_REG=REG_NIVEIS$NIVEIS_REG
         ,NIVEIS_REG_UF=REG_UF_NIVEIS$NIVEIS_REG_UF
         ,NIVEIS_FORMA_AQUISIÇÃO=FORMA_AQUISIÇÃO_NIVEIS$NIVEIS_FORMA_AQUISIÇÃO
         ,NIVEIS_RENDA=RENDA_NIVEIS$NIVEIS_RENDA)

rm(UF_NIVEIS,REG_NIVEIS,REG_UF_NIVEIS,FORMA_AQUISIÇÃO_NIVEIS)

#Desp_Ind
DESP_IND<-despesa_individual%>%
  mutate(codigo = round(V9001/100))%>%
  subset(QUADRO==24|codigo==41001|codigo==48018|codigo==49075|codigo==49089)%>%
  mutate(valor_mensal = ifelse( QUADRO==24|QUADRO==41,
                                ((V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12), 
                                ((V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12)
  ))

colnames(DESP_IND)<-c("UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA"
                     ,"NUM_DOM","NUM_UC","COD_INFORMANTE","QUADRO","SEQ","TIPO_AQUISICAO"
                     ,"FORMA_AQUISIÇÃO","VALOR_R$","ULTIMA_COMPRA_MÊS"
                     ,"N_MESES_COMPRA","ESTADO_PROD","MOTIVO_VIAGEM","ORIGEM_VIAGEM"
                     ,"DEFLATOR","V8000_DEFLA"
                     ,"COD_IMPUT_VALOR","FATOR_ANUALIZACAO","PESO"
                     ,"PESO_FINAL","RENDA_TOTAL","codigo","valor_mensal")

rm(despesa_individual)


#Criando diferentes níveis de UF e indexando no df. "DESP_IND"
UF_NIVEIS<-as_tibble(factor(findInterval(DESP_IND$UF,
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


#Criando diferentes níveis regionais (Urbano e Rural) e indexando no df. "DESP_IND"
REG_NIVEIS<-as_tibble(factor(findInterval(DESP_IND$TIPO_SITUACAO_REG,
                                          c(1,2)),levels = 1:2,
                             labels = c("Urbano","Rural")))
colnames(REG_NIVEIS)<-c("NIVEIS_REG")


#Criando diferentes níveis de UF e indexando no df. "DESP_IND"
REG_UF_NIVEIS<-as_tibble(cut(DESP_IND$UF,breaks=c(0,18,30,36,44,53),
                             include.lowest=TRUE,
                             labels=c("Norte","Nordeste","Sudeste","Sul"
                                      ,"Centro-oeste")))

colnames(REG_UF_NIVEIS)<-c("NIVEIS_REG_UF")


#Criando diferentes níveis de UF e indexando no df. "DESP_IND"
FORMA_AQUISIÇÃO_NIVEIS<-as_tibble(factor(findInterval(DESP_IND$FORMA_AQUISIÇÃO,
                                                      c(1,2,3,4,5,6,7,8,9,10,11))
                                         ,levels = 1:11
                                         ,labels = c("Monetária à vista p/ a UC"
                                                     ,"Monetária à vista p/ outra UC"
                                                     ,"Monetária a prazo p/ a UC"
                                                     ,"Monetária a prazo p/ outra UC"
                                                     ,"Cartão de crédito à vista p/ a UC"
                                                     ,"Cartão de crédito à vista p/ outra UC"
                                                     ,"Doação"
                                                     ,"Retirada do negócio"
                                                     ,"Troca"
                                                     ,"Produção própria"
                                                     ,"Outra")))
colnames(FORMA_AQUISIÇÃO_NIVEIS)<-c("NIVEIS_FORMA_AQUISIÇÃO")

#Criando diferentes níveis de renda e indexando no df. "DESP_IND"
RENDA_NIVEIS<-as_tibble(cut(DESP_IND$RENDA_TOTAL
                            ,breaks=c(0,954,1908,2862,5724,9540,14310,23850,Inf),
                            include.lowest=TRUE
                            ,labels = c("Até 1","Mais de 1 a 2","Mais de 2 a 3",
                                        "Mais de 3 a 6","Mais de 6 a 10",
                                        "Mais de 10 a 15","Mais de 15 a 25",
                                        "Mais de 25")))
colnames(RENDA_NIVEIS)<-c("NIVEIS_RENDA")

#Indexando todos os níveis no df. "DESP_IND"
DESP_IND<-DESP_IND%>%
  mutate(NIVEIS_UF=UF_NIVEIS$NIVEIS_UF
         ,NIVEIS_REG=REG_NIVEIS$NIVEIS_REG
         ,NIVEIS_REG_UF=REG_UF_NIVEIS$NIVEIS_REG_UF
         ,NIVEIS_FORMA_AQUISIÇÃO=FORMA_AQUISIÇÃO_NIVEIS$NIVEIS_FORMA_AQUISIÇÃO
         ,NIVEIS_RENDA=RENDA_NIVEIS$NIVEIS_RENDA)

rm(UF_NIVEIS,REG_NIVEIS,REG_UF_NIVEIS,FORMA_AQUISIÇÃO_NIVEIS,RENDA_NIVEIS)

#Juntando as tabelas

SBT_ANALISE<-bind_rows(CAD_COL,DESP_IND)

rm(CAD_COL,DESP_IND)

