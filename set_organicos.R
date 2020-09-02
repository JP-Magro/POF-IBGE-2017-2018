#Lendo planilhas com os produtos de interesse
PRODUTOS_ORGANICOS<-as_tibble(read.csv("organicos - Página1.csv"))
DOM_ORG<-SBT_ORGANICOS[,c("COD_UPA","NUM_DOM")]

#Criando um SBT das compras de produtos
SBT_PRODUTOS_ANALISE_FUNC<-function(data){
  dm<-dim(data)
  dm<-as.numeric(dm[1])
  sbt_prod_F<-as_tibble(NULL)
  sbt_prod_1<-as_tibble(NULL)
  for(i in 1:dm){
    sbt_prod_F<-filter(SBT_ANALISE,SBT_ANALISE[,9]==as.numeric(data[i,1]))
    sbt_prod_1<-rbind(sbt_prod_1,sbt_prod_F)
  }
  return(sbt_prod_1)
}
SBT_ORGANICOS<-SBT_PRODUTOS_ANALISE_FUNC(PRODUTOS_ORGANICOS)

DOM_ORG<-SBT_ORGANICOS[,c("COD_UPA","NUM_DOM")]
SBT_COD_DOM_ANALISE_FUNC<-function(data){
  dm<-dim(data)
  dm<-as.numeric(dm[1])
  sbt_prod_F<-as_tibble(NULL)
  sbt_prod_1<-as_tibble(NULL)
  for(i in 1:dm){
    sbt_prod_F<-filter(SBT_ANALISE,SBT_ANALISE[,4]==as.numeric(data[i,1])& SBT_ANALISE[,5]==as.numeric(data[i,2]))
    sbt_prod_1<-rbind(sbt_prod_1,sbt_prod_F)
  }
  return(sbt_prod_1)
}
SBT_DOM_ORG<-SBT_COD_DOM_ANALISE_FUNC(DOM_ORG)