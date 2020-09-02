#Tabela 1
#Frequência de domicílios que compraram produtos orgânicos na pesquisa

#NA PESQUISA
NUM_OBS_UNQ_TOTAL<-SBT_ANALISE%>%
  distinct(COD_UPA,NUM_DOM)%>%
  nrow()
NUM_OBS_UNQ_ORGANICOS<-SBT_ORGANICOS%>%
  distinct(COD_UPA,NUM_DOM)%>%
  nrow()

#Criando uma tabela do total da população da amostra
TAB_1_PESQ<-as_tibble(1:2)
TAB_1_PESQ<-TAB_1_PESQ%>%
  mutate(Consumo=c("Consumiram","Não consumiram"))%>%
  mutate(Frequência=rbind(NUM_OBS_UNQ_TOTAL,NUM_OBS_UNQ_ORGANICOS))%>%
  mutate(Frequência=accounting(c(round(Frequência[2,]),round((Frequência[1,]-Frequência[2,])))))%>%
  mutate("%"=percent(Frequência/sum(Frequência)))%>%
  adorn_totals(where = "row",fill = "Total")%>%
  mutate(value=NULL)

formattable(TAB_1_PESQ,list(`Frequência`=color_bar("lightgray"))
            ,align = c("l", rep("r", NCOL(TAB_1_PESQ))))

#Tabela 2

#Frequência de indivíduos que compraram produtos orgânicos no Brasil
SOMA_SBT_ANALISE_ORG<-sum(count(x=SBT_ORGANICOS,COD_UPA,wt=PESO_FINAL)$n)

#Criando uma tabela do total estimado de moradias
TAB_2_POP<-as_tibble(1:2)
TAB_2_POP<-TAB_2_POP%>%
  mutate(Consumo=c("Consumiram","Não consumiram"))%>%
  mutate(Frequência=rbind(SOMA_SBT_ANALISE_ORG,SOMA_TOTAL_PESOS))%>%
  mutate(Frequência=accounting(c(round(Frequência[1,]),round((Frequência[2,]-Frequência[1,])))))%>%
  mutate("%"=percent(Frequência/sum(Frequência)))%>%
  adorn_totals(where = "row",fill = "Total")%>%
  mutate(value=NULL)

formattable(TAB_2_POP
            ,list(`Frequência`=color_bar("lightgray"))
            ,align = c("l", rep("r", NCOL(TAB_2_POP))))



#Tabela 3
#Participação na aquisição de produtos orgânicos por regiões do Brasil
TAB_3_PROD_REG_PESQ<-SBT_ORGANICOS%>%
  count(NIVEIS_REG_UF)%>%
  mutate(p=percent(n/sum(n)))%>%
  mutate("Região"=NIVEIS_REG_UF)%>%
  mutate("Frequência"=n)%>%
  mutate("%"=p)%>%
  adorn_totals()%>%
  mutate(NIVEIS_REG_UF=NULL,p=NULL,n=NULL)

formattable(TAB_3_PROD_REG_PESQ
            ,list(`Frequência`=color_bar("lightgray"))
            ,align = c("l", rep("r", NCOL(TAB_3_PROD_REG_PESQ))))


#Tabela 4
#Número de moradias que consumiram produtos orgânicos por região

Tab_4_DOM_REG_PESQ<-SBT_ORGANICOS%>%
  distinct(COD_UPA,NUM_DOM,.keep_all = T)%>%
  count(NIVEIS_REG_UF)%>%
  mutate(p=percent(n/sum(n)))%>%
  mutate("Região"=NIVEIS_REG_UF,"Frequência"=n,"%"=p)%>%
  mutate(NIVEIS_REG_UF=NULL,n=NULL,p=NULL)%>%
  adorn_totals()

formattable(Tab_4_DOM_REG_PESQ
            ,list(`Frequência`=color_bar("lightgray"))
            ,align = c("l", rep("r", NCOL(TAB_1_PESQ))))

#Tabela 5

Tab_5_DOM_REG_PESQ_1<-SBT_ORGANICOS%>%
  distinct(COD_UPA,NUM_DOM,.keep_all = T)%>%
  group_by(NIVEIS_REG_UF)%>%
  tabyl(NIVEIS_REG_UF)

Tab_5_DOM_REG_PESQ_2<-MORADOR_UC%>%
  anti_join(SBT_ORGANICOS,by= c("COD_UPA","NUM_DOM"))%>%
  distinct(COD_UPA,NUM_DOM,.keep_all = T)%>%
  group_by(NIVEIS_REG_UF)%>%
  tabyl(NIVEIS_REG_UF)
  
Tab_5_DOM_REG_PESQ<-left_join(Tab_5_DOM_REG_PESQ_1[,1:2]
                              ,Tab_5_DOM_REG_PESQ_2[,1:2]
                              ,by ="NIVEIS_REG_UF")%>%
  adorn_percentages(denominator = "row")%>%
  adorn_pct_formatting()%>%
  left_join(Tab_5_DOM_REG_PESQ_1[,1:2]
            ,Tab_5_DOM_REG_PESQ_2[,1:2]
            ,by ="NIVEIS_REG_UF")%>%
  left_join(Tab_5_DOM_REG_PESQ_2[,1:2]
            ,Tab_5_DOM_REG_PESQ_1[,1:2]
            ,by ="NIVEIS_REG_UF")%>%
  mutate("Região"=NIVEIS_REG_UF
         ,"Frequência - Compraram"=accounting(n.x.x)
         ,"% - Cons."=n.x
         ,"Frequência - Não compraram"=accounting(n.y.y)
         ,"% - Não cons."=n.y
         ,.keep="none")%>%
  adorn_totals(where = c("row"))

formattable(Tab_5_DOM_REG_PESQ
            ,list(`Frequência - Consumidoras`=color_bar("lightgray")
                  ,`Frequência - Não consumidoras`=color_bar("lightgray"))
            ,align = c("l", rep("r", NCOL(Tab_5_DOM_REG_PESQ))))

#Tabela 6

TAB_3_PROD_REG_PESQ_2<-SBT_ORGANICOS%>%
  count(NIVEIS_REG_UF)%>%
  mutate(n=as.numeric(n))%>%
  mutate(p=percent(n/sum(n)))

Tab_4_DOM_REG_PESQ_2<-SBT_ORGANICOS%>%
  distinct(COD_UPA,NUM_DOM,.keep_all = T)%>%
  count(NIVEIS_REG_UF)%>%
  mutate(n=as.numeric(n))%>%
  mutate(p=percent(n/sum(n)))

TAB_6_PESQ_1<-SBT_ORGANICOS%>%
  distinct(TIPO_AQUISICAO,.keep_all = T)%>%
  group_by(NIVEIS_REG_UF)%>%
  tabyl(NIVEIS_REG_UF)%>%
  mutate(percent=percent(percent))

TAB_6_PESQ<-left_join(Tab_4_DOM_REG_PESQ_2
                      ,TAB_3_PROD_REG_PESQ_2
                      ,by ="NIVEIS_REG_UF")

TAB_6_PESQ<-left_join(TAB_6_PESQ
                      ,TAB_6_PESQ_1
                      ,by ="NIVEIS_REG_UF")%>%
  adorn_totals(where = c("row"))%>%
  mutate(MAD=round((n.y/n.x),2))%>%
  mutate(n.y.y=NULL,p=NULL)

colnames(TAB_6_PESQ)<-c("Região"
                        ,"Frequência - Domicílios"
                        ,"% - Dom."
                        ,"Frequência - Compras"
                        ,"% - Comp."
                        ,"Frequência - Produtos"
                        ,"% - Prod."
                        ,"Média de aquisições por domicílio")

formattable(TAB_6_PESQ
            ,list(`Frequência - Domicílios`=color_bar("lightgray")
                  ,`Frequência - Compras`=color_bar("lightgray")
                  ,`Frequência - Produtos`=color_bar("lightgray"))
            ,align = c("l", rep("r", NCOL(TAB_6_PESQ))))

TAB_6_PESQ_M <- as.data.table(TAB_6_PESQ[(1:(nrow(TAB_6_PESQ)-1))
                                         ,c("Região"
                                         ,"Frequência - Domicílios"
                                         ,"Frequência - Compras"
                                         ,"Frequência - Produtos")])
colnames(TAB_6_PESQ_M)<-c("Região"
                          ,"Domicílios"
                          ,"Compras"
                          ,"Produtos")
  

TAB_6_PESQ_M <- melt(TAB_6_PESQ_M,id.vars = 1)



ggplot(TAB_6_PESQ_M,aes(x = Região,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge")+
  labs(
    title = "Comparação entre as frequências do número de domicílios
    ,número de compras e tipo de produtos da pesquisa"
    ,y = "Frequência"
    ,fill = "Variáveis")+
  theme_minimal()

#Tabela 7

TAB_7_DESP_PESQ_1<-SBT_ANALISE%>%
  summarise(n=sum(valor_mensal))%>%
  mutate(n=n/SOMA_TOTAL_PESOS)

TAB_7_DESP_PESQ_2<-SBT_DOM_ORG%>%
  group_by(COD_UPA,NUM_DOM)%>%
  summarise(m=sum(valor_mensal/PESO_FINAL.groups="keep"))

TAB_7_DESP_PESQ_3<-SBT_ORGANICOS%>%
  group_by(COD_UPA,NUM_DOM)%>%
  summarise(m=sum(valor_mensal/PESO_FINAL.groups="keep"))

TAB_7_DESP_PESQ_T1<-as.data.frame(rbind(mean(TAB_7_DESP_PESQ_3$m),0))
TAB_7_DESP_PESQ_T2<-rbind(mean(TAB_7_DESP_PESQ_2$m),TAB_7_DESP_PESQ_1)

TAB_7_DESP_PESQ<-cbind(TAB_7_DESP_PESQ_T1,TAB_7_DESP_PESQ_T2)%>%
  mutate("Produtos orgânicos"=V1,"Alimentação"=n,.keep="none")%>%
  mutate("Famílias"=c("Consumiram","Não Consumiram"),.before=1)

formattable(TAB_7_DESP_PESQ,align = c("l",rep("r",NCOL(TAB_7_DESP_PESQ))))
  
