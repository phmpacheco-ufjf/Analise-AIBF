library(RPostgreSQL)
library(dplyr)

# Criando conexão local
localdb <- src_postgres(dbname = 'AIBF',
                        host = 'localhost',
                        port = 5432,
                        user = 'postgres',
                        password = 'pedrin127ma')

# Determinando vetor com nomes das variáveis
var.names <- c('com', 'rou_inf', 'rou_con', 'rou_fil', 'est_fil', 'sau_fil', 'gas', 'trab_inf', 'trab_con', 'evi')

# Carregando Dados.Rdata
load(file.choose())

# Enviando tabelas importantes para banco de dados
for (i in var.names) {
  dbWriteTable(localdb$con, paste0('benef.', i, '.2005'), eval(parse(text = paste0('benef.', i, '.2005'))), row.names=FALSE)
  dbWriteTable(localdb$con, paste0('benef.', i, '.2009'), eval(parse(text = paste0('benef.', i, '.2009'))), row.names=FALSE)
  dbWriteTable(localdb$con, paste0('geral.', i), eval(parse(text = paste0('geral.', i))), row.names=FALSE)
}

# Deletando variáveis do R
rm(benef.com.2005, benef.com.2009, benef.est_fil.2005, benef.est_fil.2009, benef.evi.2005, benef.evi.2009, benef.gas.2005, benef.gas.2009, benef.rou_con.2005, benef.rou_con.2009,
   benef.rou_fil.2005, benef.rou_fil.2009, benef.rou_inf.2005, benef.rou_inf.2009, benef.sau_fil.2005, benef.sau_fil.2009, benef.trab_con.2005, benef.trab_con.2009, benef.trab_inf.2005,
   benef.trab_inf.2009, geral.com, geral.est_fil, geral.evi, geral.gas, geral.rou_con, geral.rou_fil, geral.rou_inf, geral.sau_fil, geral.trab_con, geral.trab_inf)
rm(AIBF.2005, AIBF.2009)

# Alterando atributos das tabelas
attributes(score_barganha)$class <- 'data.frame'
attributes(score_barganha.2)$class <- 'data.frame'
attributes(score.fem)$class <- 'data.frame'
attributes(score.fem.2)$class <- 'data.frame'

# Enviando os scores brutos para Banco de dados
dbWriteTable(localdb$con, 'score_barganha', score_barganha, row.names = FALSE)
dbWriteTable(localdb$con, 'score_barganha.2', score_barganha.2, row.names = FALSE)
dbWriteTable(localdb$con, 'score.fem', score.fem, row.names = FALSE)
dbWriteTable(localdb$con, 'score.fem.2', score.fem.2, row.names = FALSE)

# Excluindo tabelas
rm(score_barganha, score_barganha.2, score.fem, score.fem.2)

# Definindo Querys
benef.com.2005 <- tbl(localdb$con, 'benef.com.2005')
benef.com.2009 <- tbl(localdb$con, 'benef.com.2009')
benef.est_fil.2005 <- tbl(localdb$con, 'benef.est_fil.2005')
benef.est_fil.2009 <- tbl(localdb$con, 'benef.est_fil.2009')
benef.evi.2005 <- tbl(localdb$con, 'benef.evi.2005')
benef.evi.2009 <- tbl(localdb$con, 'benef.evi.2009')
benef.gas.2005 <- tbl(localdb$con, 'benef.gas.2005')
benef.gas.2009 <- tbl(localdb$con, 'benef.gas.2009')
benef.rou_con.2005 <- tbl(localdb$con, 'benef.rou_con.2005')
benef.rou_con.2009 <- tbl(localdb$con, 'benef.rou_con.2009')
benef.rou_fil.2005 <- tbl(localdb$con, 'benef.rou_fil.2005')
benef.rou_fil.2009 <- tbl(localdb$con, 'benef.rou_fil.2009')
benef.rou_inf.2005 <- tbl(localdb$con, 'benef.rou_inf.2005')
benef.rou_inf.2009 <- tbl(localdb$con, 'benef.rou_inf.2009')
benef.sau_fil.2005 <- tbl(localdb$con, 'benef.sau_fil.2005')
benef.sau_fil.2009 <- tbl(localdb$con, 'benef.sau_fil.2009')
benef.trab_con.2005 <- tbl(localdb$con, 'benef.trab_con.2005')
benef.trab_con.2009 <- tbl(localdb$con, 'benef.trab_con.2009')
benef.trab_inf.2005 <- tbl(localdb$con, 'benef.trab_inf.2005')
benef.trab_inf.2009 <- tbl(localdb$con, 'benef.trab_inf.2009')
geral.com <- tbl(localdb$con, 'geral.com')
geral.est_fil <- tbl(localdb$con, 'geral.est_fil')
geral.evi <- tbl(localdb$con, 'geral.evi')
geral.gas <- tbl(localdb$con, 'geral.gas')
geral.rou_con <- tbl(localdb$con, 'geral.rou_con')
geral.rou_fil <- tbl(localdb$con, 'geral.rou_fil')
geral.rou_inf <- tbl(localdb$con, 'geral.rou_inf')
geral.sau_fil <- tbl(localdb$con, 'geral.sau_fil')
geral.trab_con <- tbl(localdb$con, 'geral.trab_con')
geral.trab_inf <- tbl(localdb$con, 'geral.trab_inf')

###############################

install.packages('janitor')
library(tidyverse)
library(janitor)

make_tbl(subclass = 'tribble', benef.com.2005)

benef.com.2005 %>% as_tibble() %>% group_by(grupo) %>% nest(.key = 'Unidades') 

benef.com.2005 %>% as_tibble() %>% group_by(grupo, indicatriz) %>% summarise(n = n()) %>% spread(key = indicatriz, value = n)

benef.com.2005 %>% as_tibble() %>% tabyl(grupo, indicatriz) %>% adorn_totals(c('row', 'col')) %>% kable(caption = 'Ano 2005', format = 'markdown')

?kable
kable(addmargins(a, FUN = Total, quiet = T), caption = "Ano 2005") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)
rm(Total)

