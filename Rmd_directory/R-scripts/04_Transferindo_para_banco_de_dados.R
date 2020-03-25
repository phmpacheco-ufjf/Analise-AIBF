library(RPostgreSQL)
library(tidyverse)

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

# Pacote necessário para usar dplyr
library(tidyverse)

# Transformando dados em long
for (i in var.names) {
  assign(paste0('benef.', i), eval(parse(text = paste0('benef.', i, '.2005'))) %>% as_tibble() %>% mutate(ano = 0) %>% bind_rows(mutate(as_tibble(eval(parse(text = paste0('benef.', i, '.2009')))), ano = 1)))
}

# Juntando todas as variáveis
bd.trans <- benef.com %>% 
            full_join(benef.rou_inf[,-3], by = c('chave', 'ano')) %>% 
            full_join(benef.rou_con[,-3], by = c('chave', 'ano')) %>% 
            full_join(benef.rou_fil[,-3], by = c('chave', 'ano')) %>% 
            full_join(benef.est_fil[,-3], by = c('chave', 'ano')) %>% 
            full_join(benef.sau_fil[,-3], by = c('chave', 'ano')) %>% 
            full_join(benef.gas[,-3], by = c('chave', 'ano')) %>% 
            full_join(benef.trab_inf[,-3], by = c('chave', 'ano')) %>% 
            full_join(benef.trab_con[,-3], by = c('chave', 'ano')) %>% 
            full_join(benef.evi[,-3], by = c('chave', 'ano'))

# Reorganizando dados
bd.trans <- bd.trans %>% select(chave, ano, grupo, everything())

# Mudando nome das variáveis
for (i in 4:13) {
  names(bd.trans)[i] <- paste0('indicatriz.', var.names[i-3])
}

# Retirando NA's e organizando fatores do grupo
bd.trans <- bd.trans %>% drop_na()
bd.trans$grupo <- factor(bd.trans$grupo, labels = c('Domicilio com outro beneficio ou cadastrado', 'Domicilio sem beneficio e nao cadastrado', 'Domicilio com Bolsa Familia'))
bd.trans$grupo <- factor(bd.trans$grupo, levels = c('Domicilio com Bolsa Familia', 'Domicilio com outro beneficio ou cadastrado', 'Domicilio sem beneficio e nao cadastrado'))

# Transformando dados para nest
bd.trans <- bd.trans %>% group_by(ano) %>% nest()

# Tratando mesmas questões para o caso longitudinal
# Transformando dados para long
for (i in var.names) {
  assign(paste0('geral.', i), eval(parse(text = paste0('geral.', i))) %>% as_tibble() %>% select(chave, indicatriz = indicatriz.x, grupo = grupo.x) %>% mutate(ano = 0) %>% bind_rows(eval(parse(text = paste0('geral.', i))) %>% as_tibble() %>% select(chave, indicatriz = indicatriz.y, grupo = grupo.y) %>% mutate(ano = 1)))
}

# Unindo variáveis
bd.long <- geral.com %>% 
  inner_join(geral.rou_inf[,-3], by = c('chave', 'ano')) %>% 
  inner_join(geral.rou_con[,-3], by = c('chave', 'ano')) %>% 
  inner_join(geral.rou_fil[,-3], by = c('chave', 'ano')) %>% 
  inner_join(geral.est_fil[,-3], by = c('chave', 'ano')) %>% 
  inner_join(geral.sau_fil[,-3], by = c('chave', 'ano')) %>% 
  inner_join(geral.gas[,-3], by = c('chave', 'ano')) %>% 
  inner_join(geral.trab_inf[,-3], by = c('chave', 'ano')) %>% 
  inner_join(geral.trab_con[,-3], by = c('chave', 'ano')) %>% 
  inner_join(geral.evi[,-3], by = c('chave', 'ano'))

# Reordenando tabela
bd.long <- bd.long %>% select(chave, ano, grupo, everything())

# Alterando nomes
for (i in 4:13) {
  names(bd.long)[i] <- paste0('indicatriz.', var.names[i-3])
}

# Removendo NA's e reorganizando fatores
bd.long <- bd.long %>% drop_na()
bd.long$grupo <- factor(bd.long$grupo, labels = c('Domicilio com outro beneficio ou cadastrado', 'Domicilio sem beneficio e nao cadastrado', 'Domicilio com Bolsa Familia'))
bd.long$grupo <- factor(bd.long$grupo, levels = c('Domicilio com Bolsa Familia', 'Domicilio com outro beneficio ou cadastrado', 'Domicilio sem beneficio e nao cadastrado'))

# Transformando dados em nest
bd.long <- bd.long %>% group_by(ano) %>% nest()