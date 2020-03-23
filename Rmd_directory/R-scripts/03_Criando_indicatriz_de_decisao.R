################################## CRIANDO INDICATRIZ DE DECISÃO ##################################

#################################### ANO 2005 ####################################

# Indicatriz de decisão por variável
ind.com.2005 <- func.score2005(1)
ind.rou_inf.2005 <- func.score2005(2)
ind.rou_con.2005 <- func.score2005(3)
ind.rou_fil.2005 <- func.score2005(4)
ind.est_fil.2005 <- func.score2005(5)
ind.sau_fil.2005 <- func.score2005(6)
ind.gas.2005 <- func.score2005(7)
ind.trab_inf.2005 <- func.score2005(8)
ind.trab_con.2005 <- func.score2005(9)
ind.evi.2005 <- func.score2005(10)

# Criando Base auxiliar
aux.2005 <- AIBF.2005[, c(2,5)]
names(aux.2005) <- c('grupo', 'chave')

# Indicatriz de decisão com grupo que o domicílio pertence
benef.com.2005 <- merge(ind.com.2005, aux.2005, by = 'chave', all.x = T)
benef.com.2005 <- unique(benef.com.2005)
benef.rou_inf.2005 <- merge(ind.rou_inf.2005, aux.2005, by = 'chave', all.x = T)
benef.rou_inf.2005 <- unique(benef.rou_inf.2005)
benef.rou_con.2005 <- merge(ind.rou_con.2005, aux.2005, by = 'chave', all.x = T)
benef.rou_con.2005 <- unique(benef.rou_con.2005)
benef.rou_fil.2005 <- merge(ind.rou_fil.2005, aux.2005, by = 'chave', all.x = T)
benef.rou_fil.2005 <- unique(benef.rou_fil.2005)
benef.est_fil.2005 <- merge(ind.est_fil.2005, aux.2005, by = 'chave', all.x = T)
benef.est_fil.2005 <- unique(benef.est_fil.2005)
benef.sau_fil.2005 <- merge(ind.sau_fil.2005, aux.2005, by = 'chave', all.x = T)
benef.sau_fil.2005 <- unique(benef.sau_fil.2005)
benef.gas.2005 <- merge(ind.gas.2005, aux.2005, by = 'chave', all.x = T)
benef.gas.2005 <- unique(benef.gas.2005)
benef.trab_inf.2005 <- merge(ind.trab_inf.2005, aux.2005, by = 'chave', all.x = T)
benef.trab_inf.2005 <- unique(benef.trab_inf.2005)
benef.trab_con.2005 <- merge(ind.trab_con.2005, aux.2005, by = 'chave', all.x = T)
benef.trab_con.2005 <- unique(benef.trab_con.2005)
benef.evi.2005 <- merge(ind.evi.2005, aux.2005, by = 'chave', all.x = T)
benef.evi.2005 <- unique(benef.evi.2005)

#################################### ANO 2009 ####################################

# Indicatriz de decisão por variável
ind.com.2009 <- func.score2009(1)
ind.rou_inf.2009 <- func.score2009(2)
ind.rou_con.2009 <- func.score2009(3)
ind.rou_fil.2009 <- func.score2009(4)
ind.est_fil.2009 <- func.score2009(5)
ind.sau_fil.2009 <- func.score2009(6)
ind.gas.2009 <- func.score2009(7)
ind.trab_inf.2009 <- func.score2009(8)
ind.trab_con.2009 <- func.score2009(9)
ind.evi.2009 <- func.score2009(10)

# Criando Base auxiliar
aux.2009 <- AIBF.2009[, c(2,5)]
names(aux.2009) <- c('grupo', 'chave')

# Indicatriz de decisão com grupo que o domicílio pertence
benef.com.2009 <- merge(ind.com.2009, aux.2009, by = 'chave', all.x = T)
benef.com.2009 <- unique(benef.com.2009)
benef.rou_inf.2009 <- merge(ind.rou_inf.2009, aux.2009, by = 'chave', all.x = T)
benef.rou_inf.2009 <- unique(benef.rou_inf.2009)
benef.rou_con.2009 <- merge(ind.rou_con.2009, aux.2009, by = 'chave', all.x = T)
benef.rou_con.2009 <- unique(benef.rou_con.2009)
benef.rou_fil.2009 <- merge(ind.rou_fil.2009, aux.2009, by = 'chave', all.x = T)
benef.rou_fil.2009 <- unique(benef.rou_fil.2009)
benef.est_fil.2009 <- merge(ind.est_fil.2009, aux.2009, by = 'chave', all.x = T)
benef.est_fil.2009 <- unique(benef.est_fil.2009)
benef.sau_fil.2009 <- merge(ind.sau_fil.2009, aux.2009, by = 'chave', all.x = T)
benef.sau_fil.2009 <- unique(benef.sau_fil.2009)
benef.gas.2009 <- merge(ind.gas.2009, aux.2009, by = 'chave', all.x = T)
benef.gas.2009 <- unique(benef.gas.2009)
benef.trab_inf.2009 <- merge(ind.trab_inf.2009, aux.2009, by = 'chave', all.x = T)
benef.trab_inf.2009 <- unique(benef.trab_inf.2009)
benef.trab_con.2009 <- merge(ind.trab_con.2009, aux.2009, by = 'chave', all.x = T)
benef.trab_con.2009 <- unique(benef.trab_con.2009)
benef.evi.2009 <- merge(ind.evi.2009, aux.2009, by = 'chave', all.x = T)
benef.evi.2009 <- unique(benef.evi.2009)

# Unificando os anos de 2005 e 2009
geral.com <- merge(benef.com.2005, benef.com.2009, by = 'chave')
geral.rou_inf <- merge(benef.rou_inf.2005, benef.rou_inf.2009, by = 'chave')
geral.rou_con <- merge(benef.rou_con.2005, benef.rou_con.2009, by = 'chave')
geral.rou_fil <- merge(benef.rou_fil.2005, benef.rou_fil.2009, by = 'chave')
geral.est_fil <- merge(benef.est_fil.2005, benef.est_fil.2009, by = 'chave')
geral.sau_fil <- merge(benef.sau_fil.2005, benef.sau_fil.2009, by = 'chave')
geral.gas <- merge(benef.gas.2005, benef.gas.2009, by = 'chave')
geral.trab_inf <- merge(benef.trab_inf.2005, benef.trab_inf.2009, by = 'chave')
geral.trab_con <- merge(benef.trab_con.2005, benef.trab_con.2009, by = 'chave')
geral.evi <- merge(benef.evi.2005, benef.evi.2009, by = 'chave')