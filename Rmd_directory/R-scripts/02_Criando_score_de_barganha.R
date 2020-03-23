################################## CRIANDO SCORE DE BARGANHA ##################################

#################################### ANO 2005 ####################################

# Base de dados inicial para criação do score de barganha
score_barganha.inicial <- AIBF.2005[, c(
  'chave', # Identificador do domicílio em 2005
  's11b_nor', # Número do Informante
  
  's11b3001', # Informante toma as decisões com gastos de comida
  's11b4001', # Conjuge toma as decisões com gastos de comida
  's11b01c_com', # Ambos tomam as decisões com gastos de comida
  's11b5001', # Outro toma as decisões com gastos de comida
  's11b6001', # Não sabe quem toma as decisões com gastos de comida
  
  's11b3002', # Informante toma as decisões sobre roupas para informante
  's11b4002', # Conjuge toma as decisões sobre roupas para informante
  's11b02c_rou', # Ambos tomam as decisões sobre roupas para informante
  's11b5002', # Outro toma as decisões sobre roupas para informante
  's11b6002', # Não sabe quem toma as decisões sobre roupas para informante
  
  's11b3003', # Informante toma as decisões sobre roupas para conjugue
  's11b4003', # Conjuge toma as decisões sobre roupas para conjugue
  's11b03c_rou', # Ambos tomam as decisões sobre roupas para conjugue
  's11b5003', # Outro toma as decisões sobre roupas para conjugue
  's11b6003', # Não sabe quem toma as decisões sobre roupas para conjugue
  
  's11b3004', # Informante toma as decisões sobre roupas para filhos
  's11b4004', # Conjuge toma as decisões sobre roupas para filhos
  's11b04c_rou', # Ambos tomam as decisões sobre roupas para filhos
  's11b5004', # Outro toma as decisões sobre roupas para filhos
  's11b6004', # Não sabe quem toma as decisões sobre roupas para informante
  's11b7004', # Não tem filhos
  
  's11b3005', # Informante toma as decisões sobre quando filhos param de estudar
  's11b4005', # Conjuge toma as decisões sobre quando filhos param de estudar
  's11b05c_qua', # Ambos tomam as decisões sobre quando filhos param de estudar
  's11b5005', # Outro toma as decisões sobre quando filhos param de estudar
  's11b6005', # Não sabe quem toma as decisões sobre quando filhos param de estudar
  's11b7005', # Não tem filhos
  
  's11b3006', # Informante toma as decisões sobre saude e remedio para filhos
  's11b4006', # Conjuge toma as decisões sobre saude e remedio para filhos
  's11b06c_sau', # Ambos tomam as decisões sobre saude e remedio para filhos
  's11b5006', # Outro toma as decisões sobre saude e remedio para filhos
  's11b6006', # Não sabe quem toma as decisões sobre saude e remedio para informante
  's11b7006', # Não tem filhos
  
  's11b3007', # Informante toma as decisões sobre gastos com bens duraveis
  's11b4007', # Conjuge toma as decisões sobre gastos com bens duraveis
  's11b07c_ben', # Ambos tomam as decisões sobre gastos com bens duraveis
  's11b5007', # Outro toma as decisões sobre gastos com bens duraveis
  's11b6007', # Não sabe quem toma as decisões sobre gastos com bens duraveis
  
  's11b3008', # Informante toma as decisões sobre informante deve trabalhar
  's11b4008', # Conjuge toma as decisões sobre informante deve trabalhar
  's11b08c_se', # Ambos tomam as decisões sobre informante deve trabalhar
  's11b5008', # Outro toma as decisões sobre informante deve trabalhar
  's11b6008', # Não sabe quem toma as decisões sobre informante deve trabalhar
  
  's11b3009', # Informante toma as decisões sobre conjugue deve trabalhar
  's11b4009', # Conjuge toma as decisões sobre conjugue deve trabalhar
  's11b09c_se', # Ambos tomam as decisões sobre conjugue deve trabalhar
  's11b5009', # Outro toma as decisões sobre conjugue deve trabalhar
  's11b6009', # Não sabe quem toma as decisões sobre conjugue deve trabalhar
  
  's11b3010', # Informante toma as decisões sobre metodos para evitar filhos
  's11b4010', # Conjuge toma as decisões sobre metodos para evitar filhos
  's11b10c_met', # Ambos tomam as decisões sobre metodos para evitar filhos
  's11b5010', # Outro toma as decisões sobre metodos para evitar filhos
  's11b6010' # Não sabe quem toma as decisões sobre metodos para evitar filhos
)]

# Label das variáveis do score de barganha
label.score_barganha <- labels(score_barganha.inicial)
label.score_barganha <- name_rows(as.data.frame(label.score_barganha))
colnames(label.score_barganha) <- c('Descrição', 'Variável')

# Unificando respostas por domicílio
score_barganha <- unique(score_barganha.inicial)

# Verificando unicidade dos domicílios
length(unique(score_barganha.inicial$chave))

# Verificando unicidade das respostas
ver.comida <- verifica.miss(score_barganha$s11b3001,
                            score_barganha$s11b4001,
                            score_barganha$s11b01c_com,
                            score_barganha$s11b5001,
                            score_barganha$s11b6001)

ver.roupas.inf <- verifica.miss(score_barganha$s11b3002,
                                score_barganha$s11b4002,
                                score_barganha$s11b02c_rou,
                                score_barganha$s11b5002,
                                score_barganha$s11b6002)

ver.roupas.con <- verifica.miss(score_barganha$s11b3003,
                                score_barganha$s11b4003,
                                score_barganha$s11b03c_rou,
                                score_barganha$s11b5003,
                                score_barganha$s11b6003)

ver.roupas.fil <- verifica.miss(score_barganha$s11b3004,
                                score_barganha$s11b4004,
                                score_barganha$s11b04c_rou,
                                score_barganha$s11b5004,
                                score_barganha$s11b6004,
                                score_barganha$s11b7004)

ver.estudo.fil <- verifica.miss(score_barganha$s11b3005,
                                score_barganha$s11b4005,
                                score_barganha$s11b05c_qua,
                                score_barganha$s11b5005,
                                score_barganha$s11b6005,
                                score_barganha$s11b7005)

ver.saude.fil <- verifica.miss(score_barganha$s11b3006,
                               score_barganha$s11b4006,
                               score_barganha$s11b06c_sau,
                               score_barganha$s11b5006,
                               score_barganha$s11b6006,
                               score_barganha$s11b7006)

ver.gastos <- verifica.miss(score_barganha$s11b3007,
                            score_barganha$s11b4007,
                            score_barganha$s11b07c_ben,
                            score_barganha$s11b5007,
                            score_barganha$s11b6007)

ver.trabalho.inf <- verifica.miss(score_barganha$s11b3008,
                                  score_barganha$s11b4008,
                                  score_barganha$s11b08c_se,
                                  score_barganha$s11b5008,
                                  score_barganha$s11b6008)

ver.trabalho.con <- verifica.miss(score_barganha$s11b3009,
                                  score_barganha$s11b4009,
                                  score_barganha$s11b09c_se,
                                  score_barganha$s11b5009,
                                  score_barganha$s11b6009)

ver.evitar.fil <- verifica.miss(score_barganha$s11b3010,
                                score_barganha$s11b4010,
                                score_barganha$s11b10c_met,
                                score_barganha$s11b5010,
                                score_barganha$s11b6010)

# Criando banco de dados auxiliar
aux.db <- AIBF.2005[, c('chave', 's02ad', 'npes')]
aux.db[aux.db[, 2] == 'sd', 2] <- NA
aux.db$s02ad <- factor(aux.db$s02ad)

# Definindo sexo do informante
for (i in 1:dim(score_barganha)[1]) {
  aux <- sexo.inf(aux.db, score_barganha[i,1], score_barganha[i,2])
  score_barganha[score_barganha[, 1] == aux[1], 56] <- aux[2]
  rm(aux)
}
names(score_barganha)[56] <- 'Sexo informante'
score_barganha$`Sexo informante` <- factor(score_barganha$`Sexo informante`, levels = c(1, 2), labels = c('Masculino', 'Femenino'))
describe(score_barganha$`Sexo informante`)

# Restringindo banco de score a informantes do sexo Femenino
score.fem <- subset(score_barganha, score_barganha$`Sexo informante` == 'Femenino')

# Verificando unicidade do banco de dados restrito
ver.fem.comida <- verifica.miss(score.fem$s11b3001,
                                score.fem$s11b4001,
                                score.fem$s11b01c_com,
                                score.fem$s11b5001,
                                score.fem$s11b6001)

ver.fem.roupas.inf <- verifica.miss(score.fem$s11b3002,
                                    score.fem$s11b4002,
                                    score.fem$s11b02c_rou,
                                    score.fem$s11b5002,
                                    score.fem$s11b6002)

ver.fem.roupas.con <- verifica.miss(score.fem$s11b3003,
                                    score.fem$s11b4003,
                                    score.fem$s11b03c_rou,
                                    score.fem$s11b5003,
                                    score.fem$s11b6003)

ver.fem.roupas.fil <- verifica.miss(score.fem$s11b3004,
                                    score.fem$s11b4004,
                                    score.fem$s11b04c_rou,
                                    score.fem$s11b5004,
                                    score.fem$s11b6004,
                                    score.fem$s11b7004)

ver.fem.estudo.fil <- verifica.miss(score.fem$s11b3005,
                                    score.fem$s11b4005,
                                    score.fem$s11b05c_qua,
                                    score.fem$s11b5005,
                                    score.fem$s11b6005,
                                    score.fem$s11b7005)

ver.fem.saude.fil <- verifica.miss(score.fem$s11b3006,
                                   score.fem$s11b4006,
                                   score.fem$s11b06c_sau,
                                   score.fem$s11b5006,
                                   score.fem$s11b6006,
                                   score.fem$s11b7006)

ver.fem.gastos <- verifica.miss(score.fem$s11b3007,
                                score.fem$s11b4007,
                                score.fem$s11b07c_ben,
                                score.fem$s11b5007,
                                score.fem$s11b6007)

ver.fem.trabalho.inf <- verifica.miss(score.fem$s11b3008,
                                      score.fem$s11b4008,
                                      score.fem$s11b08c_se,
                                      score.fem$s11b5008,
                                      score.fem$s11b6008)

ver.fem.trabalho.con <- verifica.miss(score.fem$s11b3009,
                                      score.fem$s11b4009,
                                      score.fem$s11b09c_se,
                                      score.fem$s11b5009,
                                      score.fem$s11b6009)

ver.fem.evitar.fil <- verifica.miss(score.fem$s11b3010,
                                    score.fem$s11b4010,
                                    score.fem$s11b10c_met,
                                    score.fem$s11b5010,
                                    score.fem$s11b6010)

#################################### ANO 2009 ####################################

# Base de dados inicial para criação do score de barganha
score_barganha.inicial.2 <- AIBF.2009[, c(
  'chave', # Identificador do domicílio em 2005
  's11b_nor', # Número do Informante
  
  's11b3001', # Informante toma as decisões com gastos de comida
  's11b4001', # Conjuge toma as decisões com gastos de comida
  's11b5001', # Outro toma as decisões com gastos de comida
  's11b6001', # Não sabe quem toma as decisões com gastos de comida
  's11b7001', # Não tem filhos
  
  's11b3002', # Informante toma as decisões sobre roupas para informante
  's11b4002', # Conjuge toma as decisões sobre roupas para informante
  's11b5002', # Outro toma as decisões sobre roupas para informante
  's11b6002', # Não sabe quem toma as decisões sobre roupas para informante
  's11b7002', # Não tem filhos
  
  's11b3003', # Informante toma as decisões sobre roupas para conjugue
  's11b4003', # Conjuge toma as decisões sobre roupas para conjugue
  's11b5003', # Outro toma as decisões sobre roupas para conjugue
  's11b6003', # Não sabe quem toma as decisões sobre roupas para conjugue
  's11b7003', # Não tem filhos
  
  's11b3004', # Informante toma as decisões sobre roupas para filhos
  's11b4004', # Conjuge toma as decisões sobre roupas para filhos
  's11b5004', # Outro toma as decisões sobre roupas para filhos
  's11b6004', # Não sabe quem toma as decisões sobre roupas para informante
  's11b7004', # Não tem filhos
  
  's11b3005', # Informante toma as decisões sobre quando filhos param de estudar
  's11b4005', # Conjuge toma as decisões sobre quando filhos param de estudar
  's11b5005', # Outro toma as decisões sobre quando filhos param de estudar
  's11b6005', # Não sabe quem toma as decisões sobre quando filhos param de estudar
  's11b7005', # Não tem filhos
  
  's11b3006', # Informante toma as decisões sobre saude e remedio para filhos
  's11b4006', # Conjuge toma as decisões sobre saude e remedio para filhos
  's11b5006', # Outro toma as decisões sobre saude e remedio para filhos
  's11b6006', # Não sabe quem toma as decisões sobre saude e remedio para informante
  's11b7006', # Não tem filhos
  
  's11b3007', # Informante toma as decisões sobre gastos com bens duraveis
  's11b4007', # Conjuge toma as decisões sobre gastos com bens duraveis
  's11b5007', # Outro toma as decisões sobre gastos com bens duraveis
  's11b6007', # Não sabe quem toma as decisões sobre gastos com bens duraveis
  's11b7007', # Não tem filhos
  
  's11b3008', # Informante toma as decisões sobre informante deve trabalhar
  's11b4008', # Conjuge toma as decisões sobre informante deve trabalhar
  's11b5008', # Outro toma as decisões sobre informante deve trabalhar
  's11b6008', # Não sabe quem toma as decisões sobre informante deve trabalhar
  's11b7008', # Não tem filhos
  
  's11b3009', # Informante toma as decisões sobre conjugue deve trabalhar
  's11b4009', # Conjuge toma as decisões sobre conjugue deve trabalhar
  's11b5009', # Outro toma as decisões sobre conjugue deve trabalhar
  's11b6009', # Não sabe quem toma as decisões sobre conjugue deve trabalhar
  's11b7009', # Não tem filhos
  
  's11b3010', # Informante toma as decisões sobre metodos para evitar filhos
  's11b4010', # Conjuge toma as decisões sobre metodos para evitar filhos
  's11b5010', # Outro toma as decisões sobre metodos para evitar filhos
  's11b6010', # Não sabe quem toma as decisões sobre metodos para evitar filhos
  's11b7010' # Não tem filhos
)]

# Label das variáveis do score de barganha
label.score_barganha.2 <- labels(score_barganha.inicial.2)
label.score_barganha.2 <- name_rows(as.data.frame(label.score_barganha.2))
colnames(label.score_barganha.2) <- c('Descrição', 'Variável')

# Unificando respostas por domicílio
score_barganha.2 <- unique(score_barganha.inicial.2)

# Verificando unicidade dos domicílios
length(unique(score_barganha.inicial.2$chave))

# Note que as variáveis do tipo 7000 não possuem valores a não ser que estejam relacionadas a filhos.
describe(score_barganha.2$s11b7001)
describe(score_barganha.2$s11b7002)
describe(score_barganha.2$s11b7003)
describe(score_barganha.2$s11b7004)
describe(score_barganha.2$s11b7005)
describe(score_barganha.2$s11b7006)
describe(score_barganha.2$s11b7007)
describe(score_barganha.2$s11b7008)
describe(score_barganha.2$s11b7009)
describe(score_barganha.2$s11b7010)

# Assim, podemos retirá-las do score.
score_barganha.2 <- score_barganha.2[, c(-7, -12, -17, -37, -42, -47, -52)]

# Verificando unicidade das respostas
ver.comida.2 <- verifica.miss.2(score_barganha.2$s11b3001,
                                score_barganha.2$s11b4001,
                                score_barganha.2$s11b5001,
                                score_barganha.2$s11b6001)

ver.roupas.inf.2 <- verifica.miss.2(score_barganha.2$s11b3002,
                                    score_barganha.2$s11b4002,
                                    score_barganha.2$s11b5002,
                                    score_barganha.2$s11b6002)

ver.roupas.con.2 <- verifica.miss.2(score_barganha.2$s11b3003,
                                    score_barganha.2$s11b4003,
                                    score_barganha.2$s11b5003,
                                    score_barganha.2$s11b6003)

ver.roupas.fil.2 <- verifica.miss.2(score_barganha.2$s11b3004,
                                    score_barganha.2$s11b4004,
                                    score_barganha.2$s11b5004,
                                    score_barganha.2$s11b6004,
                                    score_barganha.2$s11b7004)

ver.estudo.fil.2 <- verifica.miss.2(score_barganha.2$s11b3005,
                                    score_barganha.2$s11b4005,
                                    score_barganha.2$s11b5005,
                                    score_barganha.2$s11b6005,
                                    score_barganha.2$s11b7005)

ver.saude.fil.2 <- verifica.miss.2(score_barganha.2$s11b3006,
                                   score_barganha.2$s11b4006,
                                   score_barganha.2$s11b5006,
                                   score_barganha.2$s11b6006,
                                   score_barganha.2$s11b7006)

ver.gastos.2 <- verifica.miss.2(score_barganha.2$s11b3007,
                                score_barganha.2$s11b4007,
                                score_barganha.2$s11b5007,
                                score_barganha.2$s11b6007)

ver.trabalho.inf.2 <- verifica.miss.2(score_barganha.2$s11b3008,
                                      score_barganha.2$s11b4008,
                                      score_barganha.2$s11b5008,
                                      score_barganha.2$s11b6008)

ver.trabalho.con.2 <- verifica.miss.2(score_barganha.2$s11b3009,
                                      score_barganha.2$s11b4009,
                                      score_barganha.2$s11b5009,
                                      score_barganha.2$s11b6009)

ver.evitar.fil.2 <- verifica.miss.2(score_barganha.2$s11b3010,
                                    score_barganha.2$s11b4010,
                                    score_barganha.2$s11b5010,
                                    score_barganha.2$s11b6010)

# Entendendo um pouco mais das variáveis com a opção Não tem filhos
describe(score_barganha.2$s11b7004)
describe(score_barganha.2$s11b7005)
describe(score_barganha.2$s11b7006)

# Pergunta 4 caso 1: onde não houve resposta em Não tem filhos
p4.caso1 <- score_barganha.2[is.na(score_barganha.2$s11b7004), 15:19]
describe(p4.caso1) # Observamos que existem respostas "sim" e "não"

# Pergunta 4 caso 2: onde houve resposta "não" em Não tem filhos
p4.caso2 <- score_barganha.2[score_barganha.2$s11b7004 == 'Nao', 15:19]
describe(p4.caso2) # Observamos que existem respostas "sim" e "não"

# Pergunta 4 caso 3: onde houve resposta "sim" em Não tem filhos
p4.caso3 <- score_barganha.2[score_barganha.2$s11b7004 == 'Sim', 15:19]
describe(p4.caso3) # Observamos que existem respostas "sim" e "não"

# Banco de dados auxiliar para determinar sexo do informante e conjuge
aux.db.2 <- AIBF.2009[, c('chave', 's02ad', 'npes')]

# Definindo sexo do informante e conjuge
for (i in 1:dim(score_barganha.2)[1]) {
  aux <- sexo.inf(aux.db.2, score_barganha.2[i,1], score_barganha.2[i,2])
  score_barganha.2[score_barganha.2[, 1] == aux[1], 46] <- aux[2]
  rm(aux)
}
names(score_barganha.2)[46] <- 'Sexo informante'
score_barganha.2$`Sexo informante` <- factor(score_barganha.2$`Sexo informante`, levels = c(1, 2), labels = c('Masculino', 'Femenino'))

# Analise da variável criada
describe(score_barganha.2$`Sexo informante`)

# Restringindo banco de score a informantes do sexo Femenino
score.fem.2 <- subset(score_barganha.2, score_barganha.2$`Sexo informante` == 'Femenino')
