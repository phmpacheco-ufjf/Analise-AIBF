################################## IMPORTANDO BASE DE DADOS ##################################

#################################### ANO 2005 ####################################

# Importando base de dados
AIBF.2005 <- read.dta('G:/Meu Drive/Meu Drive/Marcel/AIBF/AIBF - Final/AIBF_2005.dta')

# Label das variáveis do banco de dados
label.2005 <- attr(AIBF.2005,"var.labels")

# Acrescentando label ao banco de dados
labels(AIBF.2005) <- label.2005

# Criando vetor com label e nome da variável
label.2005 <- labels(AIBF.2005)
label.2005 <- name_rows(as.data.frame(label.2005))
colnames(label.2005) <- c('Descrição', 'Variável')

#################################### ANO 2009 ####################################

# Importando base de dados
AIBF.2009 <- read.dta('G:/Meu Drive/Meu Drive/Marcel/AIBF/AIBF - Final/AIBF_2009.dta')

# Label das variáveis do banco de dados
label.2009 <- attr(AIBF.2009,"var.labels")

# Acrescentando label ao banco de dados
labels(AIBF.2009) <- label.2009

# Criando vetor com label e nome da variável
label.2009 <- labels(AIBF.2009)
label.2009 <- name_rows(as.data.frame(label.2009))
colnames(label.2009) <- c('Descrição', 'Variável')
