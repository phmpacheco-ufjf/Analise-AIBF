################################## FUNÇÕES AUXILIARES E PACOTES NECESSÁRIOS ##################################

# Pacotes necessários
library(Hmisc)
library(foreign)
library(geepack)
library(papeR)
library(plyr)

verifica.miss <- function(inf, con, amb, out, nse, ntf = NA) {
  cond <- is.factor(ntf)
  if(cond == TRUE) {
    ind.inf <- ifelse(is.na(inf), 0, 1)
    ind.con <- ifelse(is.na(con), 0, 1)
    ind.amb <- ifelse(is.na(amb), 0, 1)
    ind.out <- ifelse(is.na(out), 0, 1)
    ind.nse <- ifelse(is.na(nse), 0, 1)
    ind.ntf <- ifelse(is.na(ntf), 0, 1)
    
    x <- c('inf', 'con', 'amb', 'out', 'nse', 'ntf')
    comb66 <- combn(x, 6)
    comb65 <- combn(x, 5)
    comb64 <- combn(x, 4)
    comb63 <- combn(x, 3)
    comb62 <- combn(x, 2)
    
    soma66 <- list()
    soma65 <- list()
    soma64 <- list()
    soma63 <- list()
    soma62 <- list()
    for (i in 1:6) {
      if(i == 1) {
        soma66[[1]] <- eval(parse(text = paste0('ind.', comb66[i,1])))
      } else {
        soma66[[1]] <- soma66[[1]] + eval(parse(text = paste0('ind.', comb66[i,1])))
      }
      for (j in 1:5) {
        if(j == 1) {
          soma65[[i]] <- eval(parse(text = paste0('ind.', comb65[j,i])))
        } else {
          soma65[[i]] <- soma65[[i]] + eval(parse(text = paste0('ind.', comb65[j,i])))
        }
      }
      soma65[[i]] <- describe(factor(soma65[[i]]))$values
      names(soma65)[i] <- paste(comb65[,i], collapse = " ")
    }
    soma66[[1]] <- describe(factor(soma66[[1]]))$values
    names(soma66)[1] <- paste(comb66[,1], collapse = " ")
    for (i in 1:15) {
      for (j in 1:4) {
        if(j == 1) {
          soma64[[i]] <- eval(parse(text = paste0('ind.', comb64[j,i])))
        } else {
          soma64[[i]] <- soma64[[i]] + eval(parse(text = paste0('ind.', comb64[j,i])))
        }
      }
      soma64[[i]] <- describe(factor(soma64[[i]]))$values
      names(soma64)[i] <- paste(comb64[,i], collapse = " ")
      for (j in 1:2) {
        if(j == 1) {
          soma62[[i]] <- eval(parse(text = paste0('ind.', comb62[j,i])))
        } else {
          soma62[[i]] <- soma62[[i]] + eval(parse(text = paste0('ind.', comb62[j,i])))
        }
      }
      soma62[[i]] <- describe(factor(soma62[[i]]))$values
      names(soma62)[i] <- paste(comb62[,i], collapse = " ")
    }
    for (i in 1:20) {
      for (j in 1:3) {
        if(j == 1) {
          soma63[[i]] <- eval(parse(text = paste0('ind.', comb63[j,i])))
        } else {
          soma63[[i]] <- soma63[[i]] + eval(parse(text = paste0('ind.', comb63[j,i])))
        }
      }
      soma63[[i]] <- describe(factor(soma63[[i]]))$values
      names(soma63)[i] <- paste(comb63[,i], collapse = " ")
    }
    return(c(soma66, soma65, soma64, soma63, soma62))
  } else {
    ind.inf <- ifelse(is.na(inf), 0, 1)
    ind.con <- ifelse(is.na(con), 0, 1)
    ind.amb <- ifelse(is.na(amb), 0, 1)
    ind.out <- ifelse(is.na(out), 0, 1)
    ind.nse <- ifelse(is.na(nse), 0, 1)
    
    x <- c('inf', 'con', 'amb', 'out', 'nse')
    comb55 <- combn(x, 5)
    comb54 <- combn(x, 4)
    comb53 <- combn(x, 3)
    comb52 <- combn(x, 2)
    
    soma55 <- list()
    soma54 <- list()
    soma53 <- list()
    soma52 <- list()
    for (i in 1:5) {
      if(i == 1) {
        soma55[[1]] <- eval(parse(text = paste0('ind.', comb55[i,1])))
      } else {
        soma55[[1]] <- soma55[[1]] + eval(parse(text = paste0('ind.', comb55[i,1])))
      }
      for (j in 1:4) {
        if(j == 1) {
          soma54[[i]] <- eval(parse(text = paste0('ind.', comb54[j,i])))
        } else {
          soma54[[i]] <- soma54[[i]] + eval(parse(text = paste0('ind.', comb54[j,i])))
        }
      }
      soma54[[i]] <- describe(factor(soma54[[i]]))$values
      names(soma54)[i] <- paste(comb54[,i], collapse = " ")
    }
    soma55[[1]] <- describe(factor(soma55[[1]]))$values
    names(soma55)[1] <- paste(comb55[,1], collapse = " ")
    for (i in 1:10) {
      for (j in 1:3) {
        if(j == 1) {
          soma53[[i]] <- eval(parse(text = paste0('ind.', comb53[j,i])))
        } else {
          soma53[[i]] <- soma53[[i]] + eval(parse(text = paste0('ind.', comb53[j,i])))
        }
      }
      soma53[[i]] <- describe(factor(soma53[[i]]))$values
      names(soma53)[i] <- paste(comb53[,i], collapse = " ")
      for (j in 1:2) {
        if(j == 1) {
          soma52[[i]] <- eval(parse(text = paste0('ind.', comb52[j,i])))
        } else {
          soma52[[i]] <- soma52[[i]] + eval(parse(text = paste0('ind.', comb52[j,i])))
        }
      }
      soma52[[i]] <- describe(factor(soma52[[i]]))$values
      names(soma52)[i] <- paste(comb52[,i], collapse = " ")
    }
    return(c(soma55, soma54, soma53, soma52))
  }
}

verifica.miss.2 <- function(inf, con, out, nse, ntf = NA) {
  cond <- is.factor(ntf)
  if(cond == TRUE) {
    ind.inf <- ifelse(is.na(inf), 0, 1)
    ind.con <- ifelse(is.na(con), 0, 1)
    ind.out <- ifelse(is.na(out), 0, 1)
    ind.nse <- ifelse(is.na(nse), 0, 1)
    ind.ntf <- ifelse(is.na(ntf), 0, 1)
    
    x <- c('inf', 'con', 'out', 'nse', 'ntf')
    comb55 <- combn(x, 5)
    comb54 <- combn(x, 4)
    comb53 <- combn(x, 3)
    comb52 <- combn(x, 2)
    
    soma55 <- list()
    soma54 <- list()
    soma53 <- list()
    soma52 <- list()
    for (i in 1:5) {
      if(i == 1) {
        soma55[[1]] <- eval(parse(text = paste0('ind.', comb55[i,1])))
      } else {
        soma55[[1]] <- soma55[[1]] + eval(parse(text = paste0('ind.', comb55[i,1])))
      }
      for (j in 1:4) {
        if(j == 1) {
          soma54[[i]] <- eval(parse(text = paste0('ind.', comb54[j,i])))
        } else {
          soma54[[i]] <- soma54[[i]] + eval(parse(text = paste0('ind.', comb54[j,i])))
        }
      }
      soma54[[i]] <- describe(factor(soma54[[i]]))$values
      names(soma54)[i] <- paste(comb54[,i], collapse = " ")
    }
    soma55[[1]] <- describe(factor(soma55[[1]]))$values
    names(soma55)[1] <- paste(comb55[,1], collapse = " ")
    for (i in 1:10) {
      for (j in 1:3) {
        if(j == 1) {
          soma53[[i]] <- eval(parse(text = paste0('ind.', comb53[j,i])))
        } else {
          soma53[[i]] <- soma53[[i]] + eval(parse(text = paste0('ind.', comb53[j,i])))
        }
      }
      soma53[[i]] <- describe(factor(soma53[[i]]))$values
      names(soma53)[i] <- paste(comb53[,i], collapse = " ")
      for (j in 1:2) {
        if(j == 1) {
          soma52[[i]] <- eval(parse(text = paste0('ind.', comb52[j,i])))
        } else {
          soma52[[i]] <- soma52[[i]] + eval(parse(text = paste0('ind.', comb52[j,i])))
        }
      }
      soma52[[i]] <- describe(factor(soma52[[i]]))$values
      names(soma52)[i] <- paste(comb52[,i], collapse = " ")
    }
    return(c(soma55, soma54, soma53, soma52))
  } else {
    ind.inf <- ifelse(is.na(inf), 0, 1)
    ind.con <- ifelse(is.na(con), 0, 1)
    ind.out <- ifelse(is.na(out), 0, 1)
    ind.nse <- ifelse(is.na(nse), 0, 1)
    
    x <- c('inf', 'con', 'out', 'nse')
    comb44 <- combn(x, 4)
    comb43 <- combn(x, 3)
    comb42 <- combn(x, 2)
    
    soma44 <- list()
    soma43 <- list()
    soma42 <- list()
    for (i in 1:4) {
      if(i == 1) {
        soma44[[1]] <- eval(parse(text = paste0('ind.', comb44[i,1])))
      } else {
        soma44[[1]] <- soma44[[1]] + eval(parse(text = paste0('ind.', comb44[i,1])))
      }
      for (j in 1:3) {
        if(j == 1) {
          soma43[[i]] <- eval(parse(text = paste0('ind.', comb43[j,i])))
        } else {
          soma43[[i]] <- soma43[[i]] + eval(parse(text = paste0('ind.', comb43[j,i])))
        }
      }
      soma43[[i]] <- describe(factor(soma43[[i]]))$values
      names(soma43)[i] <- paste(comb43[,i], collapse = " ")
    }
    soma44[[1]] <- describe(factor(soma44[[1]]))$values
    names(soma44)[1] <- paste(comb44[,1], collapse = " ")
    for (i in 1:6) {
      for (j in 1:2) {
        if(j == 1) {
          soma42[[i]] <- eval(parse(text = paste0('ind.', comb42[j,i])))
        } else {
          soma42[[i]] <- soma42[[i]] + eval(parse(text = paste0('ind.', comb42[j,i])))
        }
      }
      soma42[[i]] <- describe(factor(soma42[[i]]))$values
      names(soma42)[i] <- paste(comb42[,i], collapse = " ")
    }
    return(c(soma44, soma43, soma42))
  }
}

sexo.inf <- function(db.aux, chave, nor = NA) {
  x <- array()
  x[1] <- chave
  if(is.na(nor)) {
    x[2] <- NA
  } else {
    if(!(nor %in% db.aux[db.aux[, 1] == chave, 3])) {
      x[2] <- NA
      return(x)
    }
    x[2] <- db.aux[db.aux[, 1] == chave & db.aux[, 3] == nor, 2]
  }
  return(x)
}

func.score2005 <- function(x) {
  a1 <- 3000 + x
  a2 <- 4000 + x
  a3 <- 5000 + x
  a4 <- 6000 + x
  a5 <- 7000 + x
  
  b1 <- paste0('score.fem$s11b', a1)
  b2 <- paste0('score.fem$s11b', a2)
  b3 <- paste0('score.fem$s11b', a3)
  b4 <- paste0('score.fem$s11b', a4)
  b5 <- paste0('score.fem$s11b', a5)
  
  aux <- c('01c_com', '02c_rou', '03c_rou', '04c_rou', '05c_qua', '06c_sau', '07c_ben', '08c_se', '09c_se', '10c_met')
  
  b6 <- paste0('score.fem$s11b', aux[x])
  
  w <- matrix(NA, nrow = dim(score.fem)[1], ncol = 2)
  w[, 1] <- score.fem$chave
  if(x == 4 | x == 5 | x == 6) {
    w[, 2] <- ifelse(is.na(eval(parse(text = b1))) & is.na(eval(parse(text = b2)))
                     & is.na(eval(parse(text = b3))) & is.na(eval(parse(text = b4)))
                     & is.na(eval(parse(text = b5))) & is.na(eval(parse(text = b6))), -1, NA)
  } else {
    w[, 2] <- ifelse(is.na(eval(parse(text = b1))) & is.na(eval(parse(text = b2)))
                     & is.na(eval(parse(text = b3))) & is.na(eval(parse(text = b4)))
                     & is.na(eval(parse(text = b6))), -1, NA)
  }
  w <- w[is.na(w[,2]),]
  w <- as.data.frame(w)
  names(w)[1] <- 'chave'
  w <- merge(w, score.fem, by = 'chave', all.x = T)
  w <- w[,-2]
  
  c1 <- paste0('w$s11b', a1)
  c2 <- paste0('w$s11b', a2)
  c3 <- paste0('w$s11b', a3)
  c4 <- paste0('w$s11b', a4)
  c5 <- paste0('w$s11b', a5)
  
  c6 <- paste0('w$s11b', aux[x])
  
  if(x == 4 | x == 5 | x == 6) {
    z <- matrix(NA, nrow = dim(w)[1], ncol = 8)
    z[, 1] <- w$chave
    
    z[, 2] <- ifelse(eval(parse(text = c1)) == 'sim', 1, 0)
    z[, 3] <- ifelse(eval(parse(text = c2)) == 'sim', 1, 0)
    z[, 4] <- ifelse(eval(parse(text = c3)) == 'sim', 1, 0)
    z[, 5] <- ifelse(eval(parse(text = c4)) == 'sim', 1, 0)
    z[, 6] <- ifelse(eval(parse(text = c5)) == 'sim', 1, 0)
    z[, 7] <- ifelse(eval(parse(text = c6)) == 'sim', 1, 0)
    
    z <- as.data.frame(z)
    
    z[is.na(z[,2]),2] <- 0
    z[is.na(z[,3]),3] <- 0
    z[is.na(z[,4]),4] <- 0
    z[is.na(z[,5]),5] <- 0
    z[is.na(z[,6]),6] <- 0
    z[is.na(z[,7]),7] <- 0
    
    z[,8] <- apply(z[,2:7], 1, sum)
    z[,9] <- NA
    z[,9] <- ifelse(z$V2 == 1 & z$V8 == 1, 1, 0)
    
    z <- z[,c(1, 9)]
    names(z) <- c('chave', 'indicatriz')
  } else {
    z <- matrix(NA, nrow = dim(w)[1], ncol = 7)
    z[, 1] <- w$chave
    
    z[, 2] <- ifelse(eval(parse(text = c1)) == 'sim', 1, 0)
    z[, 3] <- ifelse(eval(parse(text = c2)) == 'sim', 1, 0)
    z[, 4] <- ifelse(eval(parse(text = c3)) == 'sim', 1, 0)
    z[, 5] <- ifelse(eval(parse(text = c4)) == 'sim', 1, 0)
    z[, 6] <- ifelse(eval(parse(text = c6)) == 'sim', 1, 0)
    
    z <- as.data.frame(z)
    
    z[is.na(z[,2]),2] <- 0
    z[is.na(z[,3]),3] <- 0
    z[is.na(z[,4]),4] <- 0
    z[is.na(z[,5]),5] <- 0
    z[is.na(z[,6]),6] <- 0
    
    z[,7] <- apply(z[,2:6], 1, sum)
    z[,8] <- NA
    z[,8] <- ifelse(z$V2 == 1 & z$V7 == 1, 1, 0)
    
    z <- z[,c(1, 8)]
    names(z) <- c('chave', 'indicatriz')
  }
  return(z)
}

func.score2009 <- function(x) {
  a1 <- 3000 + x
  a2 <- 4000 + x
  a3 <- 5000 + x
  a4 <- 6000 + x
  a5 <- 7000 + x
  
  b1 <- paste0('score.fem.2$s11b', a1)
  b2 <- paste0('score.fem.2$s11b', a2)
  b3 <- paste0('score.fem.2$s11b', a3)
  b4 <- paste0('score.fem.2$s11b', a4)
  b5 <- paste0('score.fem.2$s11b', a5)
  
  w <- matrix(NA, nrow = dim(score.fem.2)[1], ncol = 2)
  w[, 1] <- score.fem.2$chave
  if(x == 4 | x == 5 | x == 6) {
    w[, 2] <- ifelse(is.na(eval(parse(text = b1))) & is.na(eval(parse(text = b2)))
                     & is.na(eval(parse(text = b3))) & is.na(eval(parse(text = b4)))
                     & is.na(eval(parse(text = b5))), -1, NA)
  } else {
    w[, 2] <- ifelse(is.na(eval(parse(text = b1))) & is.na(eval(parse(text = b2)))
                     & is.na(eval(parse(text = b3))) & is.na(eval(parse(text = b4))), -1, NA)
  }
  w <- w[is.na(w[,2]),]
  w <- as.data.frame(w)
  names(w)[1] <- 'chave'
  w <- merge(w, score.fem.2, by = 'chave', all.x = T)
  w <- w[,-2]
  
  c1 <- paste0('w$s11b', a1)
  c2 <- paste0('w$s11b', a2)
  c3 <- paste0('w$s11b', a3)
  c4 <- paste0('w$s11b', a4)
  c5 <- paste0('w$s11b', a5)
  
  if(x == 4 | x == 5 | x == 6) {
    z <- matrix(NA, nrow = dim(w)[1], ncol = 7)
    z[, 1] <- w$chave
    
    z[, 2] <- ifelse(eval(parse(text = c1)) == 'Sim', 1, 0)
    z[, 3] <- ifelse(eval(parse(text = c2)) == 'Sim', 1, 0)
    z[, 4] <- ifelse(eval(parse(text = c3)) == 'Sim', 1, 0)
    z[, 5] <- ifelse(eval(parse(text = c4)) == 'Sim', 1, 0)
    z[, 6] <- ifelse(eval(parse(text = c5)) == 'Sim', 1, 0)
    
    z <- as.data.frame(z)
    
    z[is.na(z[,2]),2] <- 0
    z[is.na(z[,3]),3] <- 0
    z[is.na(z[,4]),4] <- 0
    z[is.na(z[,5]),5] <- 0
    z[is.na(z[,6]),6] <- 0
    
    z[,7] <- apply(z[,2:6], 1, sum)
    z[,8] <- NA
    z[,8] <- ifelse(z$V2 == 1 & z$V7 == 1, 1, 0)
    
    z <- z[,c(1, 8)]
    names(z) <- c('chave', 'indicatriz')
  } else {
    z <- matrix(NA, nrow = dim(w)[1], ncol = 6)
    z[, 1] <- w$chave
    
    z[, 2] <- ifelse(eval(parse(text = c1)) == 'Sim', 1, 0)
    z[, 3] <- ifelse(eval(parse(text = c2)) == 'Sim', 1, 0)
    z[, 4] <- ifelse(eval(parse(text = c3)) == 'Sim', 1, 0)
    z[, 5] <- ifelse(eval(parse(text = c4)) == 'Sim', 1, 0)
    
    z <- as.data.frame(z)
    
    z[is.na(z[,2]),2] <- 0
    z[is.na(z[,3]),3] <- 0
    z[is.na(z[,4]),4] <- 0
    z[is.na(z[,5]),5] <- 0
    
    z[,6] <- apply(z[,2:5], 1, sum)
    z[,7] <- NA
    z[,7] <- ifelse(z$V2 == 1 & z$V6 == 1, 1, 0)
    
    z <- z[,c(1, 7)]
    names(z) <- c('chave', 'indicatriz')
  }
  return(z)
}