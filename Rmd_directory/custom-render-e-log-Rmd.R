# Criando a função que redenriza os .Rmd
custom_render <- function(category, slug, date) {
  custom_dir <- paste0('content/posts/', category)
  dir.create(custom_dir)
  rmarkdown::render(paste0('Rmd_directory/', category, '/', slug, '.Rmd'), output_dir = custom_dir, output_file = paste0(date, '---', slug))
}

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #

## Modelo para .Rmd
'
---
title: "Perfecting the Art of Perfection"
date: "2016-09-01"
template: "post"
draft: false
slug: "perfecting-the-art-of-perfection"
category: "Design Inspiration"
tags:
  - "Handwriting"
  - "Learning to write"
description: "Quisque cursus, metus vitae pharetra auctor, sem massa mattis sem, at interdum magna augue eget diam."

output: 
  md_document:
    preserve_yaml: TRUE
---

```{r setup, include=FALSE}
opts_chunk$set(cache=TRUE,
               prompt=FALSE,
               tidy=TRUE,
               comment=NA,
               message=FALSE,
               warning=FALSE,
               echo=FALSE)
```
'
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #

## Artigo 1

---
title: "Tabelas de Contingência - Análise transversal"
date: "2020-03-23"
template: "post"
draft: false
slug: "tab-freq-analise-transversal"
category: "Análise AIBF"
tags:
  - "Tabelas de contingência"
  - "Dados categóricos"
description: "Partindo do princípio que nossos dados podem ser estudados tanto sob uma perspectiva longitudinal quanto transversal, nesta seção iremos abordar as tabelas de contingência entre a indicatriz de decisão e grupo sob qual o domicílio pertença."

output: 
  md_document:
    preserve_yaml: TRUE
---

custom_render(category = 'Análise AIBF', slug = 'tab-freq-analise-transversal', date = '2020-03-23')
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- #
  