# Criando a função que redenriza os .Rmd
custom_render <- function(category, slug, date) {
  custom_dir <- paste0('content/posts/', category)
  dir.create(custom_dir)
  rmarkdown::render(paste0('Rmd_directory/', category, '/', slug, '.Rmd'), output_dir = custom_dir, output_file = paste0(date, '---', slug))
}



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
