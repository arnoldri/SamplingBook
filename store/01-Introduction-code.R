
# automatically create a bib database for R packages
knitr::write_bib(c(
  .packages(), 'survey', 'srvyr', 'mice'
), 'packages.bib')


