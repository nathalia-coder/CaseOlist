hipo_categorica <- function(dados, grupo, variavel){
  
  dados <- dados %>% drop_na(grupo) %>% mutate(grupo = ifelse(grupo == "0", "Novo", "Recorrente")) 
  data_grupo <- dados %>% dplyr::select( !!sym(grupo) ) %>% pull( !!sym(grupo) )
  data_variavel <- dados %>% dplyr::select( !!sym(variavel) ) %>% pull( !!sym(variavel) )
  tab = xtabs(~ data_variavel + data_grupo) # tabela de frequencia
  teste <- stats::chisq.test(tab, simulate.p.value = TRUE,  B = 10000)
  
  tabela <- list(
    `Variável` = variavel,
    Categorias = rownames(tab),
    Novo = tab[,"Novo"],
    `Novo(%)` = paste0( (tab[,"data_grupo"]/sum(tab[,"data_grupo"]) * 100) %>% round(2) ,"%" ),
    MP = tab[,"MP"],
    `Recorrente(%)` = paste0( (tab[,"Recorrente"]/sum(tab[,"Recorrente"]) * 100) %>% round(2) ,"%" ),
    `P(Teste)` = teste$p.value
  ) %>% as_tibble()
  
  return(tabela)
}