interpreta_ANOVA <- function(mod, alpha = 0.05){
  library(flextable)
  # Realiza a análise de variância e converte para tibble
  tab <- mod %>% anova()
  tabela <- tab %>% as_tibble() %>% round(3)
  
  # Prepara a tabela
  tabela <- tabela %>% mutate(
    `Covariável` = row.names(tab) %>% 
      str_replace_all("(?<=[a-z])(?=[A-Z0-9])", " ") %>% 
      str_to_title() %>% 
      str_replace_all(":", " : "),
    sig = case_when(
      is.na(`Pr(>F)`) ~ " ",
      `Pr(>F)` < 0.001 ~ "<0.001*",
      `Pr(>F)` <= alpha ~ paste0(" ", format(`Pr(>F)`, digits = 3), "*"),
      `Pr(>F)` <= (alpha * 2) ~ paste0(" ", format(`Pr(>F)`, digits = 3), "."),
      TRUE ~ paste0(" ", format(`Pr(>F)`, digits = 3))
    )
  )
  
  tabela <- tabela %>%
    dplyr::select(`Covariável`, Df, `Sum Sq`, `Mean Sq`, `F value`, sig) %>%
    rename(
      `gl` = Df, SQ = `Sum Sq`, MQ = `Mean Sq`,
      "F" = `F value`, "Pvalor" = sig
    ) %>%
    mutate_all(~ ifelse(is.na(.), "", .)) # Tirando NA da saída
  
  # Criar um objeto flextable
  ft <- flextable(tabela)
  
  # Adicionar estilo
  ft <- set_table_properties(ft, layout = "autofit")
  
  # Adicionar rodapé
  ft <- add_footer_lines(ft, values = paste0("Significativo a ", (alpha * 100),
                                             "% de significância e '.' significativo a ", (2 * alpha * 100), "%"))
  
  # Destacar cabeçalhos e a primeira coluna
  ft <- bold(ft, bold = TRUE, part = "header")
  ft <- bold(ft, bold = TRUE, j = 1)
  
  return(ft)
}

Arruma_CompMult <- function(comparacao){
  # Funcao para interacaoes de Tukey de ordem ate ^2
  
  AjeitaLista <- function(lista, alpha = 0.05){
    tabela <- lista %>% as_tibble() %>% round(3) %>% 
      mutate(Comparacoes = rownames(lista)) %>% # nomes das comparacoes
      # pegando apenas significativas
      filter(`p adj` <= alpha*2)
    if(tabela %>% nrow() != 0){
      tabela <- tabela %>% 
        # reagrupa num novo formato
        separate(Comparacoes, into = c("G1", "G2"), sep = "-") %>% 
        separate(G1, into = c("G1.1", "G1.2"), sep = ":") %>% 
        separate(G2, into = c("G2.1", "G2.2"), sep = ":") %>% 
        unite(col = Fator1, G1.1,G2.1, sep = " - ", remove = T) %>% 
        unite(col = Fator2, G1.2,G2.2, sep = " : ", remove = T) %>% 
        # mudando forma de mostrar pvalor
        mutate(Pvalor = case_when(
          `p adj` < 0.001 ~ "<0.001*",
          `p adj` <= alpha ~ paste0(" ", format(`p adj`, digits = 3), "*"),
          `p adj` <= (alpha*2) ~ paste0(" ", format(`p adj`, digits = 3), "."),
          TRUE ~ paste0(" ", format(`p adj`, digits = 3))) # Caso padrão
        )%>%
        dplyr::select(Fator1, Fator2, everything(), -lwr, -upr, -`p adj`) %>% 
        rename(`Diferença`= diff)
      
      if(grepl("NA", tabela$Fator2) %>% unique() ){
        return(tabela %>% dplyr::select(-Fator2)) #caso nao tenha fator 2
      }else{return(tabela)}
    }
  }
  
  return( lapply(comparacao, AjeitaLista) )
}

CompMult_visual <- function(CompMultArrumado, alpha=0.05){
  library(kableExtra)
  nome_objeto <- deparse(substitute(CompMultArrumado))
  nome_obj_split <- strsplit(gsub("[`]", "", nome_objeto), ":")[[1]]
  fator2_nome <- nome_obj_split[2] #quando for 1 fator, isso sera NA
  posicao_cifrao <- str_locate(nome_obj_split, "\\$")[1]
  fator1_nome <- str_sub(nome_obj_split[1], posicao_cifrao + 1)
  
  if(!CompMultArrumado %>% is.null()){
    tabela <- CompMultArrumado %>% 
      arrange(Fator1) %>% 
      kbl(align = "c", booktabs = T) %>% 
      # estilo
      row_spec(0, bold = T) %>%
      kable_classic_2(full_width = F, html_font = "Arial") %>% 
      kable_styling(bootstrap_options = "responsive") %>%
      kable_paper(full_width = F) %>% 
      # agrupar linhas
      collapse_rows(columns = 1, valign = "middle") # isso tira o formato tabela bonito igual da anova
    
    if(is.na(fator2_nome)){ # implica que so tem um fator no ajuste
      tabela %>% 
        footnote( # rodape
          symbol = paste0("significativo a ", (alpha*100),
                          "% de significância e '.' significativo a ", (2*alpha*100),"%"),
          general = paste0("Fator1 refere a ", fator1_nome),
          general_title = "Nota: ",footnote_as_chunk = T,
        )
    }else{ # implica ter mais de um fator no ajuste
      tabela %>% 
        footnote(# rodape
          symbol = paste0("significativo a ", (alpha*100),
                          "% de significância e '.' significativo a ", (2*alpha*100),"%"),
          general = paste0("Fator1 refere a ", fator1_nome, " e Fator2 a ", fator2_nome),
          general_title = "Nota: ",footnote_as_chunk = T,
        )
    }
  }
}
