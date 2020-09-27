library(sidrar)
library(dplyr)
library(lubridate)
library(tidyquant)

# IPCA Mensal Cheio --------------------------------------------------------------
# ipca var mensal (%) - 2020
ipca_2020 <- get_sidra(
    x = 7060, # código do ipca
    variable = 63, # variação mensal (%)
    category = list(7169), # índice geral
    classific = "c315", # código necessário para filtrar a categoria (descoberto no info_sidra)
    period = "all", # seleciona todo o período da tabela
    header = 1) # nome das colunas legível

ipca_2020 <- janitor::clean_names(ipca_2020)
ipca_2020$dt <- as.Date(paste0(as.character(ipca_2020$mes_codigo),01), "%Y%m%d")
ipca_2020$dt <- lubridate::ceiling_date(ipca_2020$dt,unit = "month")-1
ipca_2020 <- ipca_2020 %>% select(dt, valor) %>% as_tibble()

ipca_2012 <- get_sidra(
    x = 1419, # código do ipca
    variable = 63, # variação mensal (%)
    category = list(7169), # índice geral
    classific = "c315", # código necessário para filtrar a categoria (descoberto no info_sidra)
    period = "all", # seleciona todo o período da tabela
    header = 1) # nome das colunas legível

ipca_2012 <- janitor::clean_names(ipca_2012)
ipca_2012$dt <- as.Date(paste0(as.character(ipca_2012$mes_codigo),01), "%Y%m%d")
ipca_2012$dt <- lubridate::ceiling_date(ipca_2012$dt,unit = "month")-1
ipca_2012 <- ipca_2012 %>% select(dt, valor) %>% as_tibble()

ipca_mes <- ipca_2012 %>% rbind(ipca_2020)

# funcao que gera ipca acum 12 meses
acum_ipca <- function(df){
    ind = df$valor/100 + 1
    ipca = (prod(ind) - 1)*100
    return(ipca)
}

# df ipca mensal e acum.(12m)
ipca_mes <- ipca_mes %>% 
    tidyquant::tq_mutate(mutate_fun = rollapply,
                         width      = 12,
                         FUN        = acum_ipca,
                         by.column  = FALSE,
                         col_rename = "ipca_acum") %>% 
    rename(ipca_mensal = valor) %>% 
    na.omit()

#  IPCA mensal grupos --------------------------------------------------------------
# ipca var mensal (%) - 2020
ipca_20 <- get_sidra(
    x = 7060, # código do ipca
    variable = 63, # variação mensal (%)
    classific = "c315", # código necessário para filtrar a categoria (descoberto no info_sidra)
    period = "all", # seleciona todo o período da tabela
    header = 1, # nome das colunas legível
    category = list(c(
        7170, # Alimentacao
        7445, # Habitacao
        7486, # Artigos Residenciais
        7558, # Vestuario
        7625, # Transportes
        7660, # Saude
        7712, # Desp. Pessoais
        7766, # Educacao
        7786) # Comunicacao
    )
)

ipca_20 <- janitor::clean_names(ipca_20)
ipca_20$dt <- as.Date(paste0(as.character(ipca_20$mes_codigo),01), "%Y%m%d")
ipca_20$dt <- lubridate::ceiling_date(ipca_20$dt,unit = "month")-1
ipca_20 <- ipca_20 %>% 
    select(5,10,13,14) %>% 
    rename(anomes = mes_codigo,
           grupo = geral_grupo_subgrupo_item_e_subitem) %>% as_tibble()

ipca_12 <- get_sidra(
    x = 1419, # código do ipca
    variable = 63, # variação mensal (%)
    classific = "c315", # código necessário para filtrar a categoria (descoberto no info_sidra)
    period = "all", # seleciona todo o período da tabela
    header = 1, # nome das colunas legível
    category = list(c(
        7170, # Alimentacao
        7445, # Habitacao
        7486, # Artigos Residenciais
        7558, # Vestuario
        7625, # Transportes
        7660, # Saude
        7712, # Desp. Pessoais
        7766, # Educacao
        7786) # Comunicacao
    )
)

ipca_12 <- janitor::clean_names(ipca_12)
ipca_12$dt <- as.Date(paste0(as.character(ipca_12$mes_codigo),01), "%Y%m%d")
ipca_12$dt <- lubridate::ceiling_date(ipca_12$dt,unit = "month")-1
ipca_12 <- ipca_12 %>% 
    select(5,10,13,14) %>% 
    rename(anomes = mes_codigo,
           grupo = geral_grupo_subgrupo_item_e_subitem) %>% as_tibble()

# empilha ipca 19 + ipca 20
ipca_mes_grupo <- ipca_12 %>% 
    rbind(ipca_20)
ipca_mes_grupo$grupo <- sub(pattern = "\\d.","",ipca_mes_grupo$grupo) 


ipca_mes_grupo <- ipca_mes_grupo %>%
    group_by(grupo) %>%
    # select(-anomes) %>% 
    tidyquant::tq_mutate(mutate_fun = rollapply,
                         width      = 12,
                         FUN        = acum_ipca,
                         by.column  = FALSE,
                         col_rename = "ipca_acum") %>% 
    rename(ipca_mensal = valor) %>% 
    na.omit() %>% 
    ungroup()
    # mutate(anomes = as.character(year(dt)*100 + month(dt)))

# ipca_grupo_mes ----------------------------------------------------------
# captura o último dado do IPCA GERAL
last_ipca <- ipca_mes %>%
    filter(dt == max(dt)) %>%
    select(-dt)
