library(dplyr)
library(stringr)


# 1. Leitura das bases ----------------------------------------------------

# Bases do CNPQ

# Com exceção das bases dos anos de 2020 e 2019, a url para acessar os dados
# muda apenas em relação ao ano de referência.
# Vamos começar por elas, criando uma função que faz a leitura da base, transforma,
# e salva em uma pasta dedicada no formato .rds

leitura_limpeza <- function(u_base, ano) {
  
  readr::read_csv(paste0(u_base, ano, ".csv"), 
                  locale = readr::locale(encoding = 'latin1')) %>% 
    janitor::clean_names() %>% 
    select(-c(processo, beneficiario, nome_chamada, programa_cn_pq,
              sigla_instituicao_destino)) %>% 
    mutate(categoria = case_when(
      str_detect(linha_de_fomento, "Iniciação Científica") ~ "Iniciação Científica",
      str_detect(linha_de_fomento, "Mestrado") ~ "Mestrado",
      str_detect(linha_de_fomento, "Doutorado") ~ "Doutorado",
      str_detect(linha_de_fomento, "Pós-doutorado") ~ "Pós-doutorado",
      TRUE ~ "Outros"
    ), .before = "linha_de_fomento") %>% 
    with_groups(c(ano_referencia:pais_destino), 
                summarise, bolsas_concedidas = n(), 
                valor_pago = sum(valor_pago)) %>% 
    readr::write_rds(paste0("data/", "cnpq_", ano, ".rds"))
  
}

### Download das bases de 2010 a 2018 ----

url <- "http://ftp.cnpq.br/pub/CKAN/investimentos_cnpq_"

purrr::map2(url,c(2010:2017), ~leitura_limpeza(.x, .y))


### Download das bases de 2019 e 2020 ----

# A url das bases de 2019 e 2020 possuem um padrão diferente. Além disto, 
# a de 2020 está comprimida em um arquivo .zip
# Por conta dessa diferença nas urls, precisaremos fazer adaptações na função
# para pegar as bases

leitura_limpeza_nova <- function(u_base, ano) {
  
  readr::read_csv(u_base, 
                  locale = readr::locale(encoding = 'latin1')) %>% 
    janitor::clean_names() %>% 
    select(-c(processo, beneficiario, nome_chamada, programa_cn_pq,
              sigla_instituicao_destino)) %>% 
    mutate(categoria = case_when(
      str_detect(linha_de_fomento, "Iniciação Científica") ~ "Iniciação Científica",
      str_detect(linha_de_fomento, "Mestrado") ~ "Mestrado",
      str_detect(linha_de_fomento, "Doutorado") ~ "Doutorado",
      str_detect(linha_de_fomento, "Pós-doutorado") ~ "Pós-doutorado",
      TRUE ~ "Outros"
    ), .before = "linha_de_fomento") %>% 
    with_groups(c(ano_referencia:pais_destino), 
                summarise, bolsas_concedidas = n(), 
                valor_pago = sum(valor_pago)) %>% 
    readr::write_rds(paste0("data/", "cnpq_", ano, ".rds"))
  
}

# Base de 2019

u_2019 <- "http://ftp.cnpq.br/pub/CKAN/investimentos-2019r/investimentos-2019r.csv"
leitura_limpeza_nova(u_2019, 2019)

# Base de 2020

url <- "http://dadosabertos.cnpq.br/pt_BR/dataset/bed8fae4-ce1d-433c-bb66-96051033a029/resource/c62837e9-4445-48a6-a782-e1b04eacab0d/download/resultado_dados_abertos_2020.zip"

temp <- tempfile()
download.file(url,temp)
data <- readr::read_csv2(temp, locale = readr::locale(encoding = 'latin1'))
unlink(temp)

data %>% 
  janitor::clean_names() %>% 
  select(-c(x1, processo, beneficiario, nome_chamada, programa_cn_pq,
            sigla_instituicao_destino)) %>% 
  mutate(categoria = case_when(
    str_detect(linha_de_fomento, "Iniciação Científica") ~ "Iniciação Científica",
    str_detect(linha_de_fomento, "Mestrado") ~ "Mestrado",
    str_detect(linha_de_fomento, "Doutorado") ~ "Doutorado",
    str_detect(linha_de_fomento, "Pós-doutorado") ~ "Pós-doutorado",
    TRUE ~ "Outros"
  ), .before = "linha_de_fomento") %>% 
  with_groups(c(ano_referencia:pais_destino), 
              summarise, bolsas_concedidas = n(), 
              valor_pago = sum(valor_pago)) %>% 
  readr::write_rds(paste0("data/", "cnpq_2020.rds"))


# 2. Unindo as bases ------------------------------------------------------


bases <- list.files("data/")

cnpq_completo <- purrr::map_dfr(bases, ~readr::read_rds(paste0("data/", .x)))

readr::write_rds(cnpq_completo, "data/cnpq_completo.rds")
