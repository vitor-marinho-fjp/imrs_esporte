

# IMRS Esporte ------------------------------------------------------------

#' Ano para o qual deseja-se obter os dados (ou seja, colaca-se o ano anterior ao atual)

ano_dados = 2021


#' ## Carrega as bibliotecas
pacotes <- c("readxl", "tidyverse", "fuzzyjoin",
             "janitor", "writexl", "stringdist",
             "hablar", "rlang")

#' Verifica se alguma das bibliotecas necessárias ainda não foi instalada
pacotes_instalados <- pacotes %in% rownames(installed.packages())
if (any(pacotes_instalados == FALSE)) {
  install.packages(pacotes[!pacotes_instalados])
}

#' carrega as bibliotecas
#+ results = "hide"
lapply(pacotes, library, character.only=TRUE)



# Bases -------------------------------------------------------------------

#' Aqui é realizada a leitura dos arquivos em formato .xlsx. Os arquivos deverão estar na mesma pasta em que o 
#' script está salvo.
dados_imrs <- as_tibble(readxl::read_excel("IMRS2021 - BASE ESPORTE.xlsx", sheet = 1, col_types = c("numeric", "numeric", "numeric", "numeric",
                                                                                                    "text", "numeric", "text", "numeric", "numeric",
                                                                                                    "numeric", "text", "text", "text" )))
dados_icms <- as_tibble(readxl::read_excel("Dados_Basicos_Esportes_2022.xlsx", sheet = 1)) %>% janitor::clean_names()
dados_quadra <- as_tibble(readxl::read_excel("ProcQuadras_2021.xlsx", sheet = 1))
dados_munic <- as_tibble(readxl::read_excel("Base_MUNIC_2021.xlsx", sheet = 7)) %>% 
  rename(IBGE7=1) %>% filter(UF=='MG')



# Dados MUNIC -------------------------------------------------------------

dados_munic <- as_tibble(readxl::read_excel("Base_MUNIC_2021.xlsx", sheet = 7)) %>% 
  rename(IBGE7=1) %>% filter(UF=='MG')


indicadores_munic <-  dados_munic %>% select(IBGE7,
                       L_EQUI = Mesp22,                 ##L_EQUI -instalação esportiva MESP22
                       L_QUANT = Mesp221,               #L_QUANT - Quantidade de instalações esportivas municipais - MESP221
                       L_PARTESP = Mesp18,              #L_PARTESP-  Execução de ações, projetos ou programas esportivos pela prefeitura - MESP18
                       L_CONSESP =Mesp10,               #L_CONSESP - Conselho Municipal de Esportes em Atividade - MESP10
                       L_ORGESP = Mesp01,               #L_ORGESP -  Tipo de orgão gestor do esporte - MESP01
                       L_CONVESP = Mesp18               #L_CONVESP - Realização de convênio no âmbito de esporte pela prefeitura - Mesp18
                       )     



# ICMS --------------------------------------------------------------------

#' Reconstrói o código IBGE
indicadores <- dados_icms %>% mutate(ibge = as.character(ibge)) %>%
  mutate(IBGE6 = apply(dados_icms, 1, function (x) as.numeric(paste0("31", gsub(" ", "0", x["ibge"])))))

#' Cria a coluna de chave, usando o ano anterior (ou seja, o último ano em que foram calculados os indicadores do IMRS).
#' Posteriormente, essa chave será atualizada 
indicadores <- dados_icms %>% mutate(CHAVE = as.numeric(paste0(ano-1, IBGE6)))


#' Assinalando não para os valores ausentes, lembrando que na base do Max, municipios que 
#' não tem conselho de esporte aparecem como missing
indicadores <- indicadores %>%
  mutate(conselho_de_esportes = if_else(is.na(conselho_de_esportes), "NÃO", conselho_de_esportes))


#L_PROGE - Pontuação pela participação em programas governamentais de esporte

#' Cálculo do indicador: dado as cidades que tem conselho de esportes (primeira linha), dividir a pontuação pelos pesos
indicadores <- indicadores %>% mutate(L_PROGE =  if_else_(indicadores$conselho_de_esportes=="SIM",
                                                          if_else_(is.na(indicadores$pontuacao_municipio_soma_das_atividades_do_municipio), 0, 
                                                                   if_else_(is.na(indicadores$pontuacao_municipio_soma_das_atividades_do_municipio/indicadores$peso_da_rcl), 0, 
                                                                            indicadores$pontuacao_municipio_soma_das_atividades_do_municipio/indicadores$peso_da_rcl)), 0))


# L_ILRHE - Participação percentual em atividades esportivas habilitadas no critério esporte do ICMS Solidário

indicadores <- indicadores %>% 
  mutate(L_ILRHE =  if_else_(indicadores$conselho_de_esportes=="SIM",
                              if_else_(is.na(indicadores$pontuacao_municipio_soma_das_atividades_do_municipio), 0, 
                             (indicadores$pontuacao_municipio_soma_das_atividades_do_municipio/indicadores$soma_pontuacao_municipios_mg)*100), 0))


#selecionar colunas relevante
indicadores_icms <- indicadores %>% select(9:12)

#criar coluna IBGE6
indicadores_munic$IBGE6 <- as.integer(substring(indicadores_munic$IBGE7, 1, nchar(indicadores_munic$IBGE7) - 1))



indicadores <- merge(indicadores_munic, indicadores_icms, by = "IBGE6" ) 


# IDEB --------------------------------------------------------------------

# L_ESPESC - Percentual de alunos em escolas com quadra de esporte


#' Renomeia as colunas
colnames(dados_quadra) <- c("IBGE7", "pquadras")

#' Faz a união dos indicadores com os dados sobre percentual de alunos em escolas com quadra
ind_qdr <- merge(indicadores, dados_quadra, by= "IBGE7")

#' Atualiza o indicador
indicadores <- ind_qdr %>% mutate(L_ESPESC = ind_qdr$pquadras) %>%
  select(-pquadras)


# Ajustar e exporta a base ------------------------------------------------
indicadores <-  indicadores %>% mutate(ANO=2021)


indicadores <- indicadores[, c("CHAVE", "IBGE6", "IBGE7", "ANO", "L_EQUI", "L_QUANT", "L_PARTESP", 
                               "L_PROGE", "L_ESPESC", "L_ILRHE", "L_CONSESP", "L_ORGESP", "L_CONVESP")]


#ajustar texto do tipo de secretaria
indicadores$L_ORGESP <- ifelse(indicadores$L_ORGESP == "Secretaria exclusiva", "Secretaria municipal exclusiva", "Outros")



#juntar com a base histórica do IMRS
dados_imrs <-  rbind(dados_imrs, indicadores)


#replicar a base para o ano de 2022

ind_2022 <-  indicadores %>% mutate(ANO=2022)


#juntar a base histórica do IMRS

dados_imrs <-  rbind(dados_imrs, ind_2022)

#arredondar casas decimais
dados_imrs <- dados_imrs %>%
  mutate_at(vars(8:10,), ~ifelse(is.na(as.numeric(.)), NA, as.numeric(.))) %>%
  mutate_at(vars(8:10), ~round(., 2))

#exportar

writexl::write_xlsx(dados_imrs, "BASE_ESPORTE.xlsx")
















