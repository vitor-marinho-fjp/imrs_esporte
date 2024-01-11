#' ---
#' title: "Importação dos dados IMRS dimensão Esporte"

#' ---
#' 
options(warn=-1)

#' # Estrutura do script
#' 
#' ## Limpa a memória e console
cat("\014")  
rm(list = ls())

#' ## Configura o diretório de trabalho
#' Altera a pasta de trabalho para a mesma onde o script está salvo
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd('E:\\Meu Drive\\IMRS\\ESPORTE\\Esporte\\2022')


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


#' Ano para o qual deseja-se obter os dados (ou seja, colaca-se o ano anterior ao atual)
ano_dados = 2021

#' ## Importação dos dados
#' 
#' Aqui é realizada a leitura dos arquivos em formato .xlsx. Os arquivos deverão estar na mesma pasta em que o 
#' script está salvo.
dados_imrs <- as_tibble(readxl::read_excel("IMRS2021 - BASE ESPORTE.xlsx", sheet = 1, col_types = c("numeric", "numeric", "numeric", "numeric",
                                                                                                    "text", "numeric", "text", "numeric", "numeric",
                                                                                                    "numeric", "text", "text", "text" )))
dados_icms <- as_tibble(readxl::read_excel("Dados_Basicos_Esportes_2022.xlsx", sheet = 1))
dados_quadra <- as_tibble(readxl::read_excel("ProcQuadras_2021.xlsx", sheet = 1))
dados_munic <- as_tibble(readxl::read_excel("Base_MUNIC_2021.xlsx", sheet = 7))





#' ## Tratamento dos dados
#' 
#' ### Dados do ICMS

# Dados ICMS --------------------------------------------------------------


dados_icms <- dados_icms %>% janitor::clean_names()

#' Faz a conversão dos dados de caracter para númerico                       
dados_icms[, c(5:8)] <- sapply(dados_icms[, c(5:8)], as.numeric)

#' Reconstrói o código IBGE
dados_icms <- dados_icms %>% mutate(ibge = as.character(ibge)) %>%
                             mutate(IBGE6 = apply(dados_icms, 1, function (x) as.numeric(paste0("31", gsub(" ", "0", x["ibge"])))))

#' Cria a coluna de chave, usando o ano anterior (ou seja, o último ano em que foram calculados os indicadores do IMRS).
#' Posteriormente, essa chave será atualizada 
dados_icms <- dados_icms %>% mutate(CHAVE = as.numeric(paste0(ano_dados-1, IBGE6)))



#' Cria a tabela de indicadores, que é o resultado da união dos dados do IMRS com os dados
#' do ICMS utilizando a chave do ano anterior
indicadores <- merge(dados_imrs, dados_icms, by = c("CHAVE", "IBGE6")) 

#' Atualiza a chave e o ano na tabela indicadores
indicadores <- indicadores %>% mutate(CHAVE = paste0(ano_dados, substring(CHAVE, 5))) %>% 
                               mutate(ANO = ano_dados)

#' Remove os valores relativos ao ano anterior
indicadores[, 5:13] <- NA 
# seleciona todas as linhas e colunas de 5 a 13 e atribui o valor NA a elas


#' Cálculo do indicador: dado as cidades que tem conselho de esportes (primeira linha), dividir a pontuação pelos pesos
indicadores <- indicadores %>% mutate(L_PROGE =  if_else_(indicadores$conselho_de_esportes=="SIM",
                                                         if_else_(is.na(indicadores$pontuacao_municipio_soma_das_atividades_do_municipio), 0, 
                                                                  if_else_(is.na(indicadores$pontuacao_municipio_soma_das_atividades_do_municipio/indicadores$peso_da_rcl), 0, 
                                                          indicadores$pontuacao_municipio_soma_das_atividades_do_municipio/indicadores$peso_da_rcl)), 0))

indicadores <- indicadores %>% mutate(L_CONSESP = indicadores$conselho_de_esportes)

#' Corrige o valor das variáveis binárias
indicadores$L_CONSESP = case_when(indicadores$L_CONSESP == "SIM" ~ "Sim",
                                  indicadores$L_CONSESP == "NÃO" ~ "NÃO",
                                  TRUE ~ as.character(NA))

#' Default do R é apresentar números em notação cinentífica, aqui eliminamos essa possibilidade.
options(scipen = 9999999)
indicadores <- indicadores %>% mutate(L_ILRHE =  if_else_(indicadores$conselho_de_esportes=="SIM",
                                                          if_else_(is.na(indicadores$pontuacao_municipio_soma_das_atividades_do_municipio), 0, 
                                                          (indicadores$pontuacao_municipio_soma_das_atividades_do_municipio/indicadores$soma_pontuacao_municipios_mg)*100), 0))
#' Remove as colunas que não são mais necessárias
indicadores <- indicadores %>% select(-c(14:ncol(indicadores)))


# Utilizar Munic unica ----------------------------------------------------

#' Filtra os dados para os municípios de Minas Gerais
dados_munic <- dados_munic[dados_munic$UF=="MG",]

#' Altera o nome da coluna CodMun para IBGE7
colnames(dados_munic)[1] <- "IBGE7"

#' Faz a união das duas tabelas de acordo com o código do ibge
indicadores <- merge(indicadores, dados_munic, by = "IBGE7")

#' Tipo de órgão gestor do esporte
indicadores$L_ORGESP <- indicadores$Mesp01

#' Verifica se todos os valores do indicador l_orgesp estão corretos
if (sum(c( "Não possui estrutura", "Secretaria municipal exclusiva") %in% tabyl(indicadores$L_ORGESP)[[1]]) < 3 ){
  stop("Erro na variavel MESP01 da MUNIC. Algum municipio não se enquadra em um dos valores esperado")
}


#' Altera o nome da coluna S1 para IBGE7
colnames(dados_munic_conv) <- c("IBGE7", "Mesp18")

#' Faz a uni?o das duas tabelas de acordo com o c?digo do ibge
indicadores <- merge(indicadores, dados_munic_conv, by = "IBGE7")

#' Obt?m o indicador
#' *Observa??o*: para realizar a leitura a partir da MUNIC, basta descomentar as pr?ximas duas linhas.
indicadores <- indicadores %>% mutate(L_CONVESP = indicadores$Mesp18) %>%
                               select(-Mesp18)

#' Caso deseje preencher o indicador com N?o, basta descomentar as duas linhas seguintes.
indicadores <- indicadores %>% mutate(L_CONVESP = "N?o") %>%
  select(-Mesp18)

#' Por fim, se desejar deixar os valores em branco, basta descomentar a linha a seguir
indicadores <- indicadores %>% select(-Mesp18)




# L_CONVESP ---------------------------------------------------------------

L_con <-  dados_munic %>% mutate(L_CONVESP = Mesp18) %>% 
  select(IBGE7, L_CONVESP)

L_con$IBGE7 <- as.double(L_con$IBGE7)

indicadores <-  indicadores %>% select(-L_CONVESP)

indicadores <-  left_join(indicadores, L_con, by = 'IBGE7')


#' Filtra os dados para os municípios de Minas Gerais
#dados_munic_conv <- dados_munic_conv %>% subset(substr(dados_munic_conv$CodMun, 1, 2) == "31") %>% 
#seleciona as linhas cujos códigos 
                                                                                               
# de município começam com 31
#select(c(1, 5)) 
# seleciona as colunas com as variáveis de interesse (CodMun, Mesp18)

#' Altera o nome da coluna CodMun para IBGE7
#colnames(dados_munic_conv) <- c("IBGE7", "Mesp18")

#' Faz a união das duas tabelas de acordo com o código do ibge
#indicadores <- merge(indicadores, dados_munic_conv, by = "IBGE7")

#' Obtém o indicador
#' *Observação*: para realizar a leitura a partir da MUNIC, basta descomentar as próximas duas linhas.
#indicadores$L_CONVESP <- indicadores$Mesp18.y
#indicadores <- indicadores[, !names(indicadores) %in% c("Mesp18.x", "Mesp18.y")]

#' Caso deseje preencher o indicador com Não, basta descomentar as duas linhas seguintes.
#indicadores <- indicadores %>% mutate(L_CONVESP = "N?o") %>%
#                               select(-Mesp18)

#' Por fim, se desejar deixar os valores em branco, basta descomentar a linha a seguir
#indicadores <- indicadores %>% select(-Mesp18)



#' Filtra os dados para os municípios de Minas Gerais
dados_munic_equip <- dados_munic_equip %>% subset(substr(dados_munic_equip$CodMun, 1, 2) == "31") %>% 
#seleciona as linhas cujos códigos 
                                                                                                  
# de município começam com 31
select(c("CodMun", "Mesp22", "Mesp221")) 
# seleciona as colunas com as variáveis de interesse (CodMun, Mesp22, Mesp221)

#' Altera o nome da coluna CodMun para IBGE7
colnames(dados_munic_equip) <- c("IBGE7", "Mesp22", "Mesp221")

#' Faz a união das duas tabelas de acordo com o código do ibge
indicadores <- merge(indicadores, dados_munic_equip, by = "IBGE7")


#' Obtém o indicador
indicadores <- indicadores %>% mutate(L_EQUI = indicadores$Mesp22) %>% select(-Mesp22) %>%
                               mutate(L_QUANT = as.numeric(indicadores$Mesp221)) %>% select(-Mesp221)


#' Filtra os dados para os municípios de Minas Gerais
dados_munic_acoes <- dados_munic_acoes[dados_munic_acoes$UF=="MG", ]

# seleciona as colunas com as variáveis de interesse (CodMun, Mesp161, Mesp162, Mesp163 e Mesp164)

#' O indicador tem valor Sim se pelo menos uma das variáveis tem valor Sim, caso contrário é Não
indicadores <- indicadores %>% mutate(L_PARTESP =  if_else_(dados_munic_acoes$Mesp161 == "Sim" |
                                                            dados_munic_acoes$Mesp162 == "Sim" |
                                                            dados_munic_acoes$Mesp163 == "Sim" |
                                                            dados_munic_acoes$Mesp164 == "Sim", true = "Sim", false = "N?o", missing = NA))
#' ### Dados INEP

#' Renomeia as colunas
colnames(dados_quadra) <- c("IBGE7", "pquadras")

#' Faz a união dos indicadores com os dados sobre percentual de alunos em escolas com quadra
indicadores <- merge(indicadores, dados_quadra, by= "IBGE7")

#' Atualiza o indicador
indicadores <- indicadores %>% mutate(L_ESPESC = indicadores$pquadras) %>%
                               select(-pquadras)

indicadores <- indicadores[, c("CHAVE", "IBGE6", "IBGE7", "ANO", "L_EQUI", "L_QUANT", "L_PARTESP", "L_PROGE", "L_ESPESC", "L_ILRHE", "L_CONSESP", "L_ORGESP", "L_CONVESP")]

#' Juntando a base original com os indicadores
dados_imrs <- rbind(dados_imrs, indicadores) 

#' Ajusta o número de casas decimais
dados_imrs <- dados_imrs %>% mutate(L_PROGE = round(L_PROGE, digits = 2)) %>%
                             mutate(L_ESPESC = round(L_ESPESC, digits = 2)) %>%
                             mutate(L_ILRHE = round(L_ILRHE, digits = 3))

#' Exportando a base. O arquivo gerado terá o mesmo nome do arquivo com os dados do IMRS, porém com
#' o ano atualizado.
#' 

writexl::write_xlsx(dados_imrs,paste0("IMRS", ano_dados+1, " - BASE ESPORTE.xlsx"))
