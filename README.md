# IMRS Esporte

Este repositório contém um script R para a análise de dados relacionados ao esporte, utilizando dados do ano de 2021. O script realiza a leitura, processamento e combinação de várias bases de dados relacionadas ao esporte em Minas Gerais.

## Dependências

O script depende das seguintes bibliotecas R:
- readxl
- tidyverse
- fuzzyjoin
- janitor
- writexl
- stringdist
- hablar
- rlang

As bibliotecas serão instaladas automaticamente caso ainda não estejam presentes no seu ambiente R.

## Estrutura do Código

O código está estruturado da seguinte maneira:
1. **Carregamento das Bibliotecas**: Verifica e carrega as bibliotecas necessárias.
2. **Leitura das Bases de Dados**: Lê os arquivos `.xlsx` contendo dados sobre esportes, ICMS, quadras esportivas e dados municipais.
3. **Processamento dos Dados MUNIC e ICMS**: Processa e prepara os dados, incluindo a criação de novos indicadores.
4. **Cálculo de Indicadores Específicos**: Calcula indicadores específicos como participação em programas governamentais de esporte e percentual de alunos em escolas com quadra de esporte.
5. **Ajustes Finais e Exportação**: Realiza ajustes finais nos dados e os exporta em formato `.xlsx`.

## Execução

Para executar o script, coloque os arquivos de dados na mesma pasta do script e execute-o em um ambiente R. Certifique-se de ter as permissões necessárias para ler e escrever arquivos na pasta.

## Contribuições

Contribuições são bem-vindas. Para contribuir, por favor, abra um 'issue' ou faça um 'pull request'.

## Licença

Este projeto está sob a Licença MIT. Veja o arquivo `LICENSE` para mais detalhes.

---
