# reprodutibilidade

Ferramentas em R para apoiar a preparação, a análise estatística e a documentação de indicadores no contexto do **AdaptaBrasil**.

[![Versão](https://img.shields.io/badge/vers%C3%A3o-0.0.0.9000-blue)](DESCRIPTION)
[![Status](https://img.shields.io/badge/status-em_desenvolvimento-orange)](DESCRIPTION)
[![Licença MIT](https://img.shields.io/badge/licen%C3%A7a-MIT-green)](LICENSE.md)

## Objetivo

O pacote reúne rotinas para examinar dados municipais, identificar valores ausentes e extremos, aplicar transformações e normalização, avaliar associações entre indicadores e produzir tabelas, gráficos, mapas e apresentações.

Seu propósito é facilitar a repetição e a documentação das etapas de preparação de indicadores simples. A interpretação dos diagnósticos e a escolha dos tratamentos continuam dependendo da metodologia de cada indicador.

**Nome do repositório:** `Reprodutibilidade`. **Nome do pacote no R:** `reprodutibilidade`.

## Instalação

A versão de desenvolvimento está disponível neste repositório:

```r
install.packages("remotes")
remotes::install_github(
  "netebarreto/Reprodutibilidade",
  build_vignettes = FALSE
)

library(reprodutibilidade)
```

Utilize **R 4.1 ou superior**, pois o código emprega o operador nativo `|>`. O arquivo [DESCRIPTION](DESCRIPTION) ainda declara R ≥ 3.5 e precisa ser alinhado ao código.

As dependências declaradas são instaladas conforme necessário. O pacote utiliza, entre outras, `openxlsx`, `dplyr`, `ggplot2`, `COINr`, `forecast`, `bestNormalize`, `Hmisc`, `sf`, `officer` e `flextable`. Em Linux, algumas dependências, especialmente `sf` e `rsvg`, podem exigir bibliotecas do sistema.

## Começar com os dados de exemplo

O pacote inclui uma planilha para demonstrar a leitura de dados e metadados:

```r
library(reprodutibilidade)

exemplo <- read_exemplo_xlsx()

head(exemplo$metadados)
head(exemplo$dataset)
```

Também estão disponíveis objetos de exemplo para indicadores do nível 7, seus metadados e as referências municipais:

```r
data("datasetN7", package = "reprodutibilidade")
data("metadadosN7", package = "reprodutibilidade")
data("data_ref", package = "reprodutibilidade")

# Conferir a correspondência e a ordem dos metadados
pos <- match(colnames(datasetN7), metadadosN7$CODE)
stopifnot(!anyNA(pos))
meta <- metadadosN7[pos, , drop = FALSE]

# Resumo descritivo dos indicadores
resumo <- ADPresumo(
  dataset = datasetN7,
  class_types = meta$CLASSE,
  names = colnames(datasetN7)
)

resumo$resumo_total
resumo$resumo_basico
resumo$resumo_na
```

Para consultar os argumentos e os retornos das funções:

```r
?read_exemplo_xlsx
?ADPresumo
?Tratamento
```

## Funcionalidades disponíveis

Os nomes abaixo correspondem às funções exportadas em [NAMESPACE](NAMESPACE).

| Etapa | Funções | Finalidade |
| --- | --- | --- |
| Leitura | `read_exemplo_xlsx()`, `carregar_xlsx()` | Carregar a base de exemplo ou arquivos Excel. |
| Diagnóstico descritivo | `criar_resumo()`, `ADPresumo()`, `total.na()` | Resumir distribuições, valores ausentes, valores únicos e extremos. |
| Winsorização | `winsorize_info()`, `winsorize_data()`, `winsorize_apply()` | Calcular limites e ajustar valores extremos; consultar as limitações abaixo. |
| Transformação | `boxcox_transform()`, `ADPBoxCox()` | Aplicar Box-Cox ou Yeo-Johnson, conforme o método escolhido. |
| Normalização | `sfunc_norm()`, `ADPNormalise()` | Aplicar normalização min–max. |
| Associação entre indicadores | `ADPcorrel()`, `correl_ind()` | Calcular correlações de Spearman e resumos de diagnóstico. |
| Gráficos e mapas | `criar_grafico()`, `grafico_final()`, `FigContNA()`, `FigCorrelPlot()`, `map_result()`, `map_result_normal()` | Representar distribuições, ausências, correlações e padrões espaciais. |
| Estrutura dos indicadores | `gerar_diagrama_setor()` | Representar a hierarquia dos indicadores. |
| Tratamento integrado | `Tratamento()` | Encadear etapas e exportar planilhas com diagnósticos e dados processados. |
| Tabelas e apresentações | `cria_flextables_descricao()`, `monta_ppt_descricao()`, `monta_ppt_process()`, `monta_ppt_normal()` | Organizar resultados em tabelas e slides PowerPoint. |

As rotinas de VIF e alfa de Cronbach presentes em `inst/rotinas_extras/` são complementares e **não estão exportadas na interface principal do pacote**.

## Organização dos arquivos

| Caminho | Conteúdo |
| --- | --- |
| [R/](R/) | Código das funções do pacote. |
| [man/](man/) | Documentação das funções e dos dados em formato de ajuda do R. |
| [data/](data/) | Objetos de exemplo e bases espaciais em formato `.rda`. |
| [inst/dataset/](inst/dataset/) | Planilha Excel de exemplo. |
| [inst/templates/](inst/templates/) | Modelo de apresentação PowerPoint. |
| [inst/rotinas_extras/](inst/rotinas_extras/) | Rotinas complementares fora da interface principal. |
| [vignettes/](vignettes/) | Tutorial em R Markdown e figuras. |
| [doc/](doc/) | Tutorial em R Markdown, script R e versão HTML. |
| [nao_implementado/](nao_implementado/) | Rotinas experimentais fora da implementação principal. |
| [DESCRIPTION](DESCRIPTION) e [NAMESPACE](NAMESPACE) | Metadados, dependências, importações e funções exportadas. |
| [reprodutibilidade.Rproj](reprodutibilidade.Rproj) | Projeto para uso no RStudio. |

## Formato de entrada

A função `Tratamento()` recebe um arquivo Excel com uma planilha de metadados e uma planilha de dados.

### Metadados

O fluxo utiliza os seguintes campos:

| Campo | Informação |
| --- | --- |
| `N` | Identificador sequencial. |
| `NIVEL` | Nível hierárquico do índice ou indicador. |
| `CODE` | Código correspondente ao nome da coluna na base de dados. |
| `NOME` | Nome descritivo do índice ou indicador. |
| `TIPO` | Tipo do elemento na estrutura de indicadores. |
| `PAI` | Código do elemento imediatamente superior na hierarquia. |
| `CLASSE` | Classe utilizada pelas rotinas; indicadores numéricos usam `"Numerico"`. |

### Dados

Cada linha representa uma unidade municipal. Na rotina integrada, as **três primeiras colunas** são reservadas às referências, como código municipal, nome e UF. As demais devem conter os indicadores numéricos a processar.

Os nomes das colunas dos indicadores precisam corresponder a `CODE`. A quantidade e a ordem dos metadados selecionados por `NIVEL` devem coincidir com as colunas de indicadores: a rotina atual não faz esse alinhamento automaticamente.

Preserve os identificadores municipais fora das transformações e represente ausências como `NA`, sem substituí-las automaticamente por zero.

## Fluxo de tratamento e produtos

O fluxo integrado reúne leitura, resumo descritivo, winsorização, transformação condicional e normalização min–max.

A interface de `Tratamento()` permite informar:

- `input`: caminho do arquivo Excel;
- `metadados` e `dataset`: nomes das planilhas;
- `nivel`: nível dos indicadores; quando omitido, utiliza 7;
- `method_boxcox`: `"forecast"`, `"COINr"` ou `"yeojohnson"`; quando omitido, utiliza `"forecast"`;
- `sigla` e `subsetor`: identificação nos nomes dos arquivos de saída.

A função foi estruturada para gerar dois arquivos com data e hora no nome:

| Arquivo | Conteúdo previsto |
| --- | --- |
| `ANALISE_DESCRITIVA_*.xlsx` | Resumo descritivo e diagnósticos de winsorização e transformação. |
| `DADOS_TRATADOS_*.xlsx` | Referências municipais e dados nas diferentes etapas do tratamento. |

Na implementação atual, os arquivos são gravados no **diretório de trabalho do R**, consultável com `getwd()`. A pasta `OUTPUT/` mencionada em parte da documentação não é criada automaticamente.

## Estado atual e limitações

A versão declarada é **0.0.0.9000**, de desenvolvimento. A inspeção do código identificou pontos que precisam ser corrigidos e verificados antes de utilizar o fluxo integrado em produção:

- **Winsorização:** `winsorize_apply()` consulta `linfg`, enquanto o resumo fornece `linf`; isso compromete o ajuste do limite inferior. Colunas fora da classe `"Numerico"` também podem permanecer preenchidas com `NA`.
- **Transformação:** `boxcox_transform()` utiliza uma seleção que produz um vetor vazio quando a entrada não contém `NA`. Além disso, `ADPBoxCox()` avalia a assimetria e a curtose nos dados winsorizados, mas aplica a transformação aos dados originais quando o critério é atendido.
- **Normalização:** colunas constantes geram divisão por zero. A rotina não implementa o tratamento de colunas não numéricas descrito em sua documentação.
- **Exportação:** `Tratamento()` consulta `Data_Win$Resumo`, embora `winsorize_apply()` retorne o elemento `resumo`; isso compromete a exportação do diagnóstico de winsorização.
- **Correlações:** `ADPcorrel()` retorna o valor absoluto da correlação de Spearman, sem preservar o sinal. Em `correl_ind()`, os limites de 55 ausências e de correlação ≥ 0,6 estão fixos no código; as sugestões de remoção exigem avaliação metodológica.
- **Tutorial:** alguns trechos ainda apontam a instalação para `AdaptaBrasil/reprodutibilidade` e apresentam divergências em relação às funções atuais. Para instalar esta versão, utilize o endereço informado neste README.

A winsorização implementada utiliza limites baseados em quartis e **1,5 vezes o intervalo interquartil**, e não percentis fixos de corte. A transformação e a normalização, por si só, não demonstram validade do indicador nem garantem normalidade dos dados.

Para documentar uma execução, registre a versão ou o commit do pacote, a origem dos dados, os parâmetros utilizados e `sessionInfo()`.

## Tutorial

Consulte o [tutorial em R Markdown](vignettes/tutorial_reprodutibilidade.Rmd) e o [script de demonstração](doc/tutorial_reprodutibilidade.R).

A [versão HTML](doc/tutorial_reprodutibilidade.html) pode ser baixada e aberta no navegador. O GitHub normalmente mostra esse arquivo como código, sem renderizar o tutorial.

## Autoria e licença

O arquivo `DESCRIPTION` registra **AdaptaBrasil / INPE** como autor e mantenedor. O tutorial é assinado por **Nete Barreto**.

O código está disponibilizado sob a **licença MIT**, com copyright de 2025 do **Instituto Nacional de Pesquisas Espaciais (INPE)**, conforme [LICENSE.md](LICENSE.md) e [LICENSE](LICENSE).

Ao reutilizar bases de dados e malhas geográficas, verifique também as condições de uso e a atribuição das respectivas fontes.

## Contribuições

Problemas e sugestões podem ser registrados nas [issues do repositório](https://github.com/netebarreto/Reprodutibilidade/issues). Para relatar um erro, inclua a função utilizada, um exemplo mínimo, a mensagem recebida e a saída de `sessionInfo()`.

