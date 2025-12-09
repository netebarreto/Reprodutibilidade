# reprodutibilidade  
Ferramentas para análise estatística, validação e reprodutibilidade dos indicadores do AdaptaBrasil.

![Status](https://img.shields.io/badge/status-stable-brightgreen)
![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)
![R >= 4.2](https://img.shields.io/badge/R-%3E%3D4.2-lightgrey)
![Dev Version](https://img.shields.io/badge/dev-GitHub-black)

---

## 📌 Visão Geral

O pacote **reprodutibilidade** fornece ferramentas para apoiar o fluxo completo de construção de indicadores no contexto do AdaptaBrasil.  
Permite pré-processamento, normalização, geração de estatísticas, validação estrutural, visualização e criação de produtos finais para análise e comunicação.

O objetivo principal é garantir **reprodutibilidade**, **consistência** e **rastreabilidade** em todas as etapas do processo.

---

## 📦 Instalação

### Instalação da versão de desenvolvimento via GitHub

```r
install.packages("devtools")
devtools::install_github("AdaptaBrasil/reprodutibilidade")
```

🧭 Funcionalidades Principais
1. Pré-processamento

Ferramentas voltadas ao ajuste inicial dos dados:

ADPwinsorise() — Winsorização e controle de outliers

ADPBoxCox() — Transformação Box-Cox

ADPNormalise() — Normalização padronizada de variáveis

2. Análise e Validação

Funções destinadas a examinar estrutura, distribuição e consistência:

criar_resumo() — Resumo estatístico completo

calc_correlacoes() — Correlação total, parcial e métricas associadas

gerar_diagrama_setor() — Estrutura hierárquica das variáveis com DiagrammeR

3. Visualização e Produtos

Rotinas capazes de gerar saídas padronizadas:

grafico_final() — Gráficos normalizados

Map_result() — Mapas temáticos por município ou unidade federativa

monta_excel() — Organização e exportação de resultados em planilhas Excel

monta_ppt() — Criação automatizada de apresentações PowerPoint

🔗 Dependências

O pacote depende, entre outros, dos seguintes pacotes:

dplyr

tidyr

ggplot2

DiagrammeR

DiagrammeRsvg

rsvg

psych

Hmisc

COINr

A lista completa encontra-se no arquivo DESCRIPTION do pacote.

🚀 Fluxo de Uso Sugerido
1. Pré-processamento
w <- ADPwinsorise(dados, meta, ref = dados$CLUSTER)
b <- ADPBoxCox(w, meta)
n <- ADPNormalise(b, meta)

2. Estatísticas e validação
resumo <- criar_resumo(n, meta)
cor    <- calc_correlacoes(n)
diag   <- gerar_diagrama_setor(meta)

3. Produção de visualizações
grafico_final(n, meta)
Map_result("Indicador", dados_municipios, ano_ref)

4. Exportação de resultados
monta_excel(resumo)
monta_ppt(resumo)

📘 Exemplo Completo
library(reprodutibilidade)

dados <- data.frame(
  var1 = c(1, 2, 3, 100),
  var2 = c(10, 20, 30, 40),
  CLUSTER = c(1, 1, 2, 2)
)

meta <- data.frame(
  Classe = c("Numerico", "Numerico", "Cluster"),
  Code   = c("var1", "var2", "CLUSTER")
)

ref <- dados$CLUSTER

ADPwinsorise(dados, meta, ref)

📁 Estrutura do Projeto
reprodutibilidade/
├── R/                # Funções principais do pacote
├── man/              # Documentação (arquivos .Rd)
├── vignettes/        # Tutoriais e demonstrações
├── inst/             # Arquivos auxiliares
├── DESCRIPTION       # Metadados do pacote
└── NAMESPACE         # Exportação/importação de funções

🤝 Contribuindo

Contribuições são bem-vindas.
Pull requests podem abranger melhorias em desempenho, documentação, testes, exemplos ou novas funcionalidades relacionadas ao escopo do pacote.

📄 Licença

Este projeto está licenciado sob MIT License.


---

