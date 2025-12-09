# reprodutibilidade  
Ferramentas para análise estatística, validação e reprodutibilidade dos indicadores do AdaptaBrasil.

![Status](https://img.shields.io/badge/status-stable-brightgreen)
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

## 🧭 Funcionalidades Principais
### 1. Pré-processamento

Ferramentas voltadas ao ajuste inicial dos dados:  

  - ADPwinsorise() — Winsorização e controle de outliers
    
  - ADPBoxCox() — Transformação Box-Cox
    
  - ADPNormalise() — Normalização padronizada de variáveis

### 2. Análise e Validação

Funções destinadas a examinar estrutura, distribuição e consistência:

 - criar_resumo() — Resumo estatístico completo

 - calc_correlacoes() — Correlação total, parcial e métricas associadas

 - gerar_diagrama_setor() — Estrutura hierárquica das variáveis com DiagrammeR

## 3. Visualização e Produtos

Rotinas capazes de gerar saídas padronizadas:

- grafico_final() — Gráficos normalizados

- Map_result() — Mapas temáticos por município ou unidade federativa

- monta_excel() — Organização e exportação de resultados em planilhas Excel

- monta_ppt() — Criação automatizada de apresentações PowerPoint

## 🔗 Dependências

O pacote depende, entre outros, dos seguintes pacotes:

- dplyr

- tidyr

- ggplot2

- DiagrammeR

- DiagrammeRsvg

- rsvg

- psych

- Hmisc

- COINr

A lista completa encontra-se no arquivo DESCRIPTION do pacote.

## 🚀 Fluxo de Uso Sugerido
### 1. Pré-processamento



### 2. Estatísticas e validação



### 3. Produção de visualizações


### 4. Exportação de resultados


📘 Exemplo Completo 
```r

```

## 📁 Estrutura do Projeto  

reprodutibilidade/  
 ├── R/                # Funções principais do pacote  
 
 ├── man/              # Documentação (arquivos .Rd)  
 
 ├── vignettes/        # Tutoriais e demonstrações  
 
 ├── inst/             # Arquivos auxiliares  
 
 ├── DESCRIPTION       # Metadados do pacote  
 
 └── NAMESPACE         # Exportação/importação de funções  

## 📄 Licença

---

