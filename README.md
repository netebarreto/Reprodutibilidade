# Construção, Validação e Reprodutibilidade de Indicadores no AdaptaBrasil 

OBJETIVO GERAL

* Apoiar as atividades do AdaptaBrasil relativas à construção, validação e reprodutibilidade dos indicadores produzidos para a plataforma.

OBJETIVOS ESPECÍFICOS

   - 1.	Testar a ferramenta de validação e corrigir dados atualmente publicados para o novo formato;
   - 2.	Estudar o processo de construção de indicadores no AdaptaBrasil.
   - 3.	Desenvolver scripts R para criação dos indicadores do AdaptaBrasil;
   - 4.	Elaborar um tutorial de como usar o pacote para criar os índices do AdaptaBrasil;
        
BOLSISTA  
* Nete Barreto - Bolsista DTI-A/INPE-DIIAV - naurinete.barreto@inpe.br

DATAS INICIO: _20/07/2024_

## Pacote _reprodutibilidade_ 

Pacote em R para análise estatística e reprodutibilidade dos indicadores simples, incluindo funções: 

- Criação de diagrama,
- Resumos Estatíticos,
- Winsorização,
- BoxCox,
- Normalização
- geração de gráficos e mapas,
- cálculos de correlação total, parcial e outras metricas,
- Criação de Arquivos Excell com resultados
- Criação de Apresentação em Powrpoint 

---

## 📦 Instalação

Você pode instalar a versão de desenvolvimento diretamente do GitHub:

```r
# Instalar o pacote devtools, se ainda não tiver
install.packages("devtools")

# Instalar o pacote reprodutibilidade do GitHub
# devtools::install_github("/reprodutibilidade")
```
> <div class="custom-block">
  <strong>Funcionalidades Principais:</strong>
> ADPNormalise() — Normalização de variáveis.  
> ADPwinsorise() — Winsorização de dados para lidar com outliers.  
> criar_resumo() — Criação de resumos estatísticos (mínimo, quartis, mediana, máximo, outliers, NAs).  
> grafico_final() — Geração de gráficos com dados normalizados.  
> gerar_diagrama_setor() — Criação de diagramas hierárquicos com DiagrammeR.  
> Map_result() — Visualização de mapas (municípios/UF).  



> <div class="custom-block">
  <strong>IMPORTANTE:</strong> O pacote depende dos seguintes pacotes R:  
> dplyr  
> ggplot2   
> tidyr  
> DiagrammeR  
> DiagrammeRsvg  
> rsvg
> psych  
> Hmisc  
> COINr  
> e outros listados em DESCRIPTION.
</div>



```R
library(reprodutibilidade)

# Criando dados fictícios
dados <- data.frame( var1 = c(1, 2, 3, 100),
                     var2 = c(10, 20, 30, 40),
                    CLUSTER = c(1, 1, 2, 2))

meta <- data.frame( Classe = c("Numerico", "Numerico", "Cluster"),
                    Code   = c("var1", "var2", "CLUSTER"))

# Winsorização
ref <- dados$CLUSTER
ADPwinsorise(dados, meta, ref)
```
