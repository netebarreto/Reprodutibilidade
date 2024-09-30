# Construção, Validação e Reprodutibilidade de Indicadores no AdaptaBrasil 

OBJETIVO GERAL

* Apoiar as atividades do AdaptaBrasil relativas à construção, validação e reprodutibilidade dos indicadores produzidos para a plataforma.

OBJETIVOS ESPECÍFICOS

   - 1.	Testar a ferramenta de validação e corrigir dados atualmente publicados para o novo formato;
   - 2.	Estudar o processo de construção de indicadores no AdaptaBrasil.
   - 3.	Desenvolver scripts R para criação dos indicadores do AdaptaBrasil usando o pacote COINr.;
   - 4.	Elaborar um tutorial de como usar o COINr para criar os índices do AdaptaBrasil; 
   - 5.	Elaborar o Relatório final de atividades, apontando inclusive as limitações do pacote COINr para a criação dos índices do AdaptaBrasil.

BOLSISTA  
* Nete Barreto - Bolsista PCI-DA/INPE-DIIAV - naurinete.barreto@gmail.com

DATAS: 
* 20/07/2024
* 30/08/2024 
* 15/09/2024
* 20/09/2024

Informações Basicas: 
- Versão do R:
  * "R version 4.3.2 (2023-10-31 ucrt)"
- Pacotes Instalados: 
   * COINr_1.1.14
   * lubridate_1.9.3  
   * openxlsx_4.2.6.1     

ARQUIVOS: 
* *IData_RH_INDBRT:* é os arquivos com os dados organizados dos indicadores simples, apenas para o nível  1, no formato de leitura do COINr
* *IMeta_RH_INDBRT:* - São os metadados dos indicadores

#### Arquivos gerados manualmente baseados no arquivo: 
  * Planilha de funções - Valores - desastres_inundação - 13-12-23_revisao6 p script.xlsx

##### "Winsorization" 23-09-2024 
  ** Incluisão de uma coluna no iMeta _ [Score_ADP] _ 
  * Score_ADP = 1; Coluna do tipo Score - Não Aplicar Winsorization 
  * Score_ADP = 0; Coluna Numérica - Aplicar o  Winsorization 
  * Score_ADP = -1; Coluna do tipo Cluster (Urbano/Não Urbano), fazer avaliação depois