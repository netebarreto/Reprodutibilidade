

#### SESSAO INICIADA 

git init
git clone https://github.com/netebarreto/ADAPTA_DEV.git

# Removendo o Arquivo do Git 20240924
git rm DESCRITORES/Texto.txt
git rm DESCRITORES/Texto.txt
git commit -m "Removendo o arquivo Texto.txt"

git mv SCRPITS/Untitled-1.sh SCRPITS/AB00-COMANDOS_GIT.sh
git mv SCRPITS/Rotina_Teste.R SCRPITS/ROTINA_DEV.R

git commit -m "Removendo o arquivo Texto.txt"


###### 20240925
git mv SCRIPTS/ROTINA_DEV.R SCRIPTS/AA01-ROTINA_DEV.R
git commit -m "Renomeando arquivo"

git add . 
git commit -m "Atualizações 20240925"

git push -u origin adapta-dev

git pull origin adapta-dev
  
    ###### REPRODUTIBILIDADE ######
    
    git clone https://github.com/netebarreto/Reprodutibilidade.git

    git remote add maindir https://github.com/netebarreto/Reprodutibilidade.git

    git checkout -b rep_adapta

    git add . 
    git commit -m "Repositório reestruturado"
    git push -u origin rep_adapta

# Localiza repositorio local

cd Documents/NETE_PROJETOS/AB01-INSTITUCIONAIS/INPE/ADAPTABRASIL/GIT_ADAPTA/Reprodutibilidade/


    ## Removendo arquivo 
    git rm SCRIPTS/AB00-COMANDOS_GIT.sh
    git commit -m "Removendo Arquivo" 
    git pull origin rep_adapta

    echo "SCRIPTS/AA01-ROTINA_DEV.R" >> .gitignore
    echo "SCRIPTS/AB00-COMANDOS_GIT.sh" >> .gitignore

######

    git init

    git add SCRIPTS/AA00-OPEN_FILES_INPUT_202408.R 
    git commit -m "Atualização do Winzorization #11"

    git add OUTPUT/ 
    git commit -m "Figuras de Comparação"

    git add DATASET/IMeta_RH_INDBRT.csv 
    git commit -m "Inclusão da Coluna Score_ADP #11"


    git add README.md 
    git commit -m "Atualização 20240930 #11"

    git push -u origin rep_adapta


###### 20241007

cd c:/Users/Nete/Documents/NETE_PROJETOS/AB01-INSTITUCIONAIS/INPE/ADAPTABRASIL/GIT_ADAPTA/reprodutibilidade

    git init
    git rm -r OUTPUT/
    git commit -m "Remoção pasta"

    git add DATASET/IMeta_RH_INDBRT.csv 
    git commit -m "Inclusão da Coluna de contador simples #11"

    git add RESPOSTAS/FIGURAS/IS/ 
    git commit -m "Figuras Atualizadas #11"  

#    git add README.md 
#    git commit -m "Atualização 20241009 #11"   
 
cd c:/Users/Nete/Documents/NETE_PROJETOS/AB01-INSTITUCIONAIS/INPE/ADAPTABRASIL/GIT_ADAPTA/reprodutibilidade


    git init

    git add DATASET/FUN_VALORES_DES_INUND_20231213_R06.xlsx 
    git commit -m "Planilha para comparação #11" 

    git add SCRIPTS/AA00-OPEN_FILES_INPUT_202408.R 
    git commit -m "Inicio BoxCox #16"     

    git add RESPOSTAS/FIGURAS/COMPARE_WINSORISE.png
    git commit -m "Boxplot Comparando o Winsorise da planilha com a função #11" 


    git push -u origin rep_adapta

    git pull origin rep_adapta

####### 30/10/2024


cd c:/Users/Nete/Documents/NETE_PROJETOS/AB01-INSTITUCIONAIS/INPE/ADAPTABRASIL/GIT_ADAPTA/reprodutibilidade


    git init

    git add SCRIPTS/FUNCTION/ 
    git commit -m " Diretorio com as rotinas " 

    git add SCRIPTS/FUNCTION/ADPwinsorise.r 
    git commit -m "Funçao Wisorization #11" 

    git add SCRIPTS/FUNCTION/ADPBoxCox.r 
    git commit -m "Funçao Boxcox #16" 

    git add SCRIPTS/FUNCTION/ADPNormalise.r 
    git commit -m "Funçao Normalização Iniciada #14" 

    git add SCRIPTS/AA00-OPEN_FILES_INPUT_202408.R
    git commit -m "Atualização Rotina principal" 

    git push -u origin rep_adapta

    git pull origin rep_adapta


####### 02/12/2024


cd c:/Users/Nete/Documents/NETE_PROJETOS/AB01-INSTITUCIONAIS/INPE/ADAPTABRASIL/GIT_ADAPTA/reprodutibilidade


    git init

    git add DATASET/Base_inicial_RH_INDBRT.xlsx 
    git commit -m "Base de Entrada Remodelada" 

    git add SCRIPTS/FUNCTION/ADPResumo.r 
    git commit -m "Função Resumo" 

    git add SCRIPTS/FUNCTION/ADPwinsorise.r 
    git commit -m "Atualização Funçao Wisorization #11" 

    git add SCRIPTS/FUNCTION/ADPBoxCox.r 
    git commit -m " Atualização Funçao Boxcox #16" 

    git add SCRIPTS/FUNCTION/ADPNormalise.r 
    git commit -m "Funçao Normalização Iniciada #14" 

    git add SCRIPTS/AA00-OPEN_FILES_INPUT_202408.R
    git commit -m "Atualização Rotina principal" 

    git push -u origin rep_adapta

    git pull origin rep_adapta



####### 16/12/2024
cd c:/Users/Nete/Documents/NETE_PROJETOS/AB01-INSTITUCIONAIS/INPE/ADAPTABRASIL/GIT_ADAPTA/reprodutibilidade


    git init

    git add DATASET/Base_inicial_RH_INDBRT.xlsx 
    git commit -m "Base de Entrada Remodelada" 

    git add SCRIPTS/FUNCTION/ADPResumo.r 
    git commit -m "Função Resumo" 

    git add SCRIPTS/FUNCTION/ADPwinsorise.r 
    git commit -m "Atualização Funçao Wisorization #11" 

    git add SCRIPTS/AA00-OPEN_FILES_INPUT_202408.R
    git commit -m "Atualização Rotina principal" 

    git push -u origin rep_adapta

    git pull origin rep_adapta

####### 15/05/2025

cd c:/Users/Nete/Documents/NETE_PROJETOS/AB01-PROJETOS_INSTITUCIONAIS/INPE/ADAPTABRASIL/GIT_ADAPTA/reprodutibilidade

    git init

    git add OUTPUT/ 
    git commit -m "local dos arquivos de saida"

    git add SCRIPTS/AA01-INICIO_DESCRITIVO_INDICADORES.R
    git commit -m "Atualização da rotina principal"

    git rm SCRIPTS/AA00-OPEN_FILES_INPUT_202408.R
    git commit -m "removendo arquivo versão antiga"


    git add SCRIPTS/FUNCTION/  
    git commit -m "Diretorio com as rotinas"

    
    git add SCRIPTS/FUNCTION/F05_ADPGraficos.r
    git commit -m "F05_ADPGraficos.r"
    git add SCRIPTS/FUNCTION/F03_ADPBoxCox.r     
    git commit -m "F03_ADPBoxCox.r"
    git add SCRIPTS/FUNCTION/F06_ADPCriar_pptx_E01.R
git commit -m "F06_ADPCriar_pptx_E01.R"
    git add SCRIPTS/FUNCTION/F01_ADPResumo.r   
git commit -m "F01_ADPResumo.r"
    git add SCRIPTS/FUNCTION/F04_ADPNormalise.r
git commit -m "F04_ADPNormalise.r"


    git commit -m "removendo diretorio"

    git add DATASET/Base_inicial_SA_Acesso.xlsx
    git commit -m "Base de Entrada Segurança Alimentar"    

    git add DATASET/SHP
    git commit -m "shapefiles para o mapa"

    git add TEMPLATE
    git commit -m "TEMPLATES"

    git push -u origin rep_adapta

    git pull origin rep_adapta 



git add .
git commit -m "Salvando alterações locais antes do merge"

# Depois, traga as mudanças remotas:
git pull origin rep_adapta --rebase    # ou use só `git pull` e depois resolva conflitos

# Se necessário, resolva conflitos e finalize o rebase com:
git rebase --continue

# Por fim, envie tudo para o remoto:
git push origin rep_adapta
