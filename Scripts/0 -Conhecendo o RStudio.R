####1 - CONHECENDO O R STUDIO####

#Olá :)
#Isso é um script do R.
#Para criar um novo script, acesse File > New File > RScript
#Abra um novo script, copie e cole todo o texto daqui nesse novo script.
#Agora, salve esse script na pasta Scripts com o nome "Meu_primeiro_script_dia-mes-ano".
#Para abrir um script já existente, basta ir em File > Open File
#Arquivos de scripts do R costumam terminar em ".R".

#Para executar uma linha (ou funçâo) do script, você pode clicar em Run (aqui em cima do lado direito) ou segurar Ctrl e apertar Enter
#Não se preocupe com a posição do cursor, ele pode estar em qualquer posição da linha
#Tente fazer isso aqui, usando as duas opçães e observe o Console aqui embaixo
1 + 1

#Você também pode digitar diretamente no console e apertar Enter. Tente fazer isso

#Perceba que nada acontece quando eu coloco uma # no inicio da linha
#As # são usadas para fazer comentários.
1 + 1 #Operação simples
#Veja o que acontece se você não colocar # para fazer um comentario
1 + 1 Operação simples
#Isso acontece porque o R não reconhece o Operação nem como uma função, nem como um objeto

####COMO FAZER TITULOS ####
#Para fazer títulos no script, basta adicionar 4 hashtags
####Aqui é outro título####
#Observe a aba de títulos, que separa a área de script do console

#### MUDAR TEMA DO R STUDIO ####
#Acesse as opçães: Tools > Global Options > Appearance > Editor Theme
#Acesse as opçães: Tools > Global Options > Code > Edditing e marque a opção soft-wrap R source files


####Projetos no R####
#Um projeto nada mais é do que uma pasta no seu computador. Nessa pasta, estarão todos os arquivos que você usaá ou criará na sua análise.
#Para criar um projeto, acesse File > New Project.
#Para testar, crie um projeto no seu computador chamado "Meu_primeiro_projeto".
#Dica: Na pasta do projeto, evite usar espaços e caracteres especiais.
#Após criar o projeto, feche o RStudio e abra novamente.

####PASTA DE TRABALHO####
# Para descobrir em que pasta você está, utilize o comando:
getwd()
#Podemos salvar esse caminho em um objeto
diretorio_atual <- getwd()
# Para mudar a pasta de trabalho, utilize o comando setwd("novo caminho"), onde o novo caminho pode ser uma pasta dentro da atual pasta de trabalho, ou um caminho completo para outra pasta
setwd("../") # "../" retorna para a pasta raiz anterior a pasta de trabalho
getwd() #Pasta de trabalho atual
# Voltar para pasta anterior
# Você também pode utilizar o comando Ctrl + Shift + H para abrir uma janela para determinar o diretório de trabalho

#Partes do RStudio:
#Script aqui!
#Environment (e outras opções) do lado  superior direito: onde aparecem os objetos criados
# Console aqui em baixo: onde aparece tudo que você executa.
#Files (ver arquivos da pasta raiz), plots (graficos), Packages (pacotes instalados e carregados), Help (ajuda com as funcoes
#Explore as opcoes do Help

#### Conhecendo alguns atalhos: ####
# CTRL+ENTER: roda a(s) linha(s) selecionada(s) no script. O atalho mais utilizado.
# ALT + -: cria no script um sinal de atribuicao (<-). Voce o usara o tempo todo.
# Ctrl+Shift+C: transforma codigo em comentario e vice-versa. Tente fazer isso com essa linha
# CTRL+SHIFT+M: (%>%) operador pipe. Guarde esse atalho, voce o usara bastante.
# Posicione o cursos sobre a funcao e tecle F1: pedir ajuda
sum #pede ajuda para essa funcao
?sum #assim tambem funciona
# Posicione o cursos sobre a funcao e tecle F2: abrir codigo base da funcao
florabr::filter_florabr()