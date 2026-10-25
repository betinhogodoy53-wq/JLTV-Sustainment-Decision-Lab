# JLTV Sustainment Decision Lab

**Código-fonte para avaliação acadêmica — versão declarada no pacote: 1.5.7**

Aplicativo R/Shiny utilizado na pesquisa sobre a indisponibilidade da frota JLTV do Corpo de Fuzileiros Navais. O código reúne questionário, AHP–TOPSIS, simulação de eventos discretos e geração de relatórios.

## Como executar no Windows

1. Instale o R e baixe o repositório por **Code → Download ZIP**. Extraia completamente o ZIP.
2. Abra o arquivo `01_ABRIR_SIMULADOR.cmd` na pasta extraída.
3. O iniciador procura o R, verifica as dependências de `02_CODIGO/install_dependencies.R` e oferece sua instalação se necessário.
4. O aplicativo será iniciado localmente no navegador. Mantenha a janela do processo R aberta.

**Importante:** o GitHub hospeda o código, mas não executa automaticamente aplicativos R/Shiny. Para acesso remoto sem instalar R seria necessário hospedar o aplicativo em um serviço próprio.

## Arquivos

- `02_CODIGO/app.R`: aplicação principal.
- `02_CODIGO/run_app.R`: abertura do aplicativo Shiny.
- `02_CODIGO/jltv_*.R`: módulos auxiliares.
- `02_CODIGO/install_dependencies.R`: dependências.
- `02_CODIGO/www/`: recursos visuais necessários.
- `JLTV_Sustainment_Decision_Lab_CODIGO_COMPLETO.R`: consolidação do código para consulta; para executar, mantenha também a estrutura de pastas.
- `04_TESTES/`: testes incluídos no pacote original.
- `03_DOCUMENTACAO/MAPA_DO_CODIGO.md`: mapa dos componentes.

## Dados e limitações

Esta seleção para publicação **não inclui** o banco SQLite do ZIP original, registros de execução, arquivos de histórico, relatórios pessoais nem respostas de especialistas. Para reproduzir os resultados numéricos específicos do TCC, é necessário usar as entradas e configurações correspondentes ao experimento documentado. O aplicativo pode criar um banco de dados local na primeira execução.

Os pesos AHP–TOPSIS exprimem prioridades atribuídas pelos especialistas e não são estimativas de taxas de falha ou tempos de reparo. Os resultados simulados dependem das hipóteses de cenário e não equivalem a dados observados da frota.

## Privacidade

Não publique credenciais, chaves de API, dados de identificação dos respondentes ou material de divulgação restrita. A utilização de serviços externos pelo aplicativo depende de configurações próprias do avaliador.

## Observações da verificação

O pacote foi reorganizado para publicação preservando os códigos executáveis e recursos selecionados. Isso não constitui certificação da plena execução em computadores diferentes nem validação empírica dos resultados da pesquisa.
