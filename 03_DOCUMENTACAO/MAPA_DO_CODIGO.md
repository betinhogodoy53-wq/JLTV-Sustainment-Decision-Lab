# Mapa do código — JLTV Sustainment Decision Lab 1.5.7

| Arquivo em 02_CODIGO | Papel e ordem de leitura |
|---|---|
| app.R | Arquivo principal: funções analíticas, acesso ao SQLite, interface Shiny e eventos de interação. Carrega os módulos abaixo. |
| jltv_systems.R | Extensão por sistemas/componentes (SYS-1); carrega jltv_enhancements.R. |
| jltv_enhancements.R | Funções complementares carregadas pela extensão de sistemas. |
| jltv_multicriteria_views.R | Visualizações de resultados multicritério. |
| jltv_word_report.R | Construção do relatório Word e integração opcional de IA. |
| run_app.R | Carrega o aplicativo, registra a abertura e inicia o servidor Shiny local. |
| install_dependencies.R | Lista e instalação das dependências. |

## Organização

Os sete arquivos R acima são cópias integrais dos módulos atuais da instalação de origem. A organização não divide nem reescreve a lógica científica. A interdependência de arquivos exige manter todos os módulos na mesma pasta.

O iniciador na raiz apenas localiza o R, verifica os pacotes e executa run_app.R com uma base própria em DADOS, sem dados sintéticos automáticos.

## Documentos existentes

Os documentos FONTES_E_ORIENTADOR, AUDITORIA_24_ORIENTADOR e ATUALIZACAO_TCC foram copiados da instalação. Seu conteúdo e suas datas não foram revisados nesta organização. README_EXECUCAO.md permanece junto ao código porque o próprio aplicativo o identifica como documentação local; contém histórico de versões.

## Integridade

INTEGRIDADE_MODULOS.json registra tamanho e SHA-256 de cada módulo R copiado. O pacote não inclui versões .bak, caches, bancos SQLite ou relatórios pessoais.
