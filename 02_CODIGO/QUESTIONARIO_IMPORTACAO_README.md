# Questionário JLTV — conexão on-line AHP–TOPSIS

Versão do aplicativo: **JLTV Sustainment Decision Lab 1.3.0**.

## Onde trabalhar

O questionário fica no **aba 1. Dados**, painel **Questionário on-line e sincronização**.

O cálculo fica no **aba 2. Prioridades**.

O fluxo normal é:

`Google Forms → Google Sheets → aplicativo → AHP → TOPSIS → vínculo de cenário → simulação`

Não é necessário baixar CSV para o fluxo normal.

## Conexão Google

Na primeira utilização em cada computador:
1. abra `1. Dados`;
2. selecione `Google Sheets — automático`;
3. confirme o link da planilha e `Form Responses 1`;
4. clique `Conectar Google`;
5. autorize a conta com acesso à planilha;
6. após a autenticação, a sincronização automática é ativada.

O token OAuth fica fora da pasta sincronizada:
`%LOCALAPPDATA%\JLTV_RAM_Decision_Lab\oauth`

## Coexistência das fontes

A Google Sheets é a fonte principal, mas não apaga os outros modos de entrada.

A sincronização on-line substitui apenas linhas `_GOOGLE_SHEETS`.

Permanecem preservadas:
- respostas `_MANUAL`;
- respostas `_FILE`;
- dados RAM/CMMS cadastrados por outras rotas.

CSV/XLSX e cadastro manual continuam disponíveis como fallback.

## Transformação

Cada resposta completa gera:
- 1 `questionnaire_respondents`;
- 14 `ahp_pairwise`;
- 100 `topsis_scores`;
- até 4 `questionnaire_factors`.

As colunas `[Row 1]` são ignoradas.

P7 é mantido em campos separados:
- `identification`;
- `occurrence_detail`;
- `related_system`;
- `evidence_reference`.

`influence_score` permanece `NA`, pois o formulário não pergunta uma nota P7 independente.

## Árvore hierárquica

O Passo 2 exibe uma árvore gerada diretamente de `questionnaire_hierarchy_spec()` e `questionnaire_alternative_spec()`:

**Objetivo → 4 critérios → 10 subcritérios → 10 alternativas → P7/rastreabilidade**

Isso evita divergência entre a árvore mostrada ao usuário e a estrutura realmente usada no cálculo.

## Evidência

Marcadores fictícios/sintéticos no texto geram apenas um aviso; a classe gravada é sempre a escolhida pelo usuário.

- `SYNTHETIC`: somente sintético;
- `EMPIRICAL`: exclui sintético;
- `HYBRID`: admite ambos preservando `data_source`.

A execução multicritério registra o `evidence_mode` usado.

## AHP

P1–P5 alimentam o AHP.

O aplicativo:
- reconstrói as matrizes;
- calcula pesos;
- calcula RC;
- permite restringir a agregação às matrizes completas com `RC < 0,10`;
- agrega julgamentos válidos por média geométrica.

## TOPSIS

P6 alimenta o TOPSIS.

`N/A` não é imputado.

O aplicativo apresenta cobertura (`n_valid`) antes do ranking para permitir avaliar desigualdade de participação entre sistema × subcritério.

## P7

P7 serve para rastrear:
- peça/componente;
- PN/NSN quando conhecido;
- ocorrência/necessidade;
- OS, requisição, estoque, relatório, TM ou outra fonte de confirmação.

P7 não altera peso AHP nem nota TOPSIS.

## Limite metodológico

AHP/TOPSIS prioriza investigação e decisão multicritério.

As respostas não são convertidas automaticamente em:
- MTBF;
- MTTR;
- ALDT;
- disponibilidade;
- lead time;
- custo;
- probabilidade de exigir peça;
- parâmetros de simulação.

Esses valores dependem de registros rastreáveis ou de hipóteses explicitamente identificadas.
## Classificação das respostas importadas (atualização v1.5.7)

Quem decide se as respostas são EMPIRICAL ou SYNTHETIC é o usuário. Ao importar um arquivo, a opção padrão é "Seguir o modo de evidência escolhido em Governança dos dados": no modo EMPIRICAL as respostas entram como EMPIRICAL; no modo SYNTHETIC, como SYNTHETIC; no modo HYBRID, como EMPIRICAL. É possível forçar uma das duas classes na própria lista. O aplicativo não troca a classe por conta própria; termos como "fictício" no texto geram apenas um aviso.

Se houver respostas gravadas em uma classe que o modo atual esconde (por exemplo, SYNTHETIC com o modo EMPIRICAL), aparece um aviso abaixo de "Aplicar modo" com o botão "Reclassificar respostas como ...". A demonstração (SYNTHETIC_DEMO) nunca é reclassificada.
