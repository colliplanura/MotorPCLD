/*
 * ARCKC628 - Book do parametro da ARCSC628 - Motor de Combinações
 * Autor: Sandro Fernandes Colli da Silva
 * Data: 29/11/2016
*/

typedef struct {
 	signed int seqlErro;
 	char txErro[120];
 } erro;

 typedef struct {
 	signed short fxa;
 	double amtr, pcld;
 } faixa;

 typedef struct {
 	signed short gr;
 	signed short qtFaixas;
 	faixa faixas[8];
 } grupo;

 typedef struct {
 	double vlEntrada;
 	signed short qtGrupos;
 	grupo grupos[10];
 } rqsc;

 typedef struct {
 	signed short gr;
 	signed short fxa;
 } grupoFaixa;

 typedef struct {
 	signed short qtGrFxa;
 	grupoFaixa grFxa[10];
 } rstd;

 typedef struct {
 	signed short qtRstd;
 	rstd result[9];
 } rpst;

 typedef struct {
 	erro retErro;
 	rqsc requisicao;
 	rpst resposta;
 } parm;
