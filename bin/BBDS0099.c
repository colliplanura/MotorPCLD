 /*
  * BBDS0099 - Combinação das reduções possíveis entre os grupos de
  *            operações para obtenção do melhor índice de reversão
  * Autor: Sandro Fernandes Colli da Silva
  * Data da criação: 23/11/2016
  */

 #include <stdio.h>
 #include <stdlib.h>
 #include <string.h>
 #include <time.h>
 #include <float.h>
 /* #include <decimal.h> */

 /*
  * PIC S9(4) COMP-5   =  signed short
  * PIC S9(9) COMP-5   =  signed int
  * PIC S9(15)V99 COMP-3 = decimal(17,2) include <decimal.h>
  */

 #define MAX_GRUPOS 10
 #define MAX_FAIXAS 8

 int combinacoes = 0;

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

 unsigned int aleatorioEntre(int menor, int maior) {
 	static unsigned char executado = 0;

 	if (!executado) {
 		srand((unsigned) time(NULL));
 		executado = 1;
 	}
 	return (menor + rand() % (maior - menor + 1));
 }

 erro validaEntrada(rqsc requisicao) {
 	int g, f;
 	erro retErro;

 	retErro.seqlErro = 0;
 	sprintf(retErro.txErro, " ");

 	if ((requisicao.qtGrupos == 0) ||
 		(requisicao.qtGrupos > MAX_GRUPOS)) {
 		retErro.seqlErro = 1;
 		sprintf(retErro.txErro, "Quantidade de grupos não está entre"
 				" 1 e %i.",
 				MAX_GRUPOS);
 		return retErro;
 	}

 	for (g = 0; g < requisicao.qtGrupos; g++) {

 		if ((requisicao.grupos[g].qtFaixas == 0)
 				|| (requisicao.grupos[g].qtFaixas > MAX_FAIXAS)) {
 			retErro.seqlErro = 2;
 			sprintf(retErro.txErro,
 					"Quantidade de faixas não está entre 1 e %i. "
 					"Grupo: %i.",
 					MAX_FAIXAS, g);
 			return retErro;
 		}

 		for (f = 0; f < requisicao.grupos[g].qtFaixas; f++) {

 			if (requisicao.grupos[g].faixas[f].fxa == 0) {
 				retErro.seqlErro = 3;
 				sprintf(retErro.txErro,
 						"Número da faixa não informado. Grupo: %i. "
 						"Faixa: %i.",
 						g, f);
 				return retErro;
 			}

 			if (requisicao.grupos[g].faixas[f].amtr <= 0) {
 				retErro.seqlErro = 4;
 				sprintf(retErro.txErro,
 						"Valor da amortização não informado. Grupo:"
 						" %i. Faixa: %i.",
 						g, f);
 				return retErro;
 			}

 			if (requisicao.grupos[g].faixas[f].pcld <= 0) {
 				retErro.seqlErro = 5;
 				sprintf(retErro.txErro,
 						"Valor da reversão da PCLD não informado. "
 						"Grupo: %i. Faixa: %i.",
 						g, f);
 				return retErro;
 			}

 			if (requisicao.grupos[g].faixas[f].pcld
 					< requisicao.grupos[g].faixas[f].amtr) {
 				retErro.seqlErro = 6;
 				sprintf(retErro.txErro,
 						"Dados inconsistentes. Valor da reversão da "
 						"PCLD menor que amortização. Grupo: %i. "
 						"Faixa: %i.",
 						g, f);
 				return retErro;
 			}

 			if (f > 0) {
 				if (requisicao.grupos[g].faixas[f].pcld
 						< requisicao.grupos[g].faixas[f - 1].pcld) {
 					retErro.seqlErro = 7;
 					sprintf(retErro.txErro,
 							"Dados inconsistentes. Valor da reversão"
 							" da PCLD não é crescente por faixas. "
 							"Grupo: %i. Faixa Atual: %i. Faixa "
 							"anterior: %i",
 							g, f, f - 1);
 					return retErro;
 				}

 				if (requisicao.grupos[g].faixas[f].amtr
 						< requisicao.grupos[g].faixas[f - 1].amtr) {
 					retErro.seqlErro = 8;
 					sprintf(retErro.txErro,
 							"Dados inconsistentes. Valor da "
 							"amortização não é crescente por faixas."
 							" Grupo: %i. Faixa Atual: %i. Faixa "
 							"anterior: %i",
 							g, f, f - 1);
 					return retErro;
 				}
 			}
 		}
 	}
 }

 double menorAmtr(rqsc *requisicao, rstd *result1) {
 	int g, f;
 	double menorAmtr = DBL_MAX;

 	for (g = 0; g < (*requisicao).qtGrupos; g++) {
 		if ((*requisicao).grupos[g].faixas[0].amtr < menorAmtr) {
 			(*result1).qtGrFxa = 1;
 			(*result1).grFxa[0].gr = (*requisicao).grupos[g].gr;
 			(*result1).grFxa[0].fxa =
 					(*requisicao).grupos[g].faixas[0].fxa;
 			menorAmtr = (*requisicao).grupos[g].faixas[0].amtr;
 		}
 	}

 	return menorAmtr;
 }

 double maiorAmtr(rqsc *requisicao, rstd *result2) {
 	int g, f;
 	double maiorAmtr = 0;

 	(*result2).qtGrFxa = 0;

 	for (g = 0; g < (*requisicao).qtGrupos; g++) {
 		maiorAmtr +=
 		(*requisicao).grupos[g].faixas[(*requisicao).
 		                        grupos[g].qtFaixas - 1].amtr;
 		(*result2).grFxa[(*result2).qtGrFxa].gr =
 				(*requisicao).grupos[g].gr;
 		(*result2).grFxa[(*result2).qtGrFxa].fxa =
 				(*requisicao).grupos[g].faixas[(*requisicao).
 				grupos[g].qtFaixas - 1].fxa;
 		(*result2).qtGrFxa++;
 	}

 	return maiorAmtr;
 }

 void combina(rqsc *requisicao, rpst *resposta, int gi,
 		double acmAmtr, double acmPcld, double mediaAmtr,
 		rstd resultAtu) {
 	int fi;
 	static float melhorIndice = 1;
 	static float melhorIndice10 = 1;
 	static double maiorPcld = 0;
 	static double maiorPcld80 = 0;
 	static double maiorPcld90 = 0;
 	static double maiorPcld110 = 0;
 	static double maiorPcld120 = 0;

 	for (gi; gi < (*requisicao).qtGrupos; gi++) {
 		for (fi = 0; fi < (*requisicao).grupos[gi].qtFaixas; fi++) {

 			combinacoes++;

 			acmAmtr += (*requisicao).grupos[gi].faixas[fi].amtr;
 			acmPcld += (*requisicao).grupos[gi].faixas[fi].pcld;

 			resultAtu.grFxa[resultAtu.qtGrFxa].gr =
 					(*requisicao).grupos[gi].gr;
 			resultAtu.grFxa[resultAtu.qtGrFxa].fxa =
 					(*requisicao).grupos[gi].faixas[fi].fxa;
 			resultAtu.qtGrFxa++;

 			/*
 			 * Resultado 3 - Melhor Índice
 			 */
 			if ((acmAmtr / acmPcld) < melhorIndice) {
 				melhorIndice = (acmAmtr / acmPcld);
 				(*resposta).result[3] = resultAtu;
 			}

 			/*
 			 * Resultado 4 - Melhor Índice entre -10% e +10% da média
 			 *  da amortizacao
 			 */
 			if ((acmAmtr > (mediaAmtr * 0.9))
 					&& (acmAmtr < (mediaAmtr * 1.1))) {
 				if ((acmAmtr / acmPcld) < melhorIndice10) {
 					melhorIndice10 = (acmAmtr / acmPcld);
 					(*resposta).result[4] = resultAtu;
 				}
 			}

 			/*
 			 * Resultado 5 - Melhor reversão da PCLD com a entrada
 			 */
 			if ((*requisicao).vlEntrada > 0) {
 				if ((*requisicao).vlEntrada <= acmAmtr) {
 					if (acmPcld > maiorPcld) {
 						maiorPcld = acmPcld;
 						(*resposta).result[5] = resultAtu;
 					}
 				}
 			}

 			/*
 			 * Resultado 6 - Melhor reversão da PCLD com a 80% da
 			 *  entrada
 			 */
 			if ((*requisicao).vlEntrada > 0) {
 				if (0.8 * (*requisicao).vlEntrada <= acmAmtr) {
 					if (acmPcld > maiorPcld80) {
 						maiorPcld80 = acmPcld;
 						(*resposta).result[6] = resultAtu;
 					}
 				}
 			}

 			/*
 			 * Resultado 7 - Melhor reversão da PCLD com a 90% da
 			 *  entrada
 			 */
 			if ((*requisicao).vlEntrada > 0) {
 				if (0.9 * (*requisicao).vlEntrada <= acmAmtr) {
 					if (acmPcld > maiorPcld90) {
 						maiorPcld90 = acmPcld;
 						(*resposta).result[7] = resultAtu;
 					}
 				}
 			}

 			/*
 			 * Resultado 8 - Melhor reversão da PCLD com a 110% da
 			 *  entrada
 			 */
 			if ((*requisicao).vlEntrada > 0) {
 				if (1.1 * (*requisicao).vlEntrada <= acmAmtr) {
 					if (acmPcld > maiorPcld110) {
 						maiorPcld110 = acmPcld;
 						(*resposta).result[8] = resultAtu;
 					}
 				}
 			}

 			/*
 			 * Resultado 9 - Melhor reversão da PCLD com a 120% da
 			 *  entrada
 			 */
 			if ((*requisicao).vlEntrada > 0) {
 				if (1.2 * (*requisicao).vlEntrada <= acmAmtr) {
 					if (acmPcld > maiorPcld120) {
 						maiorPcld120 = acmPcld;
 						(*resposta).result[9] = resultAtu;
 					}
 				}
 			}

 			/*
 			 * Chamada recursiva
 			 */
 			if (gi < ((*requisicao).qtGrupos - 1)) {
 				combina(requisicao, resposta, gi + 1, acmAmtr,
 						acmPcld, mediaAmtr, resultAtu);
 			}
 		}
 	}
 	return;
 }

 int main(parm *parametro) {
 	double start, stop, elapsed;
 	time_t timerini, timerfim;
 	rstd result;
 	double vlMenorAmtr, vlMaiorAmtr;

 	timerini = time(NULL);

 	if (validaEntrada((*parametro).requisicao).seqlErro != 0)
 		return 1;

 	(*parametro).resposta.qtRstd = 1;
 	vlMenorAmtr = menorAmtr(&(*parametro).requisicao,
 			&(*parametro).resposta.result[1]);

 	(*parametro).resposta.qtRstd = 2;
 	vlMaiorAmtr = maiorAmtr(&(*parametro).requisicao,
 			&(*parametro).resposta.result[2]);

 	if ((*parametro).requisicao.vlEntrada == 0)
 		(*parametro).resposta.qtRstd = 4;
 	else
 		(*parametro).resposta.qtRstd = 9;

 	result.qtGrFxa = 0;
 	combina(&(*parametro).requisicao, &(*parametro).resposta, 0, 0,
 			0, (vlMenorAmtr + vlMenorAmtr) / 2, result);

 	printf("Início: %s\n", ctime(&timerini));
 	printf("Número de combinações: %i\n", combinacoes);
 	timerfim = time(NULL);
 	printf("Fim...: %s\n", ctime(&timerfim));

 	return 0;
 }

