/*
 * COMBINA - Combinação das reduções possíveis entre os grupos de operações
 *           para obtenção do melhor índice de reversão
 * Autor: Sandro Fernandes Colli da Silva
 * Data da criação: 23/11/2016
 */

#include <string.h>
#include <stdio.h>

typedef struct {
    int gr, fxa;
	float amtr, pcld;
} faixa;

typedef struct {
    int qtFaixas;
    faixa faixas[8];
} grupo;

typedef struct {
    int qtGrupos;
    grupo grupos[10];
} reducao;

reducao reducoes;

typedef struct {
    char result1[39];     // menor valor para amortização
    char result2[39];     // maior valor para amortização
    char result3[39];     // melhor índice de reversão
    char result4[39];     // melhor índice de reversão na faixa média +/- 10% da média
    char result5[39];     // maior reversão a partir de um valor de entrada
    char result6[39];     // maior reversão a partir com -20% de entrada
    char result7[39];     // maior reversão a partir com -10% de entrada
    char result8[39];     // maior reversão a partir com +10% de entrada
    char result9[39];     // maior reversão a partir com +20% de entrada
} resultado;

resultado resultados;

resultado combina(int gi, int *grupos, int faixas, float acmAmtr, float acmPcld,
		float *melhorIndice, char resultado[]) {
	char result[39] = "";
	int fi;

	for (gi; gi < *grupos; gi++) {
		for (fi = 0; fi < faixas; fi++) {
			sprintf(result, "%s%02i%i+", resultado, gi + 1, fi + 1);
			acmAmtr += reducoes.grupos[gi].faixas[fi].amtr;
			acmPcld += reducoes.grupos[gi].faixas[fi].pcld;

			if ((acmAmtr / acmPcld) < *melhorIndice) {
				*melhorIndice = (acmAmtr / acmPcld);
				strcpy(resultados.result3, result);
			}

			if (gi < (*grupos - 1)) {
				combina(gi + 1, grupos, reducoes.grupos[gi + 1].qtFaixas, acmAmtr, acmPcld, melhorIndice,
						result);
			}
		}
	}
	return;
}
