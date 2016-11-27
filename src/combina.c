/*
 * COMBINA - Combinação das reduções possíveis entre os grupos de operações
 *           para obtenção do melhor índice de reversão
 * Autor: Sandro Fernandes Colli da Silva
 * Data da criação: 23/11/2016
 */

#include<string.h>

typedef struct {
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
char melhorResult[39];

void combina(int gi, int *grupos, int faixas, float acmAmtr, float acmPcld,
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
				strcpy(melhorResult, result);
			}

			if (gi < (*grupos - 1)) {
				combina(gi + 1, grupos, reducoes.grupos[gi + 1].qtFaixas, acmAmtr, acmPcld, melhorIndice,
						result);
			}
		}
	}
	return;
}