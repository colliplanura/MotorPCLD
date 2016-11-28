/*
 * COMBINA - Combinação das reduções possíveis entre os grupos de operações
 *           para obtenção do melhor índice de reversão
 * Autor: Sandro Fernandes Colli da Silva
 * Data da criação: 23/11/2016
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

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

unsigned int aleatorioEntre(int menor, int maior) {
	static unsigned char executado = 0;

    if (!executado) {
        srand((unsigned) time(NULL));
        executado = 1;
    }
	return (menor + rand() % (maior - menor + 1));
}


void combina(int gi, int *grupos, int faixas, reducao *reducoes, float acmAmtr, float acmPcld,
		float *melhorIndice, char resultTemp[], resultado *resultados) {
	int fi;

	for (gi; gi < *grupos; gi++) {
		for (fi = 0; fi < faixas; fi++) {
			sprintf(resultTemp, "%s%02i%i+", resultTemp, gi + 1, fi + 1);
			acmAmtr += (*reducoes).grupos[gi].faixas[fi].amtr;
			acmPcld += (*reducoes).grupos[gi].faixas[fi].pcld;

			if ((acmAmtr / acmPcld) < *melhorIndice) {
				*melhorIndice = (acmAmtr / acmPcld);
				strcpy((*resultados).result3, resultTemp);
			}

			if (gi < (*grupos - 1)) {
				combina(gi + 1, grupos, (*reducoes).grupos[gi + 1].qtFaixas, acmAmtr, acmPcld, melhorIndice,
						resultTemp, resultados);
			}
		}
	}
	return;
}

int main(int argc, char *argv[]) {
	int grupos, maxFaixas, g, f, amtrIni, pcldIni, pcldFim;
	char result[39] = "";
	double start, stop, elapsed;
	time_t timerini, timerfim;
	float melhorIndice = 1;
	reducao reducoes;
	resultado resultados;

	timerini = time(NULL);
	grupos = atoi(argv[1]);
	maxFaixas = atoi(argv[2]);
	pcldFim = 1000;
	pcldIni = pcldFim * (30 / 100);
	amtrIni = pcldIni * (10 / 100);

	printf("Grupos: %i | Máximo de Faixas: %i | Melhor Indice: %f\n", grupos,
			maxFaixas, melhorIndice);

	for (g = 0; g < grupos; g++) {

		reducoes.grupos[g].qtFaixas = aleatorioEntre(1, maxFaixas);

		for (f = 0; f < reducoes.grupos[g].qtFaixas; f++) {
			if (f == 0) {
				reducoes.grupos[g].faixas[f].pcld = aleatorioEntre(pcldIni,
						pcldFim);
				reducoes.grupos[g].faixas[f].amtr = aleatorioEntre(amtrIni,
						reducoes.grupos[g].faixas[f].pcld);
			} else {
				reducoes.grupos[g].faixas[f].pcld = aleatorioEntre(
						(int) reducoes.grupos[g].faixas[f - 1].pcld, pcldFim);
				reducoes.grupos[g].faixas[f].amtr = aleatorioEntre(
						(int) reducoes.grupos[g].faixas[f - 1].amtr,
						(int) reducoes.grupos[g].faixas[f].pcld);
			}
			printf("%5i%1i: %5.f %5.f", g + 1, f + 1,
					reducoes.grupos[g].faixas[f].amr,
					reducoes.grupos[g].faixas[f].pcld);
		}
		printf("\n");
	}

	printf("\n\n");

	combina(0, &grupos, reducoes.grupos[0].qtFaixas, &reducoes, 0, 0, &melhorIndice,
			"", &resultados);

	printf("Melhor Indice: %f | Melhor Resultado: %s\n\n", melhorIndice,
			resultados.result3);
	printf("Início: %s\n", ctime(&timerini));
	timerfim = time(NULL);
	printf("Fim...: %s\n", ctime(&timerfim));

	return 0;
}

