/*
 * COMBINA - Combinação das reduções possíveis entre os grupos de operações
 *           para obtenção do melhor índice de reversão
 * Autor: Sandro Fernandes Colli da Silva
 * Data da criação: 23/11/2016
 */


#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<time.h>

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

int aleatorioEntre(int menor, int maior) {

	int random, aleatorio;

	random = rand();

	aleatorio = menor + rand() % (maior - menor + 1);

	return aleatorio;
}

void combina(int gi, int *grupos, int *faixas, float acmAmtr, float acmPcld,
		float *melhorIndice, char resultado[]) {
	char result[39] = "";
	int fi;

	for (gi; gi < *grupos; gi++) {
		for (fi = 0; fi < *faixas; fi++) {
			sprintf(result, "%s%02i%i+", resultado, gi + 1, fi + 1);
			acmAmtr += reducoes[gi][fi].amtr;
			acmPcld += reducoes[gi][fi].pcld;

			if ((acmAmtr / acmPcld) < *melhorIndice) {
				*melhorIndice = (acmAmtr / acmPcld);
				strcpy(melhorResult, result);
			}

			if (gi < (*grupos - 1)) {
				combina(gi + 1, grupos, faixas, acmAmtr, acmPcld, melhorIndice,
						result);
			}
		}
	}
	return;
}

int main(int argc, char *argv[]) {
	int grupos, maxFaixas, faixas, g, f, amtrIni, pcldIni, pcldFim;
	char result[39] = "";
	double start, stop, elapsed;
	time_t timerini, timerfim;
	float melhorIndice = 1;

	timerini = time(NULL);
	grupos = atoi(argv[1]);
	maxFaixas = atoi(argv[2]);
	pcldFim = 1000;
	pcldIni = pcldFim * (30 / 100);
	amtrIni = pcldIni * (10 / 100);

	printf("Grupos: %i | Máximo de Faixas: %i | Melhor Indice: %f\n", grupos, maxFaixas,
			melhorIndice);

	srand((unsigned) time(NULL));

	for (g = 0; g < grupos; g++) {
	    
	    faixas = aleatorioEntre(1, maxFaixas);
	    
		for (f = 0; f < faixas; f++) {
			if (f == 0) {
				reducoes[g][f].pcld = aleatorioEntre(pcldIni, pcldFim);
				reducoes[g][f].amtr = aleatorioEntre(amtrIni,
						reducoes[g][f].pcld);
			} else {
				reducoes[g][f].pcld = aleatorioEntre(reducoes[g][f - 1].pcld,
						pcldFim);
				reducoes[g][f].amtr = aleatorioEntre(reducoes[g][f - 1].amtr,
						reducoes[g][f].pcld);
			}
			printf("%7.f;%5.f", reducoes[g][f].amtr, reducoes[g][f].pcld);
		}
		printf("\n");
	}

	printf("\n");

	for (g = 0; g < grupos; g++) {
		for (f = 0; f < faixas; f++) {
			printf("%4i%i", g + 1, f + 1);
		}
		printf("\n");
	}

	printf("\n");

	combina(0, &grupos, &faixas, 0, 0, &melhorIndice, result);

	printf("Melhor Indice: %f | Melhor Resultado: %s\n\n", melhorIndice,
			melhorResult);
	printf("Início: %s\n", ctime(&timerini));
	timerfim = time(NULL);
	printf("Fim...: %s\n", ctime(&timerfim));

	return 0;
}

