/*
 * testeAleatorio - Teste do programa COMBINA com input aleatória.
 * Autor: Sandro Fernandes Colli da Silva
 * Data da criação: 23/11/2016
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../src/aleatorio.h"
#include "../src/aleatorio.c"
#include "../src/combina.h"
#include "../src/combina.c"

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

int main(int argc, char *argv[]) {
	int grupos, maxFaixas, g, f, amtrIni, pcldIni, pcldFim;
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

	for (g = 0; g < grupos; g++) {
	    
	    reducoes.grupos[g].qtFaixas = aleatorioEntre(1, maxFaixas);
	    
		for (f = 0; f < reducoes.grupos[g].qtFaixas; f++) {
			if (f == 0) {
				reducoes.grupos[g].faixas[f].pcld = aleatorioEntre(pcldIni, pcldFim);
				reducoes.grupos[g].faixas[f].amtr = aleatorioEntre(amtrIni,
						reducoes.grupos[g].faixas[f].pcld);
			} else {
				reducoes.grupos[g].faixas[f].pcld = aleatorioEntre(reducoes.grupos[g].faixas[f - 1].pcld,
						pcldFim);
				reducoes.grupos[g].faixas[f].amtr = aleatorioEntre(reducoes.grupos[g].faixas[f - 1].amtr,
						reducoes.grupos[g].faixas[f].pcld);
			}
			printf("%5i%1i: %5.f %5.f",g +  1, f + 1, reducoes.grupos[g].faixas[f].amtr, reducoes.grupos[g].faixas[f].pcld);
		}
		printf("\n");
	}

	printf("\n\n");

	combina(0, &grupos, reducoes.grupos[0].qtFaixas, 0, 0, &melhorIndice, result);

	printf("Melhor Indice: %f | Melhor Resultado: %s\n\n", melhorIndice,
			melhorResult);
	printf("Início: %s\n", ctime(&timerini));
	timerfim = time(NULL);
	printf("Fim...: %s\n", ctime(&timerfim));

	return 0;
}

