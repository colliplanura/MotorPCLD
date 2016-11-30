/*
 * RODA - Executa o subprograma BBDS0099 - Motor de combinações
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "ARCKC628.h"

int BBDS0099(parm *parametro);

void displayInput(rqsc *requisicao, erro *prmErro);

unsigned int aleatorioEntre(int menor, int maior) {
	static unsigned char executado = 0;

	if (!executado) {
		srand((unsigned) time(NULL));
		executado = 1;
	}
	return (menor + rand() % (maior - menor + 1));
}


int main(int argc, char *argv[]) {

	int maxGr, maxFx, g, f;
	double  maxPcld;
	parm parametro;

	maxGr = atoi(argv[1]);
	maxFx = atoi(argv[2]);

	parametro.requisicao.vlEntrada = atoi(argv[3]);
	parametro.requisicao.qtGrupos = aleatorioEntre(1, maxGr);

	maxPcld = atoi(argv[4]);

	printf("\n\nGrupos: %i. Faixas: %i, Entrada: %9.2f. Máximo PCLD: %9.2f\n\n", maxGr, maxFx, parametro.requisicao.vlEntrada, maxPcld);

	for (g = 0; g < parametro.requisicao.qtGrupos; g++) {

		parametro.requisicao.grupos[g].qtFaixas = aleatorioEntre(1, maxFx);
		parametro.requisicao.grupos[g].gr = g + 1;

		for (f = 0; f < parametro.requisicao.grupos[g].qtFaixas; f++) {

		    parametro.requisicao.grupos[g].faixas[f].fxa = f + 1;

			if (f == 0) {
				parametro.requisicao.grupos[g].faixas[f].pcld = aleatorioEntre(1, (int)maxPcld);
				parametro.requisicao.grupos[g].faixas[f].amtr = aleatorioEntre(1, (int)parametro.requisicao.grupos[g].faixas[f].pcld);
			}
			else {
				parametro.requisicao.grupos[g].faixas[f].pcld = aleatorioEntre((int)parametro.requisicao.grupos[g].faixas[f - 1].pcld, (int)maxPcld);
				parametro.requisicao.grupos[g].faixas[f].amtr = aleatorioEntre((int)parametro.requisicao.grupos[g].faixas[f - 1].amtr, (int)parametro.requisicao.grupos[g].faixas[f].pcld);
			}
		}
	}

	printf("\n\nDisplay do RODA");
//	displayInput(&parametro.requisicao, &parametro.retErro);

	printf("\n\nDisplay do BBDS0099");
	BBDS0099(&parametro);

	if (parametro.retErro.seqlErro != 0)
		printf("Retorno BBDS0099: %s", parametro.retErro.txErro);

	return 0;
}
