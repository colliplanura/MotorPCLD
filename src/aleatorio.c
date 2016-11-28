#include <time.h>
#include "aleatorio.h"

aleatorioEntre(int menor, int maior) {
    
    if (!executado) {
        srand((unsigned) time(NULL));
        executado = 1;
    }
	return (menor + rand() % (maior - menor + 1));
}