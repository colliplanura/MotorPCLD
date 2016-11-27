#include <time.h>

bool executado = false;
enor, int maior) {
    
    if (not executado) {
        srand((unsigned) time(NULL));
        executado = true;
    }
	return (menor + rand() % (maior - menor + 1));
}