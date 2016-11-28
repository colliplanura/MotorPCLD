all:
	gcc -c src/combina.c -o bin/combina
	gcc -c src/aleatorio.c -o bin/aleatorio
	gcc testes/testeAleatorio.c -o testeAleatorio