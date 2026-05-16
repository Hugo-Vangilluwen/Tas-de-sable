#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef struct tas{
    int haut;
    int gauche;
    int droite;
    int tab[2][3][3];
}  tas;

tas* creer_tas(){
    tas* t = malloc(sizeof(tas));

    t->haut = 0;
    t->gauche = 0;
    t->droite = 0;

    for (int k = 0; k < 2; k++)
        for (int i = 0; i < 3; i++)
            for (int j = 0; j < 3; j++)
                t->tab[k][i][j] = 0;

    return t;
}

void stabilise(tas* t){
    while (t->haut > 1 || t->gauche > 2 || t->droite > 2){

        if (t->haut > 1){
            t->haut -= 2;
            t->gauche++;
            t->droite++;
        }

        if (t->gauche > 2){
            t->gauche -= 3;
            t->haut++;
            t->droite++;
        }

        if (t->droite > 2){
            t->droite -= 3;
            t->haut++;
            t->gauche++;
        }
    }
}

void ajoute_tab(tas* t){
    t->tab[t->haut][t->gauche][t->droite]++;
}

void add_random(tas* t){
    int i = rand() % 3;

    if (i == 0) t->haut++;
    else if (i == 1) t->gauche++;
    else t->droite++;

    stabilise(t);
    ajoute_tab(t);
}

int main(){
    srand(time(NULL));

    tas** tableaux = malloc(10 * sizeof(tas*));

    for (int i = 0; i < 10; i++){
        tableaux[i] = creer_tas();

        for (int j = 0; j < 100; j++){
            add_random(tableaux[i]);
        }
    }

    for (int k = 0; k < 2; k++){
        for (int i = 0; i < 3; i++){
            for (int j = 0; j < 3; j++){

                printf("(%d,%d,%d)  ", k, i, j);

                for (int t = 0; t < 10; t++){
                    printf("%3d ", tableaux[t]->tab[k][i][j]);
                }

                printf("\n");
            }
        }
    }

    for (int i = 0; i < 10; i++){
        free(tableaux[i]);
    }
    free(tableaux);

    return 0;
}