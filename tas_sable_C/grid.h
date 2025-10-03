#ifndef GRID_H
#define GRID_H

typedef struct grid grid_t;
typedef struct coordinates coordinates_t;


/* Crée une grille de dimension c */
grid_t* create(coordinates_t* c);

/* Retourne la valeur de la case de coordonnées c */
int valeur(grid_t* g, coordinates_t* c);

/* Retourne le tableau des voisins de la case de coordonnées c
 * écrit dans n le nombre de voisins
 */
coordinates_t* neighbours(grid_t* g, coordinates_t* c, int* n);

#endif /* GRID_H */
