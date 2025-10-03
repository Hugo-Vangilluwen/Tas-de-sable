#include "grid.h"
#include <stdlib.h>

struct grid {
    int** grid;
    int largeur;
    int hauteur;
};

struct coordinates {
    int x;
    int y;
};

grid_t* create(coordinates_t* c) {
    grid_t* g = malloc(sizeof(grid_t));
    g->grid = malloc(c->x * sizeof(int*));
    g->largeur = c->x;
    g->hauteur = c->y;

    for (int i = 0; i < c->x; ++i) {
        g->grid[i] = malloc(c->y * sizeof(int));
        for (int j = 0; j < c->y; ++j) {
            g->grid[i][j] = 0;
        }
    }
}

int valeur(grid_t* g, coordinates_t* c) {
    return g->grid[c->x][c->y];
}

coordinates_t* neighbours(grid_t* g, coordinates_t* c, int* n) {
    *n = 2
        + (0 < c->x && c->x < g->largeur - 1)
        + (0 < c->y && c->y < g->hauteur - 1);
    coordinates_t* neighbourhood = malloc(*n * sizeof(coordinates_t));
    int i = 0;

    if (0 < c->x) {
        neighbourhood[i].x = c->x - 1;
        neighbourhood[i].y = c->y;
        ++i;
    }
    if (c->x < g->largeur - 1) {
        neighbourhood[i].x = c->x + 1;
        neighbourhood[i].y = c->y;
        ++i;
    }
    if (0 < c->y) {
        neighbourhood[i].x = c->x;
        neighbourhood[i].y = c->y - 1;
        ++i;
    }
    if (c->y < g->hauteur - 1) {
        neighbourhood[i].x = c->x;
        neighbourhood[i].y = c->y + 1;
        ++i;
    }

    return neighbourhood;
}
