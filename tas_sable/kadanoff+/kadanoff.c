#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <time.h>

typedef struct taskadanoff_hauteur {
    int n;
    int p;
    int* tab;
} tas_kh;

typedef struct taskadanoff_pente {
    int n;
    int p;
    int* tab;
} tas_kp;

int min(int a, int b){
    if(a < b){
        return a;
    }
    return b;
}

tas_kp* kh_to_kp (tas_kh* t){
    tas_kp* tp = malloc(sizeof(tas_kp));
    tp->n = t-> n;
    tp->p = t-> p;
    int* tab = malloc(tp->n* sizeof(int));
    for(int i=0; i<tp->n-1; i++){
        tab[i] = t->tab[i] - t->tab[i+1];
    }
    tab[tp->n-1] = t->tab[tp->n-1];
    tp->tab = tab;
    return tp;
}

tas_kh* kp_to_kh (tas_kp* t){
    tas_kh* th = malloc(sizeof(tas_kh));
    th->n = t-> n;
    th->p = t-> p;
    int* tab = malloc(th->n* sizeof(int));
    tab[th->n-1] = t->tab[th->n-1];
    for(int i=th->n-2; i>=0; i--){
        tab[i] = tab[i+1] + t->tab[i];
    }
    th->tab = tab;
    return th;
}

tas_kp* creer_tas(int n, int p){
    tas_kp* t = malloc(sizeof(tas_kp));
    t->n = n;
    t->p = p;
    int* tab = malloc(n*sizeof(int));
    for (int i=0; i<n; i++){
        tab[i] =0;
    }
    t->tab = tab;
    return t;
}

tas_kp* copie (tas_kp* t){
    tas_kp* copie = malloc(sizeof(tas_kp));
    copie->n = t-> n;
    copie->p = t-> p;
    int* tab = malloc(copie->n* sizeof(int));
    for(int i=0; i<copie->n-1; i++){
        tab[i] = t->tab[i];
    }
    tab[copie->n-1] = t->tab[copie->n-1];
    copie->tab = tab;
    return copie;
}

void ajout_grain(tas_kp* t){
    t->tab[0]++;
}

void ajouter_m_grains(tas_kp* t, int m){
    for (int k=0; k<m; k++){
        ajout_grain (t);
    }
}

void eboulement (tas_kp* t, int i){
    assert(i>=0 && i<t->n);
    assert(t->tab[i]>t->p);
    t->tab[i] = t->tab[i] - (t->p+1);
    t->tab[min(i+t->p,t->n-1)] = t->tab[min(i+t->p,t->n-1)] +1;
    if (i>0){
        t->tab[i-1] = t->tab[i-1] + t->p;
    }
}

void stabilisation(tas_kp* t){
    bool stab = false;
    while (!stab){
        stab = true;
        for(int i=0; i<t->n; i++){
            if (t->tab[i] > t->p){
                stab = false;
                eboulement(t, i);
            }
        }   
    }
}

/* additionner 2 tas */
tas_kp* add (tas_kp* t1, tas_kp* t2){
    assert(t1->n == t2->n);
    assert(t1->p == t2->p);
    tas_kp* tf = malloc(sizeof(tas_kp));
    tf->n = t1-> n;
    tf->p = t2-> p;
    int* tab = malloc(tf->n* sizeof(int));
    for (int i=0;i<tf->n; i++){
        tab[i] = t1->tab[i] + t2->tab[i];
    }
    tf->tab= tab;
    return tf;
}

tas_kp* sous (tas_kp* t1, tas_kp* t2){
    assert(t1->n == t2->n);
    assert(t1->p == t2->p);
    tas_kp* tf = malloc(sizeof(tas_kp));
    tf->n = t1-> n;
    tf->p = t2-> p;
    int* tab = malloc(tf->n* sizeof(int));
    for (int i=0;i<tf->n; i++){
        tab[i] = t1->tab[i] - t2->tab[i];
    }
    tf->tab= tab;
    return tf;
}

tas_kp* cmax (int n, int p){
    tas_kp* t = malloc(sizeof(tas_kp));
    t->n = n;
    t->p = p;
    int* tab = malloc(n*sizeof(int));
    for (int i=0; i<n; i++){
        tab[i] = p;
    }
    t->tab = tab;
    return t;
}

tas_kp* identite(int n, int p){
    tas_kp* deuxcmax = add(cmax(n,p),cmax(n,p));
    tas_kp* deuxcmax_s = copie(deuxcmax);
    stabilisation(deuxcmax_s);
    tas_kp* identite = sous(deuxcmax, deuxcmax_s);
    stabilisation(identite);

    return identite;
}

void ecrire_tas(const char* nom_fichier, tas_kp* tp){
    tas_kh* t = kp_to_kh(tp);
    FILE* f = fopen(nom_fichier, "w");

    if (f == NULL){
        printf("Erreur ouverture fichier\n");
        return;
    }

    fprintf(f, "%d\n", t->n);

    for(int i = 0; i < t->n; i++){
        fprintf(f, "%d ", t->tab[i]);
    }

    fprintf(f, "\n");

    fclose(f);
}

void ecrire_tas_pente(const char* nom_fichier, tas_kp* t){
    FILE* f = fopen(nom_fichier, "w");

    if (f == NULL){
        printf("Erreur ouverture fichier\n");
        return;
    }

    fprintf(f, "%d\n", t->n);

    for(int i = 0; i < t->n; i++){
        fprintf(f, "%d ", t->tab[i]);
    }

    fprintf(f, "\n");

    fclose(f);
}


void exporter_frame(char* base, int frame, tas_kp* t){
    char nom[100];
    sprintf(nom, "%s_%d.txt", base, frame);
    ecrire_tas(nom, t);
}

void simulation(char* base, tas_kp* t, int nb_iterations){
    for(int i = 0; i < nb_iterations; i++){

        ajout_grain(t);
        stabilisation(t);

        exporter_frame(base, i, t);
    }
}

void stabilisation_frames(char* base, tas_kp* t){
    exporter_frame(base, 0, t);
    int frame = 1;
    bool stab = false;

    while (!stab){
        stab = true;

        for(int i = 0; i < t->n; i++){

            if(t->tab[i] > t->p){

                stab = false;

                eboulement(t, i);

                exporter_frame(base, frame, t);

                frame++;
            }
        }
    }
}

bool est_zone_vague(tas_kp* t, int i){
    for(int k = 0; k < t->p - 1; k++){

        int diff = t->tab[i + k] - t->tab[i + k + 1];

        // tolérance légère (IMPORTANT)
        if(diff != 1)
            return false;
    }
    return true;
}

typedef struct {
    int debut_vague1;
    int nb_cycles1;

    int position_zero;
    int nb_cycles2;
} analyse_vagues;

analyse_vagues analyser_vagues(tas_kp* t){
    analyse_vagues a;

    a.debut_vague1 = -1;
    a.nb_cycles1 = 0;
    a.position_zero = -1;
    a.nb_cycles2 = 0;

    int i = 0;

    while((i <= t->n - t->p) && (a.debut_vague1 == -1)){
        if(est_zone_vague(t, i)){
            a.debut_vague1 = i;
            a.nb_cycles1++;
            i += t->p;
        }
        else {
            i += 1;
        }
    }

    while (est_zone_vague(t, i)){
        a.nb_cycles1++;
        i+= t->p;
    }

    a.position_zero = i;
    i++;

    while (est_zone_vague(t, i)){
        a.nb_cycles2++;
        i+= t->p;
    }

    return a;
}

/* =========================
   EXPERIENCE TIPE
========================= */

void experience(int p){
    FILE* f = fopen("vagues_N.txt", "w");

    for(int N = 20; N <= 2000; N += 20){

        tas_kp* t = creer_tas(N, p);

        ajouter_m_grains(t, N * 10);
        stabilisation(t);

        analyse_vagues a = analyser_vagues(t);

        fprintf(f, "%d %d %d\n",
                N,
                a.debut_vague1,
                a.nb_cycles1);

        free(t->tab);
        free(t);
    }

    fclose(f);
}

int main(){
    /*clock_t debut, fin;
    double duree;

    tas_kp* t = creer_tas(8, 3);

    debut = clock();
    ajouter_m_grains(t, 25);
    stabilisation_frames("stab",t);
    ecrire_tas("intro_kadanoff.txt", t);
    fin = clock();
    duree = (double)(fin - debut) / CLOCKS_PER_SEC;
    printf("Durée pour une stabilisation de k grains : %f secondes\n", duree);*/

    /*tas_kp* t = identite(25, 3);
    tas_kp* t2 = creer_tas(25,3);
    ajouter_m_grains(t2, 10000);
    stabilisation(t2);

    tas_kp* t3 = add(t, t);
    stabilisation(t3);

    ecrire_tas("id25.txt", t);
    ecrire_tas("tas_ajoutid.txt", t3);*/

    /*tas_kp* t = creer_tas(25,3);
    simulation("frame", t, 100);*/
    
    /*tas_kp* did = add(identite(4,3), identite(4,3));
    stabilisation(did);
    ecrire_tas("id.txt", identite(4,3));
    ecrire_tas("deux_id.txt", did);*/

    int p = 3;

    tas_kp* t = creer_tas(100, p);

    ajouter_m_grains(t, 3150);
    stabilisation(t);
    ecrire_tas_pente("test_vagues_300.txt", t);

    analyse_vagues a = analyser_vagues(t);

    printf("Debut vague 1 : %d\n", a.debut_vague1);
    printf("Cycles vague 1 : %d\n", a.nb_cycles1);
    printf("Zero : %d\n", a.position_zero);
    printf("Cycles vague 2 : %d\n", a.nb_cycles2);

    experience(4);

    return 0;
}