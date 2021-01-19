// Bayesian Network
//   Elvira format 

bnet  "League of legends" { 

// Network Properties

kindofgraph = "directed";
title = "League of Legends";
author = "Eilder Jorge";
visualprecision = "0.00";
version = 1.0;
default node states = (presente , ausente);

// Variables 

node Clima(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =474;
pos_y =90;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("Lluvia" "Claro");
}

node Humor(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =198;
pos_y =190;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("Bueno" "Malo");
}

node Amigos(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =468;
pos_y =219;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("Disponibles" "No disponibles");
}

node Estilo_de_juego(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =196;
pos_y =334;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("Serio" "Casual");
}

node Ranking(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =748;
pos_y =191;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("Alto" "Bajo");
}

node Fortaleza(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =718;
pos_y =346;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("Fuerte" "Debil");
}

node Equipo(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =480;
pos_y =312;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("Fuerte" "Debil");
}

node Resultado(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =472;
pos_y =397;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("Victoria" "Derrota");
}

// Links of the associated graph:

link Amigos Equipo;

link Clima Amigos;

link Clima Fortaleza;

link Clima Humor;

link Clima Ranking;

link Equipo Resultado;

link Estilo_de_juego Resultado;

link Fortaleza Resultado;

link Humor Amigos;

link Humor Estilo_de_juego;

link Ranking Fortaleza;

//Network Relationships: 

relation Clima { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.18 0.82 );
}

relation Humor Clima { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.65 0.85 0.35 0.15 );
}

relation Ranking Clima { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.2 0.9 0.8 0.1 );
}

relation Fortaleza Clima Ranking { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.8 0.05 0.9 0.3 0.2 0.95 0.1 0.7 );
}

relation Estilo_de_juego Humor { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.85 0.3 0.15 0.7 );
}

relation Equipo Amigos { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.8 0.1 0.2 0.9 );
}

relation Resultado Estilo_de_juego Fortaleza Equipo { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.65 0.3 0.9 0.7 0.4 0.1 0.7 0.5 0.35 0.7 0.1 0.3 0.6 0.9 0.3 0.5 );
}

relation Amigos Clima Humor { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.95 0.25 0.35 0.01 0.05 0.75 0.65 0.99 );
}

}
