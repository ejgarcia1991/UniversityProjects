// Bayesian Network
//   Elvira format 

bnet  "Untitled1" { 

// Network Properties

kindofgraph = "directed";
visualprecision = "0.00";
version = 1.0;
default node states = (presente , ausente);

// Variables 

node Enfermedad(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =579;
pos_y =184;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("presente" "ausente");
}

node Test_2(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =302;
pos_y =259;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("positivo" "negativo");
}

node Test_1(finite-states) {
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =794;
pos_y =321;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("positivo" "negativo");
}

// Links of the associated graph:

link Enfermedad Test_1;

link Enfermedad Test_2;

//Network Relationships: 

relation Enfermedad { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.01 0.99 );
}

relation Test_1 Enfermedad { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.9 0.15 0.1 0.85 );
}

relation Test_2 Enfermedad { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.85 0.05 0.15 0.95 );
}

}
