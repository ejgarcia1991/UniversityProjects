// Bayesian Network
//   Elvira format 

bnet  "Untitled1" { 

// Network Properties

visualprecision = "0.00";
version = 1.0;
default node states = ("presente" , "ausente");

// Network Variables 

node A(finite-states) {
title = "Age";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =545;
pos_y =28;
relevance = 5.0;
purpose = "Riskfactor";
num-states = 4;
states = ("more70" "60to70" "50to60" "less50");
}

node B(finite-states) {
title = "Sexual activity";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =739;
pos_y =119;
relevance = 7.0;
purpose = "Riskfactor";
num-states = 3;
states = ("mucho" "normal" "poconada");
}

node E(finite-states) {
title = "Prostate Cancer";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =688;
pos_y =368;
relevance = 10.0;
purpose = "Disease";
num-states = 2;
states = ("present" "absent");
}

node F(finite-states) {
title = "Chronic prostatitis";
comment = "Infección gonocócica";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =478;
pos_y =206;
relevance = 5.0;
purpose = "Disease";
num-states = 2;
states = ("yes" "no");
}

node G(finite-states) {
title = "Metastasis";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =478;
pos_y =406;
relevance = 8.0;
purpose = "Sign";
num-states = 2;
states = ("present" "absent");
}

node I(finite-states) {
title = "Cystitis";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =315;
pos_y =327;
relevance = 7.0;
purpose = "Disease";
num-states = 2;
states = ("present" "absente");
}

node K(finite-states) {
title = "Bone scanning";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =553;
pos_y =559;
relevance = 9.0;
purpose = "Test";
num-states = 2;
states = ("positive" "negative");
}

node L(finite-states) {
title = "Rectal examination";
comment = "Tocar nodulo petreo";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =872;
pos_y =516;
relevance = 9.0;
purpose = "Test";
num-states = 3;
states = ("stony" "fibr_irr" "normal");
}

node M(finite-states) {
title = "Pain";
comment = "Dolor en cintura pelviana,  hombros y extremidades  inferiores";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =466;
pos_y =578;
relevance = 4.0;
purpose = "Symptom";
num-states = 2;
states = ("yes" "no");
}

node N(finite-states) {
title = "Anaemia";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =103;
pos_y =528;
relevance = 4.0;
purpose = "Sign";
num-states = 2;
states = ("present" "absent");
}

node O(finite-states) {
title = "Loss of weight";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =315;
pos_y =550;
relevance = 4.0;
purpose = "Sign";
num-states = 2;
states = ("yes" "no");
}

node P(finite-states) {
title = "Hepatic afection";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =140;
pos_y =598;
relevance = 4.0;
purpose = "Sign";
num-states = 2;
states = ("present" "absent");
}

node Q(finite-states) {
title = "Hematuria";
comment = "Hematies en orina";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =187;
pos_y =480;
relevance = 4.0;
purpose = "Sign";
num-states = 2;
states = ("present" "absent");
}

node R(finite-states) {
title = "Ganglion supra";
comment = "Ganglio supraclavicular";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =452;
pos_y =531;
relevance = 4.0;
purpose = "Sign";
num-states = 2;
states = ("present" "absent");
}

node V(finite-states) {
title = "Incomplet drained";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =99;
pos_y =127;
relevance = 3.0;
purpose = "Symptom";
num-states = 2;
states = ("yes" "no");
}

node X(finite-states) {
title = "Ancestor with PC";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =933;
pos_y =246;
relevance = 4.0;
purpose = "Riskfactor";
num-states = 2;
states = ("present" "absent");
}

node Y(finite-states) {
title = "Gleason";
comment = "Grado histologico tumoral. Mide  la agresividad del tumor";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =674;
pos_y =565;
relevance = 9.0;
purpose = "Test";
num-states = 3;
states = ("greater6" "less6" "zero");
}

node J(finite-states) {
title = "BPH";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =260;
pos_y =65;
relevance = 7.0;
purpose = "Disease";
num-states = 2;
states = ("present" "absent");
}

node A1(finite-states) {
title = "Vegetarian";
comment = "Base: soja";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =914;
pos_y =76;
relevance = 7.0;
purpose = "Riskfactor";
num-states = 2;
states = ("yes" "no");
}

node D(finite-states) {
title = "Prostate congestion";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =703;
pos_y =222;
relevance = 7.0;
purpose = "Disease";
num-states = 2;
states = ("present" "absent");
}

node B1(finite-states) {
title = "Obese";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =915;
pos_y =140;
relevance = 7.0;
purpose = "Riskfactor";
num-states = 2;
states = ("yes" "no");
}

node C1(finite-states) {
title = "Hormonal Factors";
comment = "Testosterona. Hormonodependiente";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =907;
pos_y =202;
relevance = 7.0;
purpose = "Riskfactor";
num-states = 2;
states = ("positive" "negative");
}

node S(finite-states) {
title = "IPSS";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =98;
pos_y =479;
relevance = 5.0;
purpose = "Symptom";
num-states = 3;
states = ("greater19" "from8to19" "from0to7");
}

node W(finite-states) {
title = "Flowmetry";
comment = "Mide una cantidad de volumen de orina emitido en un tiempo  determinado.";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =50;
pos_y =391;
relevance = 5.0;
purpose = "Test";
num-states = 2;
states = ("greaterqmax" "lessqmax");
}

node U(finite-states) {
title = "Polaquiuria";
comment = "Aumento de la frecuencia miccional";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =190;
pos_y =396;
relevance = 5.0;
purpose = "Symptom";
num-states = 2;
states = ("yes" "no");
}

node T(finite-states) {
title = "Dysuria";
comment = "Dolor al orinar";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =333;
pos_y =472;
relevance = 5.0;
purpose = "Symptom";
num-states = 2;
states = ("yes" "no");
}

node D1(finite-states) {
title = "Biopsy";
comment = "Biopsia de la prostata";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =881;
pos_y =351;
relevance = 9.0;
purpose = "Test";
num-states = 2;
states = ("positive" "negative");
}

node E1(finite-states) {
title = "CAT";
comment = "Tomografia axial computerizada";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =889;
pos_y =451;
relevance = 7.0;
purpose = "Test";
num-states = 2;
states = ("positive" "negative");
}

node F1(finite-states) {
title = "Constitutional syndrome ";
comment = "Astenia, anorexia, perdida de  peso, febricula o fiebre no filiada.";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =344;
pos_y =592;
relevance = 7.0;
purpose = "Symptom";
num-states = 2;
states = ("present" "absent");
}

node C(finite-states) {
title = "PSA total";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =692;
pos_y =498;
relevance = 7.0;
purpose = "Test";
num-states = 4;
states = ("greater20" "from10to20" "from4to10" "less4");
}

node G1(finite-states) {
title = "PSAl /PSAt";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =785;
pos_y =580;
relevance = 7.0;
purpose = "Test";
num-states = 2;
states = ("less0.1" "greater0.1");
}

node H1(finite-states) {
title = "Asiatic";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =917;
pos_y =18;
relevance = 7.0;
purpose = "Riskfactor";
num-states = 2;
states = ("yes" "no");
}

node I1(finite-states) {
title = "ETS";
comment = "Enfermedades de transmisión sexual: Gonococia";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =335;
pos_y =147;
relevance = 7.0;
purpose = "Riskfactor";
num-states = 2;
states = ("yes" "no");
}

node J1(finite-states) {
title = "Promiscuity";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =548;
pos_y =101;
relevance = 7.0;
purpose = "Riskfactor";
num-states = 2;
states = ("yes" "no");
}

node K1(finite-states) {
title = "Prim UTI";
comment = "Infección urinaria";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =243;
pos_y =176;
relevance = 7.0;
purpose = "Disease";
num-states = 2;
states = ("present" "absent");
}

node L1(finite-states) {
title = "Drilling";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =406;
pos_y =105;
relevance = 7.0;
purpose = "Riskfactor";
num-states = 2;
states = ("yes" "no");
}

node M1(finite-states) {
title = "MedFinas";
comment = "Medicación con finasteride";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =56;
pos_y =183;
relevance = 7.0;
purpose = "Treatment";
num-states = 2;
states = ("yes" "no");
}

node N1(finite-states) {
title = "Microtraumatisms";
comment = "Ciclistas";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =743;
pos_y =35;
relevance = 7.0;
purpose = "Riskfactor";
num-states = 2;
states = ("yes" "no");
}

node O1(finite-states) {
title = "AUR";
comment = "Retención aguda de orina";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =449;
pos_y =59;
relevance = 3.0;
purpose = "Sign";
num-states = 2;
states = ("present" "absent");
}

node P1(finite-states) {
title = "Sec UTI";
comment = "Infección urinaria por gérmenes  acantonados en la prostata";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =280;
pos_y =230;
relevance = 7.0;
purpose = "Disease";
num-states = 2;
states = ("present" "absent");
}

node Q1(finite-states) {
title = "UTI";
comment = "Cultivo de orina";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =106;
pos_y =291;
relevance = 7.0;
purpose = "Aux";
num-states = 2;
states = ("present" "absent");
}

node R1(finite-states) {
title = "Cultivation ";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =52;
pos_y =445;
relevance = 7.0;
purpose = "Test";
num-states = 2;
states = ("positive" "negative");
}

node S1(finite-states) {
title = "Cancer_o_c";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =550;
pos_y =289;
relevance = 7.0;
purpose = "Aux";
num-states = 2;
states = ("present" "absent");
}

node T1(finite-states) {
title = "Cancer_f_r";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =833;
pos_y =293;
relevance = 7.0;
purpose = "Aux";
num-states = 2;
states = ("present" "absent");
}

node H(finite-states) {
title = "PSA aux";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =634;
pos_y =454;
relevance = 7.0;
purpose = "Aux";
num-states = 4;
states = ("greater20" "from10to20" "from4to10" "from0to4");
}

node V1(finite-states) {
title = "Ecography";
comment = "Detecta la presencia de areas 
de menor densidad sugerentes de
CP e integridad capsular (que se vea
entera, no rota)";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =876;
pos_y =405;
relevance = 7.0;
purpose = "Test";
num-states = 2;
states = ("present" "absent");
}

node Z(finite-states) {
title = "Fever";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =261;
pos_y =509;
relevance = 7.0;
purpose = "Sign";
num-states = 2;
states = ("present" "absent");
}

// links of the associated graph:

link A B;

link A J;

link A1 D;

link A1 B1;

link B1 C1;

link J S;

link J W;

link E E1;

link E D1;

link E L;

link E Y;

link F D;

link J I;

link G M;

link G N;

link G P;

link G R;

link G O;

link G F1;

link G E1;

link G K;

link G Y;

link G L;

link I U;

link I T;

link E I;

link H1 A1;

link C G1;

link I1 F;

link B I1;

link J1 I1;

link I1 K1;

link V K1;

link L1 K1;

link J M1;

link M1 C;

link D S;

link D W;

link D I;

link N1 D;

link J V;

link J O1;

link O1 L1;

link O1 K1;

link K1 F;

link F P1;

link K1 Q1;

link P1 Q1;

link D P1;

link F S;

link F W;

link A J1;

link Q1 R1;

link I R1;

link D S1;

link S1 E;

link E Q;

link I Q;

link Q1 Q;

link Q1 I;

link C1 T1;

link X T1;

link T1 E;

link E G1;

link J B;

link B1 T1;

link A T1;

link A D;

link F S1;

link A N1;

link B D;

link B T1;

link E H;

link F H;

link G H;

link H C;

link E V1;

link G V1;

link E G;

link I Z;

link Q1 Z;

//Network Relationships: 

relation A { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.0996 0.0921 0.1152 0.6931 );
}

relation J A { 
comment = "";
deterministic=false;
values= table (1.0 0.8 0.4 0.2 0.0 0.2 0.6 0.8 );
}

relation B1 A1 { 
comment = "";
deterministic=false;
values= table (0.05 0.15 0.95 0.85 );
}

relation C1 B1 { 
comment = "";
deterministic=false;
values= table (0.6 0.2 0.4 0.8 );
}

relation M G { 
comment = "";
deterministic=false;
values= table (0.9 0.3 0.1 0.7 );
}

relation N G { 
comment = "";
deterministic=false;
values= table (0.7 0.4 0.3 0.6 );
}

relation P G { 
comment = "";
deterministic=false;
values= table (0.6 0.2 0.4 0.8 );
}

relation O G { 
comment = "";
deterministic=false;
values= table (0.8 0.3 0.2 0.7 );
}

relation E1 E G { 
comment = "";
deterministic=false;
values= table (0.95 0.5 0.8 0.3 0.05 0.5 0.2 0.7 );
}

relation Y E G { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.9 0.3 0.8 0.0 0.1 0.7 0.2 0.0 0.0 0.0 0.0 1.0 );
}

relation L E G { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.9 0.75 0.01 0.0 0.08 0.18 0.55 0.5 0.02 0.07 0.44 0.5 );
}

relation U I { 
comment = "";
deterministic=false;
values= table (0.95 0.15 0.05 0.85 );
}

relation T I { 
comment = "";
deterministic=false;
values= table (0.95 0.05 0.05 0.95 );
}

relation H1 { 
comment = "";
deterministic=false;
values= table (0.97 0.03 );
}

relation A1 H1 { 
comment = "";
deterministic=false;
values= table (0.05 0.95 0.95 0.05 );
}

relation I1 B J1 { 
comment = "";
deterministic=false;
values= table (0.8 0.02 0.7 0.01 0.6 0.0010 0.2 0.98 0.3 0.99 0.4 0.999 );
}

relation M1 J { 
comment = "";
deterministic=false;
values= table (0.75 0.15 0.25 0.85 );
}

relation O1 J { 
comment = "";
deterministic=false;
values= table (0.3 0.05 0.7 0.95 );
}

relation L1 O1 { 
comment = "";
deterministic=false;
values= table (1.0 0.02 0.0 0.98 );
}

relation K1 I1 V L1 O1 { 
comment = "";
deterministic=false;
values= table (0.97 0.95 0.7 0.5 0.65 0.63 0.4 0.35 0.8 0.78 0.4 0.1 0.63 0.61 0.05 0.01 0.03 0.05 0.3 0.5 0.35 0.37 0.6 0.65 0.2 0.22 0.6 0.9 0.37 0.39 0.95 0.99 );
}

relation V J { 
comment = "";
deterministic=false;
values= table (0.85 0.05 0.15 0.95 );
}

relation P1 F D { 
comment = "";
deterministic=false;
values= table (0.7 0.6 0.4 0.01 0.3 0.4 0.6 0.99 );
}

relation S J D F { 
comment = "";
deterministic=false;
values= table (0.69 0.3 0.6 0.1 0.69 0.3 0.6 0.1 0.3 0.64 0.31 0.5 0.3 0.6 0.25 0.2 0.01 0.06 0.09 0.4 0.01 0.1 0.15 0.7 );
}

relation W J D F { 
comment = "";
deterministic=false;
values= table (0.956 0.93 0.93 0.9 0.67 0.5 0.5 0.25 0.044 0.07 0.07 0.1 0.33 0.5 0.5 0.75 );
}

relation Q1 K1 P1 { 
comment = "";
deterministic=false;
values= table (1.0 1.0 1.0 0.0 0.0 0.0 0.0 1.0 );
}

relation R1 Q1 I { 
comment = "";
deterministic=false;
values= table (0.99 0.99 0.7 0.1 0.01 0.01 0.3 0.9 );
}

relation Q E I Q1 { 
comment = "";
deterministic=false;
values= table (0.95 0.75 0.8 0.4 0.8 0.7 0.8 0.05 0.05 0.25 0.2 0.6 0.2 0.3 0.2 0.95 );
}

relation I J E D Q1 { 
comment = "";
deterministic=false;
values= table (0.87 0.53 0.83 0.37 0.84 0.41 0.79 0.2 0.85 0.45 0.8 0.25 0.82 0.3 0.75 0.05 0.13 0.47 0.17 0.63 0.16 0.59 0.21 0.8 0.15 0.55 0.2 0.75 0.18 0.7 0.25 0.95 );
}

relation F I1 K1 { 
comment = "";
deterministic=false;
values= table (0.6 0.4 0.4 0.2 0.4 0.6 0.6 0.8 );
}

relation C1 B1 { 
comment = "";
deterministic=false;
values= table (0.5 0.5 0.5 0.5 );
}

relation X { 
comment = "";
deterministic=false;
values= table (0.01 0.99 );
}

relation E S1 T1 { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (1.0 1.0 1.0 0.0 0.0 0.0 0.0 1.0 );
}

relation G1 C E { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.95 0.1 0.9 0.07 0.85 0.05 0.8 0.01 0.05 0.9 0.1 0.93 0.15 0.95 0.2 0.99 );
}

relation K G { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.95 0.01 0.05 0.99 );
}

relation R G { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.9 0.0010 0.1 0.999 );
}

relation D1 E { 
comment = "";
deterministic=false;
values= table (0.9 0.05 0.1 0.95 );
}

relation B A J { 
comment = "";
deterministic=false;
values= table (0.0010 0.0020 0.0030 0.05 0.1 0.25 0.3 0.4 0.01 0.02 0.2 0.4 0.5 0.65 0.65 0.59 0.989 0.978 0.797 0.55 0.4 0.1 0.05 0.01 );
}

relation J1 A { 
comment = "";
deterministic=false;
values= table (0.5 0.6 0.7 0.8 0.5 0.4 0.3 0.2 );
}

relation S1 D F { 
comment = "";
deterministic=false;
values= table (0.15 0.05 0.1 0.0010 0.85 0.95 0.9 0.999 );
}

relation F1 G { 
comment = "";
deterministic=false;
values= table (0.9 0.04 0.1 0.96 );
}

relation N1 A { 
comment = "";
deterministic=false;
values= table (0.0010 0.0050 0.0080 0.01 0.999 0.995 0.992 0.99 );
}

relation D A1 F N1 A B { 
comment = "";
deterministic=false;
values= table (0.7 0.75 0.85 0.75 0.78 0.8 0.6 0.64 0.78 0.4 0.45 0.5 0.64 0.7 0.78 0.55 0.58 0.58 0.43 0.45 0.55 0.2 0.25 0.3 0.64 0.7 0.78 0.55 0.58 0.58 0.43 0.45 0.55 0.2 0.25 0.3 0.4 0.45 0.5 0.3 0.35 0.39 0.1 0.13 0.2 0.0010 0.0040 0.01 0.8 0.85 0.9 0.8 0.82 0.85 0.7 0.7 0.8 0.4 0.45 0.5 0.67 0.73 0.81 0.58 0.61 0.61 0.45 0.47 0.57 0.22 0.26 0.33 0.67 0.73 0.81 0.58 0.61 0.61 0.45 0.47 0.57 0.22 0.26 0.33 0.43 0.48 0.53 0.35 0.38 0.41 0.13 0.17 0.24 0.0020 0.0080 0.01 0.3 0.25 0.15 0.25 0.22 0.2 0.4 0.36 0.22 0.6 0.55 0.5 0.36 0.3 0.22 0.45 0.42 0.42 0.57 0.55 0.45 0.8 0.75 0.65 0.36 0.3 0.22 0.45 0.42 0.42 0.57 0.55 0.45 0.8 0.75 0.65 0.6 0.55 0.5 0.7 0.65 0.61 0.9 0.87 0.8 0.999 0.996 0.99 0.2 0.15 0.1 0.2 0.18 0.15 0.3 0.3 0.2 0.6 0.55 0.5 0.33 0.27 0.19 0.42 0.39 0.39 0.55 0.53 0.43 0.78 0.74 0.63 0.33 0.27 0.19 0.42 0.39 0.39 0.55 0.53 0.43 0.78 0.74 0.63 0.57 0.52 0.47 0.65 0.62 0.59 0.87 0.83 0.76 0.998 0.992 0.99 );
}

relation T1 C1 X B1 A B { 
comment = "";
deterministic=false;
values= table (0.65 0.7 0.75 0.5 0.55 0.6 0.2 0.25 0.3 0.05 0.08 0.1 0.63 0.68 0.73 0.48 0.53 0.58 0.18 0.23 0.28 0.02 0.05 0.08 0.4 0.45 0.5 0.3 0.35 0.4 0.1 0.15 0.18 0.01 0.05 0.08 0.38 0.43 0.48 0.28 0.33 0.38 0.08 0.11 0.13 0.0010 0.0030 0.0050 0.4 0.45 0.5 0.3 0.35 0.4 0.1 0.15 0.18 0.01 0.05 0.08 0.38 0.43 0.48 0.28 0.33 0.38 0.08 0.11 0.13 0.0010 0.0030 0.0050 0.25 0.3 0.35 0.2 0.25 0.3 0.05 0.1 0.15 1.0E-4 5.0E-4 0.0010 0.23 0.28 0.33 0.18 0.23 0.28 0.03 0.08 0.13 1.0E-5 5.0E-5 1.0E-4 0.35 0.3 0.25 0.5 0.45 0.4 0.8 0.75 0.7 0.95 0.92 0.9 0.37 0.32 0.27 0.52 0.47 0.42 0.82 0.77 0.72 0.98 0.95 0.92 0.6 0.55 0.5 0.7 0.65 0.6 0.9 0.85 0.82 0.99 0.95 0.92 0.62 0.57 0.52 0.72 0.67 0.62 0.92 0.89 0.87 0.999 0.997 0.995 0.6 0.55 0.5 0.7 0.65 0.6 0.9 0.85 0.82 0.99 0.95 0.92 0.62 0.57 0.52 0.72 0.67 0.62 0.92 0.89 0.87 0.999 0.997 0.995 0.75 0.7 0.65 0.8 0.75 0.7 0.95 0.9 0.85 0.9999 0.9995 0.999 0.77 0.72 0.67 0.82 0.77 0.72 0.97 0.92 0.87 0.99999 0.99995 0.9999 );
}

relation E S1 T1 { 
comment = "";
deterministic=false;
values= table (0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 );
}

relation F I1 K1 { 
comment = "";
deterministic=false;
values= table (0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 );
}

relation H E F G { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.5 0.45 0.5 0.3 0.02 0.02 0.02 0.02 0.25 0.3 0.25 0.35 0.1 0.09 0.09 0.08 0.25 0.24 0.24 0.33 0.38 0.19 0.19 0.1 0.0 0.01 0.01 0.02 0.5 0.7 0.7 0.8 );
}

relation V1 E G { 
comment = "";
deterministic=false;
values= table (1.0 0.9 0.1 0.1 0.0 0.1 0.9 0.9 );
}

relation G E { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.5 0.0020 0.5 0.998 );
}

relation Z I Q1 { 
comment = "";
deterministic=false;
values= table (0.95 0.6 0.6 0.01 0.05 0.4 0.4 0.99 );
}

relation C M1 H { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.5 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.5 0.5 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.4 0.5 0.0 0.0 0.0 1.0 0.0 0.0 0.1 0.5 1.0 0.0 0.0 0.0 1.0 );
}

}

