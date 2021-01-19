bnet  Cancer { 
// Propiedades de la red. 
	title="Red bayesiana para el diagnostico de cancer metastasico";
	comment="Es el ejemplo mas famoso de red bayesiana";
	author="Greg Cooper";
	whochanged="Equipo Proyecto Elvira";
	whenchanged="22/04/98";
// Por defecto, locked=false;
// Por defecto, version=1.0;
	default node states=(ausente presente);
// en vez de ``(absent present)" 

// Nodos

node Cancer {
// Por defecto, pertenece a la clase finite-states.  
	title="Cancer metastasico";
	comment="Indica si se da la enfermedad o no";
// Por defecto, states=(ausente presente)  
 }

node Calcio {
	title="Elevacion del calcio serico";
	states=(normal elevado);
 }

node Tumor {
	title="Tumor cerebral";
         states = (presente ausente);
 }

node Coma;
// [Por defecto, ``title" es el mismo que ``identifier".]}  

node Jaquecas;

// Enlaces

link Cancer Calcio; 
// Por defecto, es un enlace dirigido.  

link Cancer Tumor; 

link Calcio Coma; 

link Tumor Coma; 

link Tumor Jaquecas; 

// Relaciones

relation Cancer { 
// ``Cancer" {\it indica cuales son los nodos que intervienen.}  
	 comment = "Prevalencia del cancer metastasico"; 
// Por defecto, la relacion esta activa.  
// Por defecto, es una probabilidad condicionada.  
// No hace falta decir que la representacion es una tabla: ya se ve.  
	values=table (0.2 0.8);
}

relation Calcio Cancer { 
	values=table ([elevado,presente]=0.8,
		[elevado,ausente]=0.2,
		[normal,presente]=0.2,
		[normal,ausente]=0.8);
 }

relation Tumor Cancer { 
	values=table ([0,presente]=0.2,
		[0,ausente]=0.05,
		[1,presente]=0.8,
		[1,ausente]=0.95);
 }

relation Coma Calcio Tumor { 
	comment="Tanto la elevacion del calcio serico como el tumor
			cerebral pueden producir coma";
	values=tree (
		case  Coma{
                  presente=case  Calcio{ 
			     elevado=0.8;
			     normal=case  Tumor{
			              presente=0.8;
			              ausente=0.05;
			            } 
                           } 
                  ausente=case  Calcio{ 
			    elevado=0.2;
			    normal=case  Tumor{
			             presente=0.2;
			             ausente=0.95;
			           }
			  }
		}
	);
 }

relation Jaquecas Tumor { 
	values=table (0.8, 0.6,0.2,0.4);
 }

} // Fin de la red.
