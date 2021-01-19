//"Obtener el par de restaurantes mas proximos para cada "cuisine" (tipo de cocina), 
//mostrando el tipo de cocina, el nombre del restaurante, la direccion, la distancia entre ellos y la cantidad de restaurantes evaluados para cada 
//"cuisine", para aquellos restaurantes que hayan tenido un "score" mayor que 7 en alguna ocasion". (Puntuacion de la pregunta: hasta 8.5 pts. sobre 10)

db.runCommand({ 
mapReduce: "restaurants",

map: function Map() {
var key = this.cuisine; //Obtener el par de restaurantes mas proximos para cada "cuisine" (tipo de cocina), se muestra como parte de la respuesta
emit(key, {
	"data":
	[
		{
		"name" : this.name,
		"address" : this.address,
		}
	]
	}
	);
},

reduce: function Reduce(key, values){
	var reduced = {"data":[]};
	for (var i in values) {
	var inter = values[i];
	for (var j in inter.data) {
	reduced.data.push(inter.data[j]);
	}
	}
return reduced;
},

finalize : function Finalize(key, reduced) {
if (reduced.data.length == 1) {
    r0=reduced.data[0]
return { "restaurante":{"name": r0.name, "address":r0.address},
"distance":"none", "evaluados":"1"};
}

var min_dist = 999999999999;
var restaurant1 = { "name": "" };
var restaurant2 = { "name": "" };
var r1;
var r2;
var d = 0.1;
for (var i in reduced.data) {
for (var j in reduced.data) {
if (i>=j) continue;
r1 = reduced.data[i];
r2 = reduced.data[j];

r1lat = r1.address.coord[0] //lat
r1lon = r1.address.coord[1] //lon
r2lat = r2.address.coord[0] //lat
r2lon = r2.address.coord[1] //lon

latitudeSeparation=(r1lat-r2lat)
longitudeSeparation=(r1lon-r2lon)
d = Math.sqrt(Math.pow(latitudeSeparation,2)+Math.pow(longitudeSeparation,2)); //Euclidean distance
if (d < min_dist && d > 0) {
min_dist = d;
restaurant1 = r1;
restaurant2 = r2;
}

}
}

return {"restaurante 1":{"name": restaurant1.name, "address":restaurant1.address}, // nombre y direccion de cada restaurante
"restaurante 2":{"name": restaurant2.name, "address":restaurant2.address},
"distancia de separacion":min_dist, "evaluados":reduced.data.length}; // distancia entre ellos y la cantidad de restaurantes revisados
},

query : {"grades.score" : {$gt:7} }, //, para aquellos restaurantes que hayan tenido un "score" mayor que 7 en alguna ocasion.
out: {replace:"rest_mapreduce"}
})