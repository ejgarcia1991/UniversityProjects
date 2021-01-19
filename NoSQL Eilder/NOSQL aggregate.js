//"Obtener el par de restaurantes mas proximos para cada "cuisine" (tipo de cocina), 
//mostrando el tipo de cocina, el nombre del restaurante, la direccion, la distancia entre ellos y la cantidad de restaurantes evaluados para cada 
//"cuisine", para aquellos restaurantes que hayan tenido un "score" mayor que 7 en alguna ocasion". (Puntuacion de la pregunta: hasta 8.5 pts. sobre 10)

//Version aggregate	
db.runCommand({
aggregate: "restaurants",
pipeline : [
{$match: {"grades.score": {$gt:7}}},

{$group: {_id: "$cuisine", "restaurante1":{$push: {lugar: "$address", nombre:"$name"}},
			   "restaurante2":{$push: {lugar: "$address", nombre:"$name"}},
                           "evaluados": { $sum: 1 }
                           }},

							
{$unwind: "$restaurante1"},
{$unwind: "$restaurante2"},

{$project: {_id: 0, cuisine: "$_id", restaurante1: "$restaurante1", restaurante2: "$restaurante2",evaluados:"$evaluados",
distancia:{  $sqrt: {
$sum: [
{$pow: [{$subtract: [{$arrayElemAt: [ "$restaurante1.lugar.coord", 0 ]},{$arrayElemAt: [ "$restaurante2.lugar.coord", 0 ]}]},2 ]},
{$pow: [{$subtract: [{$arrayElemAt: [ "$restaurante1.lugar.coord", 1 ]},{$arrayElemAt: [ "$restaurante2.lugar.coord", 1 ]}]},2 ]}
]
}}
}},

{$redact: {"$cond": [{$and:[{"$lt": ["$restaurante1", "$restaurante2"]},{"$ne":["$distancia",0.0]}]},"$$KEEP","$$PRUNE"]}},

{$group: {_id: "$cuisine", "dist_min": {$min: "$distancia"}, "evaluados": { $first: "$evaluados" },
"parejas":{$push: {restaurante1: "$restaurante1", restaurante2: "$restaurante2", distancia: "$distancia"}}}
},
{$unwind: "$parejas"}, 

{$redact: {"$cond": [{"$eq": ["$dist_min", "$parejas.distancia"]}, "$$KEEP", "$$PRUNE"]}},


{$project: {_id: 0, "cuisine": "$_id","Restaurante1": "$parejas.restaurante1", "Restaurante2": "$parejas.restaurante2",
"distancia": "$dist_min","evaluados":"$evaluados"}},

{$out: "rest_aggregate"}
],

allowDiskUse: true,
cursor: { batchSize: 500 }}); 