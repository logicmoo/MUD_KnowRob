{
  "GND":{
    "math:":"http://www.w3.org/2000/10/swap/math#",
    "nsp0:":"file:/euler/",
    "e:":"http://eulersharp.sourceforge.net/2003/03swap/log-rules#",
    "list:":"http://www.w3.org/2000/10/swap/list#",
    "q:":"http://www.w3.org/2004/ql#",
    "xsd:":"http://www.w3.org/2001/XMLSchema#",
    "log:":"http://www.w3.org/2000/10/swap/log#",
    "nsp1:":"file:/euler/rpo-rules.n3#",
    "rpo:":"http://eulersharp.sourceforge.net/2003/03swap/rpo-rules#",
    "r:":"http://www.w3.org/2000/10/swap/reason#",
    "rdfs:":"http://www.w3.org/2000/01/rdf-schema#",
    "fn:":"http://www.w3.org/2006/xpath-functions#",
    "time:":"http://www.w3.org/2000/10/swap/time#",
    ":":"http://www.agfa.com/w3c/euler/medP#",
    "rdf:":"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "var:":"http://localhost/var#",
    "str:":"http://www.w3.org/2000/10/swap/string#",
    "owl:":"http://www.w3.org/2002/07/owl#",
    "n3:":"http://www.w3.org/2004/06/rei#"
  },
  "rpo:inSomeOf":[
    {head:{pred:"rpo:inSomeOf", args:[{pred:"?X", args:[]}, {pred:"?L", args:[]}]}, body:[{pred:"rdf:first", args:[{pred:"?L", args:[]}, {pred:"?A", args:[]}]}, {pred:"a", args:[{pred:"?L", args:[]}, {pred:"rdf:List", args:[]}]}, {pred:"a", args:[{pred:"?X", args:[]}, {pred:"?A", args:[]}]}]},
    {head:{pred:"rpo:inSomeOf", args:[{pred:"?X", args:[]}, {pred:"?L", args:[]}]}, body:[{pred:"rdf:rest", args:[{pred:"?L", args:[]}, {pred:"?R", args:[]}]}, {pred:"a", args:[{pred:"?L", args:[]}, {pred:"rdf:List", args:[]}]}, {pred:"rpo:inSomeOf", args:[{pred:"?X", args:[]}, {pred:"?R", args:[]}]}]}
  ],
  "list:in":[
    {head:{pred:"list:in", args:[{pred:"?I", args:[]}, {pred:"?L", args:[]}]}, body:[{pred:"rdf:first", args:[{pred:"?L", args:[]}, {pred:"?I", args:[]}]}, {pred:"a", args:[{pred:"?L", args:[]}, {pred:"rdf:List", args:[]}]}]},
    {head:{pred:"list:in", args:[{pred:"?I", args:[]}, {pred:"?L", args:[]}]}, body:[{pred:"rdf:rest", args:[{pred:"?L", args:[]}, {pred:"?R", args:[]}]}, {pred:"a", args:[{pred:"?L", args:[]}, {pred:"rdf:List", args:[]}]}, {pred:"list:in", args:[{pred:"?I", args:[]}, {pred:"?R", args:[]}]}]}
  ],
  ":prescribedFor":[
    {head:{pred:":prescribedFor", args:[{pred:":aspirin", args:[]}, {pred:":GastroEntritis", args:[]}]}, body:[]}
  ],
  "rdfs:range":[
    {head:{pred:"rdfs:range", args:[{pred:":excludedFor", args:[]}, {pred:"rdf:List", args:[]}]}, body:[]},
    {head:{pred:"rdfs:range", args:[{pred:"rdf:rest", args:[]}, {pred:"rdf:List", args:[]}]}, body:[]},
    {head:{pred:"rdfs:range", args:[{pred:"rdfs:subClassOf", args:[]}, {pred:"rdfs:Class", args:[]}]}, body:[]},
    {head:{pred:"rdfs:range", args:[{pred:"rdfs:subPropertyOf", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"rdfs:range", args:[{pred:"owl:equivalentClass", args:[]}, {pred:"rdfs:Class", args:[]}]}, body:[]},
    {head:{pred:"rdfs:range", args:[{pred:"owl:equivalentProperty", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"rdfs:range", args:[{pred:"owl:inverseOf", args:[]}, {pred:"owl:ObjectProperty", args:[]}]}, body:[]},
    {head:{pred:"rdfs:range", args:[{pred:"owl:distinctMembers", args:[]}, {pred:"rdf:List", args:[]}]}, body:[]},
    {head:{pred:"rdfs:range", args:[{pred:"owl:oneOf", args:[]}, {pred:"rdf:List", args:[]}]}, body:[]},
    {head:{pred:"rdfs:range", args:[{pred:"owl:intersectionOf", args:[]}, {pred:"rdf:List", args:[]}]}, body:[]},
    {head:{pred:"rdfs:range", args:[{pred:"owl:unionOf", args:[]}, {pred:"rdf:List", args:[]}]}, body:[]},
    {head:{pred:"rdfs:range", args:[{pred:"owl:complementOf", args:[]}, {pred:"rdfs:Class", args:[]}]}, body:[]}
  ],
  ":excludedFor":[
    {head:{pred:":excludedFor", args:[{pred:":aspirin", args:[]}, {pred:".", args:[{pred:":GastricUlcer", args:[]}, {pred:".", args:[{pred:":PostSurgery", args:[]}, {pred:".", args:[]}]}]}]}, body:[]}
  ],
  "rpo:mu":[
    {head:{pred:"rpo:mu", args:[{pred:".", args:[{pred:":Ann", args:[]}, {pred:".", args:[{pred:":GastroEntritis", args:[]}, {pred:".", args:[]}]}]}, {pred:"0.8", args:[]}]}, body:[]},
    {head:{pred:"rpo:mu", args:[{pred:".", args:[{pred:":Ann", args:[]}, {pred:".", args:[{pred:":GastricUlcer", args:[]}, {pred:".", args:[]}]}]}, {pred:"0.006", args:[]}]}, body:[]},
    {head:{pred:"rpo:mu", args:[{pred:".", args:[{pred:":Ann", args:[]}, {pred:".", args:[{pred:":PostSurgery", args:[]}, {pred:".", args:[]}]}]}, {pred:"0", args:[]}]}, body:[]},
    {head:{pred:"rpo:mu", args:[{pred:".", args:[{pred:"?X", args:[]}, {pred:".", args:[{pred:"?C", args:[]}, {pred:".", args:[]}]}]}, {pred:"?M", args:[]}]}, body:[{pred:"owl:intersectionOf", args:[{pred:"?C", args:[]}, {pred:".", args:[{pred:"?A", args:[]}, {pred:".", args:[{pred:"?B", args:[]}, {pred:".", args:[]}]}]}]}, {pred:"rpo:mu", args:[{pred:".", args:[{pred:"?X", args:[]}, {pred:".", args:[{pred:"?A", args:[]}, {pred:".", args:[]}]}]}, {pred:"?M", args:[]}]}, {pred:"rpo:mu", args:[{pred:".", args:[{pred:"?X", args:[]}, {pred:".", args:[{pred:"?B", args:[]}, {pred:".", args:[]}]}]}, {pred:"?N", args:[]}]}, {pred:"math:lessThan", args:[{pred:"?M", args:[]}, {pred:"?N", args:[]}]}]},
    {head:{pred:"rpo:mu", args:[{pred:".", args:[{pred:"?X", args:[]}, {pred:".", args:[{pred:"?C", args:[]}, {pred:".", args:[]}]}]}, {pred:"?N", args:[]}]}, body:[{pred:"owl:intersectionOf", args:[{pred:"?C", args:[]}, {pred:".", args:[{pred:"?A", args:[]}, {pred:".", args:[{pred:"?B", args:[]}, {pred:".", args:[]}]}]}]}, {pred:"rpo:mu", args:[{pred:".", args:[{pred:"?X", args:[]}, {pred:".", args:[{pred:"?A", args:[]}, {pred:".", args:[]}]}]}, {pred:"?M", args:[]}]}, {pred:"rpo:mu", args:[{pred:".", args:[{pred:"?X", args:[]}, {pred:".", args:[{pred:"?B", args:[]}, {pred:".", args:[]}]}]}, {pred:"?N", args:[]}]}, {pred:"math:notLessThan", args:[{pred:"?M", args:[]}, {pred:"?N", args:[]}]}]},
    {head:{pred:"rpo:mu", args:[{pred:".", args:[{pred:"?X", args:[]}, {pred:".", args:[{pred:"?C", args:[]}, {pred:".", args:[]}]}]}, {pred:"?N", args:[]}]}, body:[{pred:"owl:unionOf", args:[{pred:"?C", args:[]}, {pred:".", args:[{pred:"?A", args:[]}, {pred:".", args:[{pred:"?B", args:[]}, {pred:".", args:[]}]}]}]}, {pred:"rpo:mu", args:[{pred:".", args:[{pred:"?X", args:[]}, {pred:".", args:[{pred:"?A", args:[]}, {pred:".", args:[]}]}]}, {pred:"?M", args:[]}]}, {pred:"rpo:mu", args:[{pred:".", args:[{pred:"?X", args:[]}, {pred:".", args:[{pred:"?B", args:[]}, {pred:".", args:[]}]}]}, {pred:"?N", args:[]}]}, {pred:"math:lessThan", args:[{pred:"?M", args:[]}, {pred:"?N", args:[]}]}]},
    {head:{pred:"rpo:mu", args:[{pred:".", args:[{pred:"?X", args:[]}, {pred:".", args:[{pred:"?C", args:[]}, {pred:".", args:[]}]}]}, {pred:"?M", args:[]}]}, body:[{pred:"owl:unionOf", args:[{pred:"?C", args:[]}, {pred:".", args:[{pred:"?A", args:[]}, {pred:".", args:[{pred:"?B", args:[]}, {pred:".", args:[]}]}]}]}, {pred:"rpo:mu", args:[{pred:".", args:[{pred:"?X", args:[]}, {pred:".", args:[{pred:"?A", args:[]}, {pred:".", args:[]}]}]}, {pred:"?M", args:[]}]}, {pred:"rpo:mu", args:[{pred:".", args:[{pred:"?X", args:[]}, {pred:".", args:[{pred:"?B", args:[]}, {pred:".", args:[]}]}]}, {pred:"?N", args:[]}]}, {pred:"math:notLessThan", args:[{pred:"?M", args:[]}, {pred:"?N", args:[]}]}]},
    {head:{pred:"rpo:mu", args:[{pred:".", args:[{pred:"?X", args:[]}, {pred:".", args:[{pred:"?C", args:[]}, {pred:".", args:[]}]}]}, {pred:"?N", args:[]}]}, body:[{pred:"owl:complementOf", args:[{pred:"?C", args:[]}, {pred:"?A", args:[]}]}, {pred:"rpo:mu", args:[{pred:".", args:[{pred:"?X", args:[]}, {pred:".", args:[{pred:"?A", args:[]}, {pred:".", args:[]}]}]}, {pred:"?M", args:[]}]}, {pred:"math:difference", args:[{pred:".", args:[{pred:"1.0", args:[]}, {pred:".", args:[{pred:"?M", args:[]}, {pred:".", args:[]}]}]}, {pred:"?N", args:[]}]}]}
  ],
  "rdfs:subPropertyOf":[
    {head:{pred:"rdfs:subPropertyOf", args:[{pred:"owl:equivalentClass", args:[]}, {pred:"rdfs:subClassOf", args:[]}]}, body:[]},
    {head:{pred:"rdfs:subPropertyOf", args:[{pred:"owl:equivalentProperty", args:[]}, {pred:"rdfs:subPropertyOf", args:[]}]}, body:[]}
  ],
  "rpo:subListOf":[
    {head:{pred:"rpo:subListOf", args:[{pred:"?L", args:[]}, {pred:"?L", args:[]}]}, body:[{pred:"a", args:[{pred:"?L", args:[]}, {pred:"rdf:List", args:[]}]}]},
    {head:{pred:"rpo:subListOf", args:[{pred:"?X", args:[]}, {pred:"?L", args:[]}]}, body:[{pred:"rdf:rest", args:[{pred:"?L", args:[]}, {pred:"?R", args:[]}]}, {pred:"a", args:[{pred:"?L", args:[]}, {pred:"rdf:List", args:[]}]}, {pred:"rpo:subListOf", args:[{pred:"?X", args:[]}, {pred:"?R", args:[]}]}]}
  ],
  "rpo:inAllOf":[
    {head:{pred:"rpo:inAllOf", args:[{pred:"?X", args:[]}, {pred:"?L", args:[]}]}, body:[{pred:"rdf:first", args:[{pred:"?L", args:[]}, {pred:"?A", args:[]}]}, {pred:"a", args:[{pred:"?L", args:[]}, {pred:"rdf:List", args:[]}]}, {pred:"a", args:[{pred:"?X", args:[]}, {pred:"?A", args:[]}]}, {pred:"rdf:rest", args:[{pred:"?L", args:[]}, {pred:".", args:[]}]}]},
    {head:{pred:"rpo:inAllOf", args:[{pred:"?X", args:[]}, {pred:"?L", args:[]}]}, body:[{pred:"rdf:first", args:[{pred:"?L", args:[]}, {pred:"?A", args:[]}]}, {pred:"a", args:[{pred:"?L", args:[]}, {pred:"rdf:List", args:[]}]}, {pred:"a", args:[{pred:"?X", args:[]}, {pred:"?A", args:[]}]}, {pred:"rdf:rest", args:[{pred:"?L", args:[]}, {pred:"?R", args:[]}]}, {pred:"rpo:inAllOf", args:[{pred:"?X", args:[]}, {pred:"?R", args:[]}]}]}
  ],
  "rdfs:domain":[
    {head:{pred:"rdfs:domain", args:[{pred:"rdf:first", args:[]}, {pred:"rdf:List", args:[]}]}, body:[]},
    {head:{pred:"rdfs:domain", args:[{pred:"rdf:rest", args:[]}, {pred:"rdf:List", args:[]}]}, body:[]},
    {head:{pred:"rdfs:domain", args:[{pred:"rdfs:subClassOf", args:[]}, {pred:"rdfs:Class", args:[]}]}, body:[]},
    {head:{pred:"rdfs:domain", args:[{pred:"rdfs:subPropertyOf", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"rdfs:domain", args:[{pred:"owl:equivalentClass", args:[]}, {pred:"rdfs:Class", args:[]}]}, body:[]},
    {head:{pred:"rdfs:domain", args:[{pred:"owl:equivalentProperty", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"rdfs:domain", args:[{pred:"owl:inverseOf", args:[]}, {pred:"owl:ObjectProperty", args:[]}]}, body:[]},
    {head:{pred:"rdfs:domain", args:[{pred:"owl:distinctMembers", args:[]}, {pred:"owl:AllDifferent", args:[]}]}, body:[]},
    {head:{pred:"rdfs:domain", args:[{pred:"owl:oneOf", args:[]}, {pred:"rdfs:Class", args:[]}]}, body:[]},
    {head:{pred:"rdfs:domain", args:[{pred:"owl:intersectionOf", args:[]}, {pred:"rdfs:Class", args:[]}]}, body:[]},
    {head:{pred:"rdfs:domain", args:[{pred:"owl:unionOf", args:[]}, {pred:"rdfs:Class", args:[]}]}, body:[]},
    {head:{pred:"rdfs:domain", args:[{pred:"owl:complementOf", args:[]}, {pred:"rdfs:Class", args:[]}]}, body:[]}
  ],
  "owl:differentFrom":[
    {head:{pred:"owl:differentFrom", args:[{pred:"?X", args:[]}, {pred:"?Y", args:[]}]}, body:[{pred:"owl:distinctMembers", args:[{pred:"?A", args:[]}, {pred:"?D", args:[]}]}, {pred:"rpo:subListOf", args:[{pred:"?L", args:[]}, {pred:"?D", args:[]}]}, {pred:"rdf:first", args:[{pred:"?L", args:[]}, {pred:"?X", args:[]}]}, {pred:"rdf:rest", args:[{pred:"?L", args:[]}, {pred:"?R", args:[]}]}, {pred:"list:in", args:[{pred:"?Y", args:[]}, {pred:"?R", args:[]}]}]}
  ],
  "?Y":[
    {head:{pred:"?Y", args:[{pred:"?S", args:[]}, {pred:"?O", args:[]}]}, body:[{pred:"owl:sameAs", args:[{pred:"?X", args:[]}, {pred:"?Y", args:[]}]}, {pred:"?X", args:[{pred:"?S", args:[]}, {pred:"?O", args:[]}]}]}
  ],
  "owl:sameAs":[
    {head:{pred:"owl:sameAs", args:[{pred:"?X", args:[]}, {pred:"?Y", args:[]}]}, body:[{pred:"a", args:[{pred:"?P", args:[]}, {pred:"owl:FunctionalProperty", args:[]}]}, {pred:"?P", args:[{pred:"?S", args:[]}, {pred:"?X", args:[]}]}, {pred:"?P", args:[{pred:"?S", args:[]}, {pred:"?Y", args:[]}]}]},
    {head:{pred:"owl:sameAs", args:[{pred:"?X", args:[]}, {pred:"?Y", args:[]}]}, body:[{pred:"a", args:[{pred:"?P", args:[]}, {pred:"owl:InverseFunctionalProperty", args:[]}]}, {pred:"?P", args:[{pred:"?X", args:[]}, {pred:"?O", args:[]}]}, {pred:"?P", args:[{pred:"?Y", args:[]}, {pred:"?O", args:[]}]}]}
  ],
  ":fineWith":[
    {head:{pred:":fineWith", args:[{pred:"?WHO", args:[]}, {pred:".", args:[]}]}, body:[{pred:"a", args:[{pred:"?WHO", args:[]}, {pred:":Patient", args:[]}]}]},
    {head:{pred:":fineWith", args:[{pred:"?WHO", args:[]}, {pred:"?L", args:[]}]}, body:[{pred:"rdf:first", args:[{pred:"?L", args:[]}, {pred:"?D", args:[]}]}, {pred:"a", args:[{pred:"?L", args:[]}, {pred:"rdf:List", args:[]}]}, {pred:"rpo:mu", args:[{pred:".", args:[{pred:"?WHO", args:[]}, {pred:".", args:[{pred:"?D", args:[]}, {pred:".", args:[]}]}]}, {pred:"?N", args:[]}]}, {pred:"math:lessThan", args:[{pred:"?N", args:[]}, {pred:"0.01", args:[]}]}, {pred:"rdf:rest", args:[{pred:"?L", args:[]}, {pred:"?R", args:[]}]}, {pred:":fineWith", args:[{pred:"?WHO", args:[]}, {pred:"?R", args:[]}]}]}
  ],
  "?R":[
    {head:{pred:"?R", args:[{pred:"?S", args:[]}, {pred:"?O", args:[]}]}, body:[{pred:"rdfs:subPropertyOf", args:[{pred:"?P", args:[]}, {pred:"?R", args:[]}]}, {pred:"?P", args:[{pred:"?S", args:[]}, {pred:"?O", args:[]}]}]}
  ],
  "?Q":[
    {head:{pred:"?Q", args:[{pred:"?O", args:[]}, {pred:"?S", args:[]}]}, body:[{pred:"owl:inverseOf", args:[{pred:"?P", args:[]}, {pred:"?Q", args:[]}]}, {pred:"?P", args:[{pred:"?S", args:[]}, {pred:"?O", args:[]}]}]}
  ],
  "?P":[
    {head:{pred:"?P", args:[{pred:"?Y", args:[]}, {pred:"?O", args:[]}]}, body:[{pred:"owl:sameAs", args:[{pred:"?X", args:[]}, {pred:"?Y", args:[]}]}, {pred:"a", args:[{pred:"?P", args:[]}, {pred:"rdf:Property", args:[]}]}, {pred:"?P", args:[{pred:"?X", args:[]}, {pred:"?O", args:[]}]}]},
    {head:{pred:"?P", args:[{pred:"?S", args:[]}, {pred:"?Y", args:[]}]}, body:[{pred:"owl:sameAs", args:[{pred:"?X", args:[]}, {pred:"?Y", args:[]}]}, {pred:"a", args:[{pred:"?P", args:[]}, {pred:"rdf:Property", args:[]}]}, {pred:"?P", args:[{pred:"?S", args:[]}, {pred:"?X", args:[]}]}]},
    {head:{pred:"?P", args:[{pred:"?O", args:[]}, {pred:"?S", args:[]}]}, body:[{pred:"a", args:[{pred:"?P", args:[]}, {pred:"owl:SymmetricProperty", args:[]}]}, {pred:"?P", args:[{pred:"?S", args:[]}, {pred:"?O", args:[]}]}]},
    {head:{pred:"?P", args:[{pred:"?S", args:[]}, {pred:"?O", args:[]}]}, body:[{pred:"a", args:[{pred:"?P", args:[]}, {pred:"owl:TransitiveProperty", args:[]}]}, {pred:"?P", args:[{pred:"?X", args:[]}, {pred:"?O", args:[]}]}, {pred:"?P", args:[{pred:"?S", args:[]}, {pred:"?X", args:[]}]}]}
  ],
  "nsp1:rcsid":[
    {head:{pred:"nsp1:rcsid", args:[{pred:"nsp0:rpo-rules.n3", args:[]}, {pred:"\"$Id: med.js 1295 2007-05-11 16:52:51Z josd $\"", args:[]}]}, body:[]}
  ],
  "a":[
    {head:{pred:"a", args:[{pred:":Ann", args:[]}, {pred:":Patient", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"?R", args:[]}, {pred:"rdf:List", args:[]}]}, body:[{pred:"rdf:rest", args:[{pred:"?L", args:[]}, {pred:"?R", args:[]}]}, {pred:"a", args:[{pred:"?L", args:[]}, {pred:"rdf:List", args:[]}]}]},
    {head:{pred:"a", args:[{pred:"rdf:first", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"rdf:rest", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"rdfs:subClassOf", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"rdfs:subClassOf", args:[]}, {pred:"owl:TransitiveProperty", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"rdfs:subPropertyOf", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"rdfs:subPropertyOf", args:[]}, {pred:"owl:TransitiveProperty", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:equivalentClass", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:equivalentClass", args:[]}, {pred:"owl:SymmetricProperty", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:equivalentProperty", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:equivalentProperty", args:[]}, {pred:"owl:SymmetricProperty", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:sameAs", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:sameAs", args:[]}, {pred:"owl:SymmetricProperty", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:sameAs", args:[]}, {pred:"owl:TransitiveProperty", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:inverseOf", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:inverseOf", args:[]}, {pred:"owl:SymmetricProperty", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:differentFrom", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:differentFrom", args:[]}, {pred:"owl:SymmetricProperty", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:distinctMembers", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:oneOf", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:intersectionOf", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:unionOf", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"owl:complementOf", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[]},
    {head:{pred:"a", args:[{pred:"?P", args:[]}, {pred:"rdf:Property", args:[]}]}, body:[{pred:"?P", args:[{pred:"?S", args:[]}, {pred:"?O", args:[]}]}]},
    {head:{pred:"a", args:[{pred:"?S", args:[]}, {pred:"?C", args:[]}]}, body:[{pred:"rdfs:domain", args:[{pred:"?P", args:[]}, {pred:"?C", args:[]}]}, {pred:"?P", args:[{pred:"?S", args:[]}, {pred:"?O", args:[]}]}]},
    {head:{pred:"a", args:[{pred:"?O", args:[]}, {pred:"?C", args:[]}]}, body:[{pred:"rdfs:range", args:[{pred:"?P", args:[]}, {pred:"?C", args:[]}]}, {pred:"?P", args:[{pred:"?S", args:[]}, {pred:"?O", args:[]}]}]},
    {head:{pred:"a", args:[{pred:"?S", args:[]}, {pred:"?B", args:[]}]}, body:[{pred:"rdfs:subClassOf", args:[{pred:"?A", args:[]}, {pred:"?B", args:[]}]}, {pred:"a", args:[{pred:"?S", args:[]}, {pred:"?A", args:[]}]}]},
    {head:{pred:"a", args:[{pred:"?X", args:[]}, {pred:"?C", args:[]}]}, body:[{pred:"owl:oneOf", args:[{pred:"?C", args:[]}, {pred:"?L", args:[]}]}, {pred:"list:in", args:[{pred:"?X", args:[]}, {pred:"?L", args:[]}]}]},
    {head:{pred:"a", args:[{pred:"?X", args:[]}, {pred:"?C", args:[]}]}, body:[{pred:"owl:intersectionOf", args:[{pred:"?C", args:[]}, {pred:"?L", args:[]}]}, {pred:"rpo:inAllOf", args:[{pred:"?X", args:[]}, {pred:"?L", args:[]}]}]},
    {head:{pred:"a", args:[{pred:"?X", args:[]}, {pred:"?C", args:[]}]}, body:[{pred:"owl:unionOf", args:[{pred:"?C", args:[]}, {pred:"?L", args:[]}]}, {pred:"rpo:inSomeOf", args:[{pred:"?X", args:[]}, {pred:"?L", args:[]}]}]}
  ],
  "owl:distinctMembers":[
    {head:{pred:"owl:distinctMembers", args:[{pred:"?A", args:[]}, {pred:"?R", args:[]}]}, body:[{pred:"owl:distinctMembers", args:[{pred:"?A", args:[]}, {pred:"?D", args:[]}]}, {pred:"rdf:rest", args:[{pred:"?D", args:[]}, {pred:"?R", args:[]}]}]}
  ],
  ":isPrescribed":[
    {head:{pred:":isPrescribed", args:[{pred:"?WHO", args:[]}, {pred:"?MED", args:[]}]}, body:[{pred:":prescribedFor", args:[{pred:"?MED", args:[]}, {pred:"?D", args:[]}]}, {pred:":excludedFor", args:[{pred:"?MED", args:[]}, {pred:"?L", args:[]}]}, {pred:":fineWith", args:[{pred:"?WHO", args:[]}, {pred:"?L", args:[]}]}, {pred:"rpo:mu", args:[{pred:".", args:[{pred:"?WHO", args:[]}, {pred:".", args:[{pred:"?D", args:[]}, {pred:".", args:[]}]}]}, {pred:"?N", args:[]}]}, {pred:"math:greaterThan", args:[{pred:"?N", args:[]}, {pred:"0.7", args:[]}]}]}
  ],
  "":[
    {head:{}, body:[{pred:":isPrescribed", args:[{pred:"?WHO", args:[]}, {pred:"?MED", args:[]}]}]}
  ]
}
