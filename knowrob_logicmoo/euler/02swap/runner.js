// Euler proof mechanism -- Jos De Roo
// $Id: runner.js 1398 2007-07-20 16:41:33Z josd $
// PxButton | sup | javac classes/euler/Support.java
// PxButton | cat | cat euler.js builtins.js json.js runner.js > classes/Euler4.js
// PxButton | jsc | java org.mozilla.javascript.tools.jsc.Main -opt 9 -package euler classes/Euler4.js
// PxButton | med | bash .euler -json /euler/medP.n3 /euler/rpo-rules.n3 -query /euler/medF.n3 > med.js
// PxButton | run | java euler.Euler4 med.js
// PxButton | zip | zip -9ur deployment/euler4.zip README *.js *.html classes classes/euler

if (arguments.length == 0) print("Usage: java euler.Euler4 cases")
else {
  defineClass('euler.Support')
  var s = new Support()
  eval('var cases =' + s.fromWeb(arguments[0]))
  var t = new Date()
  for (var i = 0; i < cases[""].length; i++) if (cases[""][i] != null) prove(cases[""][i], -1)
  for (var i in evidence) evidence["GND"] = cases["GND"]
  t = new Date() - t
//  print('//' + version + '\n')
  print(JSON.stringify(evidence) + '\n')
  print('//ENDS [' + step + ' steps/' + t + ' msec]')
}
