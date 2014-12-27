// Euler proof mechanism -- Jos De Roo
version = '$Id: Euler4.js 1398 2007-07-20 16:41:33Z josd $'

function prove(goal, maxNumberOfSteps) {
  var queue = [{rule:goal, src:0, ind:0, parent:null, env:{}, ground:[]}]
  if (typeof(evidence) == 'undefined') evidence = {}
  if (typeof(step) == 'undefined') step = 0
  while (queue.length > 0) {
    var c = queue.pop()
    if (typeof(trace) != 'undefined') document.writeln('POP QUEUE\n' + JSON.stringify(c.rule) + '\n')
    var g = aCopy(c.ground)
    step++
    if (maxNumberOfSteps != -1 && step >= maxNumberOfSteps) return ''
    if (c.ind >= c.rule.body.length) {
      if (c.parent == null) {
        for (var i = 0; i < c.rule.body.length; i++) {
          var t = evaluate(c.rule.body[i], c.env)
          if (typeof(evidence[t.pred]) == 'undefined') evidence[t.pred] = []
          evidence[t.pred].push({head:t, body:[{pred:'GND', args:c.ground}]})
        }
        continue
      }
      if (c.rule.body.length != 0) g.push({src:c.rule, env:c.env})
      var r = {rule:{head:c.parent.rule.head, body:c.parent.rule.body}, src:c.parent.src, ind:c.parent.ind, 
               parent:c.parent.parent != null ? new copy(c.parent.parent) : null, env:new copy(c.parent.env), ground:g}
      unify(c.rule.head, c.env, r.rule.body[r.ind], r.env, true)
      r.ind++
      queue.push(r)
      if (typeof(trace) != 'undefined') document.writeln('PUSH QUEUE\n' + JSON.stringify(r.rule) + '\n')
      continue
    }
    var t = c.rule.body[c.ind]
    var b = builtin(t, c)
    if (b == 1) {
      g.push({src:{head:evaluate(t, c.env), body:[]}, env:{}})
      var r = {rule:{head:c.rule.head, body:c.rule.body}, src:c.src, ind:c.ind, parent:c.parent, env:c.env, ground:g}
      r.ind++
      queue.push(r)
      if (typeof(trace) != 'undefined') document.writeln('PUSH QUEUE\n' + JSON.stringify(r.rule) + '\n')
      continue
    }
    else if (b == 0) continue
    if (cases[t.pred] == null) continue
    var src = 0
    for (var k = 0; k < cases[t.pred].length; k++) {
      var rl = cases[t.pred][k]
      src++
      var g = aCopy(c.ground)
      if (rl.body.length == 0) g.push({src:rl, env:{}})
      var r = {rule:rl, src:src, ind:0, parent:c, env:{}, ground:g}
      if (unify(t, c.env, rl.head, r.env, true)) {
        var ep = c  // euler path
        while (ep = ep.parent) if (ep.src == c.src && unify(ep.rule.head, ep.env, c.rule.head, c.env, false)) break
        if (ep == null) {
          queue.unshift(r)
          if (typeof(trace) != 'undefined') document.writeln('EULER PATH UNSHIFT QUEUE\n' + JSON.stringify(r.rule) + '\n')
        }
      }
    }
  }
}

function unify(s, senv, d, denv, f) {
  if (typeof(trace) != 'undefined') document.writeln('UNIFY\n' + JSON.stringify(s) + '\nWITH\n' + JSON.stringify(d) + '\n')
  if (isVar(s.pred)) {
    var sval = evaluate(s, senv)
    if (sval != null) return unify(sval, senv, d, denv, f)
    else return true
  }
  else if (isVar(d.pred)) {
    var dval = evaluate(d, denv)
    if (dval != null) return unify(s, senv, dval, denv, f)
    else {
      if (f != null) denv[d.pred] = evaluate(s, senv)
      return true
    }
  }
  else if (s.pred == d.pred && s.args.length == d.args.length) {
    for (var i = 0; i < s.args.length; i++) if (!unify(s.args[i], senv, d.args[i], denv, f)) return false
    return true
  }
  else {
    if (typeof(trace) != 'undefined') document.writeln('FAILED TO UNIFY\n' + JSON.stringify(s) + '\nWITH\n' + JSON.stringify(d) + '\n')
    return false
  }
}

function evaluate(t, env) {
  if (isVar(t.pred)) {
    var a = env[t.pred]
    if (a != null) return evaluate(a, env)
    else return null
  }
  else if (t.args.length == 0) return t
  else {
    var n = []
    for (var i = 0; i < t.args.length; i++) {
      var a = evaluate(t.args[i], env)
      if (a != null) n.push(a)
      else n.push({pred:t.args[i].pred, args:[]})
    }
    return {pred:t.pred, args:n}
  }
}

function isVar(s) { return s.charAt(0) == '?' }
function aCopy(t) { var a = new Array(); for (var i = 0; i < t.length; i++) a[i] = t[i]; return a }
function copy(t) { for (var i in t) this[i] = t[i] }
// Euler proof mechanism -- Jos De Roo
// $Id: Euler4.js 1398 2007-07-20 16:41:33Z josd $

function builtin(t, c) {
  if (t.pred == 'GND') return 1
  var t0 = evaluate(t.args[0], c.env)
  var t1 = evaluate(t.args[1], c.env)
  if (t.pred == 'log:equalTo') {
    if (t0 != null && t1 != null && t0.pred == t1.pred) return 1
    else return 0
  }
  else if (t.pred == 'log:notEqualTo') {
    if (t0 != null && t1 != null && t0.pred != t1.pred) return 1
    else return 0
  }
  else if (t.pred == 'log:semantics') {
    if (typeof(document) == 'undefined') {
      defineClass('euler.Support')
      eval('var s = ' + new Support().fromWeb(t0.pred));
    }
    else {
      var r = window.XMLHttpRequest?new XMLHttpRequest():new ActiveXObject('Msxml2.XMLHTTP')
      r.open('get', t0.pred, false)
      r.send(null)
      if (r.status == 200) eval('var s = ' + r.responseText)
    }
    if (unify(s, c.env, t.args[1], c.env, true)) return 1
    else return 0
  }
  else if (t.pred == 'math:absoluteValue') {
    if (t0 != null && !isVar(t0.pred)) {
      var a = Math.abs(parseFloat(t0.pred))
      if (unify({pred:a.toString(), args:[]}, c.env, t.args[1], c.env, true)) return 1
    }
    else return 0
  }
  else if (t.pred == 'math:cos') {
    if (t0 != null && !isVar(t0.pred)) {
      var a = Math.cos(parseFloat(t0.pred))
      if (unify({pred:a.toString(), args:[]}, c.env, t.args[1], c.env, true)) return 1
    }
    else if (t1 != null && !isVar(t1.pred)) {
      var a = Math.acos(parseFloat(t1.pred))
      if (unify({pred:a.toString(), args:[]}, c.env, t.args[0], c.env, true)) return 1
    }
    else return 0
  }
  else if (t.pred == 'math:degrees') {
    if (t0 != null && !isVar(t0.pred)) {
      var a = parseFloat(t0.pred) * 180 / Math.PI
      if (unify({pred:a.toString(), args:[]}, c.env, t.args[1], c.env, true)) return 1
    }
    else if (t1 != null && !isVar(t1.pred)) {
      var a = parseFloat(t0.pred) * Math.PI / 180
      if (unify({pred:a.toString(), args:[]}, c.env, t.args[0], c.env, true)) return 1
    }
    else return 0
  }
  else if (t.pred == 'math:equalTo') {
    if (t0 != null && t1 != null && parseFloat(t0.pred) == parseFloat(t1.pred)) return 1
    else return 0
  }
  else if (t.pred == 'math:greaterThan') {
    if (t0 != null && t1 != null && parseFloat(t0.pred) > parseFloat(t1.pred)) return 1
    else return 0
  }
  else if (t.pred == 'math:lessThan') {
    if (t0 != null && t1 != null && parseFloat(t0.pred) < parseFloat(t1.pred)) return 1
    else return 0
  }
  else if (t.pred == 'math:notEqualTo') {
    if (t0 != null && t1 != null && parseFloat(t0.pred) != parseFloat(t1.pred)) return 1
    else return 0
  }
  else if (t.pred == 'math:notLessThan') {
    if (t0 != null && t1 != null && parseFloat(t0.pred) >= parseFloat(t1.pred)) return 1
    else return 0
  }
  else if (t.pred == 'math:notGreaterThan') {
    if (t0 != null && t1 != null && parseFloat(t0.pred) <= parseFloat(t1.pred)) return 1
    else return 0
  }
  else if (t.pred == 'math:difference' && t0 != null) {
    var a = parseFloat(evaluate(t0.args[0], c.env).pred)
    for (var ti = t0.args[1]; ti.args.length != 0; ti = ti.args[1]) a -= parseFloat(evaluate(ti.args[0], c.env).pred)
    if (unify({pred:a.toString(), args:[]}, c.env, t.args[1], c.env, true)) return 1
    else return 0
  }
  else if (t.pred == 'math:exponentiation' && t0 != null) {
    var a = parseFloat(evaluate(t0.args[0], c.env).pred)
    for (var ti = t0.args[1]; ti.args.length != 0; ti = ti.args[1]) var a = Math.pow(a, parseFloat(evaluate(ti.args[0], c.env).pred))
    if (unify({pred:a.toString(), args:[]}, c.env, t.args[1], c.env, true)) return 1
    else return 0
  }
  else if (t.pred == 'math:integerQuotient' && t0 != null) {
    var a = parseFloat(evaluate(t0.args[0], c.env).pred)
    for (var ti = t0.args[1]; ti.args.length != 0; ti = ti.args[1]) a /= parseFloat(evaluate(ti.args[0], c.env).pred)
    if (unify({pred:Math.floor(a).toString(), args:[]}, c.env, t.args[1], c.env, true)) return 1
    else return 0
  }
  else if (t.pred == 'math:negation') {
    if (t0 != null && !isVar(t0.pred)) {
      var a = -parseFloat(t0.pred)
      if (unify({pred:a.toString(), args:[]}, c.env, t.args[1], c.env, true)) return 1
    }
    else if (t1 != null && !isVar(t1.pred)) {
      var a = -parseFloat(t1.pred)
      if (unify({pred:a.toString(), args:[]}, c.env, t.args[0], c.env, true)) return 1
    }
    else return 0
  }
  else if (t.pred == 'math:product' && t0 != null) {
    var a = parseFloat(evaluate(t0.args[0], c.env).pred)
    for (var ti = t0.args[1]; ti.args.length != 0; ti = ti.args[1]) a *= parseFloat(evaluate(ti.args[0], c.env).pred)
    if (unify({pred:a.toString(), args:[]}, c.env, t.args[1], c.env, true)) return 1
    else return 0
  }
  else if (t.pred == 'math:quotient' && t0 != null) {
    var a = parseFloat(evaluate(t0.args[0], c.env).pred)
    for (var ti = t0.args[1]; ti.args.length != 0; ti = ti.args[1]) a /= parseFloat(evaluate(ti.args[0], c.env).pred)
    if (unify({pred:a.toString(), args:[]}, c.env, t.args[1], c.env, true)) return 1
    else return 0
  }
  else if (t.pred == 'math:remainder' && t0 != null) {
    var a = parseFloat(evaluate(t0.args[0], c.env).pred)
    for (var ti = t0.args[1]; ti.args.length != 0; ti = ti.args[1]) a %= parseFloat(evaluate(ti.args[0], c.env).pred)
    if (unify({pred:a.toString(), args:[]}, c.env, t.args[1], c.env, true)) return 1
    else return 0
  }
  else if (t.pred == 'math:rounded') {
    if (t0 != null && !isVar(t0.pred)) {
      var a = Math.round(parseFloat(t0.pred))
      if (unify({pred:a.toString(), args:[]}, c.env, t.args[1], c.env, true)) return 1
    }
    else return 0
  }
  else if (t.pred == 'math:sin') {
    if (t0 != null && !isVar(t0.pred)) {
      var a = Math.sin(parseFloat(t0.pred))
      if (unify({pred:a.toString(), args:[]}, c.env, t.args[1], c.env, true)) return 1
    }
    else if (t1 != null && !isVar(t1.pred)) {
      var a = Math.asin(parseFloat(t1.pred))
      if (unify({pred:a.toString(), args:[]}, c.env, t.args[0], c.env, true)) return 1
    }
    else return 0
  }
  else if (t.pred == 'math:sum' && t0 != null) {
    var a = parseFloat(evaluate(t0.args[0], c.env).pred)
    for (var ti = t0.args[1]; ti.args.length != 0; ti = ti.args[1]) a += parseFloat(evaluate(ti.args[0], c.env).pred)
    if (unify({pred:a.toString(), args:[]}, c.env, t.args[1], c.env, true)) return 1
    else return 0
  }
  else if (t.pred == 'math:tan') {
    if (t0 != null && !isVar(t0.pred)) {
      var a = Math.tan(parseFloat(t0.pred))
      if (unify({pred:a.toString(), args:[]}, c.env, t.args[1], c.env, true)) return 1
    }
    else if (t1 != null && !isVar(t1.pred)) {
      var a = Math.atan(parseFloat(t1.pred))
      if (unify({pred:a.toString(), args:[]}, c.env, t.args[0], c.env, true)) return 1
    }
    else return 0
  }
  else if (t.pred == 'rdf:first' && t0 != null && t0.pred == '.' && t0.args.length != 0) {
    if (unify(t0.args[0], c.env, t.args[1], c.env, true)) return 1
    else return 0
  }
  else if (t.pred == 'rdf:rest' && t0 != null && t0.pred == '.' && t0.args.length != 0) {
    if (unify(t0.args[1], c.env, t.args[1], c.env, true)) return 1
    else return 0
  }
  else if (t.pred == 'a' && t1 != null && t1.pred == 'rdf:List' && t0 != null && t0.pred == '.') return 1
  else if (t.pred == 'a' && t1 != null && t1.pred == 'rdfs:Resource') return 1
  else return -1
}
/*
Copyright (c) 2005 JSON.org

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The Software shall be used for Good, not Evil.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/*
    The global object JSON contains two methods.

    JSON.stringify(value) takes a JavaScript value and produces a JSON text.
    The value must not be cyclical.

    JSON.parse(text) takes a JSON text and produces a JavaScript value. It will
    throw a 'JSONError' exception if there is an error.
*/
var JSON = {
    copyright: '(c)2005 JSON.org',
    license: 'http://www.crockford.com/JSON/license.html',
/*
    Stringify a JavaScript value, producing a JSON text.
*/
    stringify: function (v) {
        var a = [];
        var t = 0;

/*
    Emit a string.
*/
        function e(s) {
            a[a.length] = s;
        }

/*
    Convert a value.
*/
        function g(x, t) {
            var c, i, l, v;

            switch (typeof x) {
            case 'object':
                if (x) {
                    if (x instanceof Array) {
                        e('[');
                        t++;
                        l = a.length;
                        for (i = 0; i < x.length; i += 1) {
                            v = x[i];
                            if (typeof v != 'undefined' &&
                                    typeof v != 'function') {
                                if (l < a.length) e(',');
                                e('\n');
                                for (var k = 0; k < t; k++) e('  ');
                                g(v, t);
                            }
                        }
                        t--;
                        e(']');
                        return;
                    } else if (typeof x.valueOf == 'function') {
                        if (a[a.length-1] != ':') e(' ');
                        e('{');
                        t++;
                        l = a.length;
                        for (i in x) {
                            v = x[i];
                            if (typeof v != 'undefined' &&
                                    typeof v != 'function' &&
                                    (!v || typeof v != 'object' ||
                                        typeof v.valueOf == 'function')) {
                                if (l < a.length) e(',');
                                if (l < a.length && v.length != 0 || a[a.length-2] == ':') {
                                    e('\n');
                                    for (var k = 0; k < t; k++) e('  ');
                                }
                                g(i, t);
                                e(':');
                                g(v, t);
                            }
                        }
                        t--;
                        return e('}');
                    }
                }
                e('null');
                return;
            case 'number':
                e(isFinite(x) ? +x : 'null');
                return;
            case 'string':
                // trimming the string x
                while (x.substring(0, 1) == ' ') x = x.substring(1, x.length);
                while (x.substring(x.length-1, x.length) == ' ') x = x.substring(0, x.length-1);
                //
                l = x.length;
                e('"');
                for (i = 0; i < l; i += 1) {
                    c = x.charAt(i);
                    if (c >= ' ') {
                        if (c == '\\' || c == '"') {
                            e('\\');
                        }
                        e(c);
                    } else {
                        switch (c) {
                        case '\b':
                            e('\\b');
                            break;
                        case '\f':
                            e('\\f');
                            break;
                        case '\n':
                            e('\\n');
                            break;
                        case '\r':
                            e('\\r');
                            break;
                        case '\t':
                            e('\\t');
                            break;
                        default:
                            c = c.charCodeAt();
                            e('\\u00' + Math.floor(c / 16).toString(16) +
                                (c % 16).toString(16));
                        }
                    }
                }
                e('"');
                return;
            case 'boolean':
                e(String(x));
                return;
            default:
                e('null');
                return;
            }
        }
        g(v, t);
        return a.join('');
    },
/*
    Parse a JSON text, producing a JavaScript value.
*/
    parse: function (text) {
        return (/^(\s+|[,:{}\[\]]|"(\\["\\\/bfnrtu]|[^\x00-\x1f"\\]+)*"|-?\d+(\.\d*)?([eE][+-]?\d+)?|true|false|null)+$/.test(text)) &&
            eval('(' + text + ')');
    }
};
// Euler proof mechanism -- Jos De Roo
// $Id: Euler4.js 1398 2007-07-20 16:41:33Z josd $
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
