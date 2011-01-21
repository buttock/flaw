function nop() {}
function consoleLog() {
    return console.log.apply(console, arguments);
}
function consoleWarn() {
    return console.warn.apply(console, arguments);
}

var log = typeof(console) == 'undefined' ? nop : consoleLog;
var warn = typeof(console) == 'undefined' ? nop : consoleWarn;

function apply() {
    var f = arguments[0];
    var i, iLim;
    var args = [];
    for (i=1, iLim=arguments.length; i<iLim; i++) {
        args.push(arguments[i]);
    }
    return f.apply(this, args);
}

function compose(f,g) {
    return function (x) {
        return f(g(x));
    };
}

function listof() {
    var i, iLim;
    var args = [];
    for (i=0, iLim=arguments.length; i<iLim; i++) {
        args.push(arguments[i]);
    }
    return args;
}

function each(a,f) {
    var i, iLim;
    for (iLim=a.length, i=0; i<iLim; i++) {
        f(a[i]);
    }
}

function map (a, f) {
    var a2 = [];
    var i, iLim;
    for (iLim=a.length, i=0; i<iLim; i++) {
        a2.push(f(a[i]));
    }
    return a2;
}

function mapi (a, f) {
    var a2 = [];
    var i, iLim;
    for (iLim=a.length, i=0; i<iLim; i++) {
        a2.push(f(i, a[i]));
    }
    return a2;
}

function filter (a, f) {
    var a2 = [];
    var i, iLim;
    for (iLim=a.length, i=0; i<iLim; i++) {
        var x = a[i];
        if (f(x))
            a2.push(x);
    }
    return a2;
}

function contains(a, obj) {
  var i = a.length;
  while (i--) {
    if (a[i] === obj) {
      return true;
    }
  }
  return false;
}

function foldl(f,k,a) {
    var r = k;
    var i, iLim;
    for (iLim=a.length, i=0; i<iLim; i++) {
        r = f(r, a[i]);
    }
    return r;
}

function pair(x,y) { return [x,y]; }
function fst(pr) { return pr[0]; }
function snd(pr) { return pr[1]; }

function sum(a) { return foldl(add,0,a); }

function add(x,y) { return x+y; }

function isTrue (x) { return x ? true : false; }

function average(nn) {
    return (nn.length > 0)
           ? (sum(nn) / nn.length)
           : 0;
}

function stripPrefix(p, s) {
    var pn = p.length;
    if (pn > s.length) {
        return null;
    }
    return (s.slice(0,pn) == p) ? s.slice(pn) : null;
}

function array_index(a, p) {
    var i, iLim;
    for (iLim=a.length, i=0; i<iLim; i++) {
        if (p(a[i]))
            return i;
    }
    return -1;
}

function array_find(a, p) {
    var i = array_index(a, p);
    return (i >= 0) ? a[i] : null;
}

function array_cons(x, a) {
    return [x].concat(a);
}

function array_concat(a1, a2) {
    return a1.concat(a2);
}

function array_copy(a) { return a.slice(0); }

function object_copy(o) {
    var o2 = {};
    var k;
    for (k in o) {
        o2[k] = o[k];
    }
    return o2;
}

function concatenate(lists) {
    var r = [];
    var iLim, i;
    for (iLim=lists.length, i=0; i<iLim; i++) {
        r = r.concat(lists[i]);
    }
    return r;
}

function interspersef(makeSep, list) {
    var r = [];
    var iLim, i;
    for (iLim=list.length, i=0; i<iLim; i++) {
        if (i>0) { r.push(makeSep()); }
        r.push(list[i]);
    }
    return r;
}

function repeat(elt, n) {
    var a = [];
    while (n--) { a.push(elt); }
    return a;
}

function notBlank(s) { return s.length > 0; }

function memoize(thunk) {
    var it;
    return function () {
        if (thunk) {
            it = thunk();
            thunk = null;
        }
        return it;
    };
}

function compose(f,g) {
    return function (x) {
        return f(g(x));
    };
}

function negate(f) { return function (x) {
    return !(f(x));
}; }

function by(f,cmp) {
    return cmp ? function (x1,x2) { return cmp(f(x1), f(x2)); }
               : function (x1,x2) { return f(x1) - f(x2); }
}

function byReverse(f,cmp) {
    return cmp ? function (x1,x2) { return cmp(f(x2), f(x1)); }
               : function (x1,x2) { return f(x2) - f(x1); }
}

function strCmp(a, b) {
    return (b < a) - (a < b);
}

function numCmp(a, b) { return a - b; }

function numNullCmp(a, b) {
    return a == null
           ? (b == null ? 0 : 1)
           : (b == null ? -1 : numCmp(a,b));
}

function later(thunk) {
    setTimeout(thunk, 0);
}

function netstring(s) {
    return "" + s.length + ":" + s + ",";
}

