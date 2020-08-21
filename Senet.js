(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Main$Human = {$: 'Human'};
var $author$project$Board$Black = {$: 'Black'};
var $author$project$Board$White = {$: 'White'};
var $author$project$Board$Free = {$: 'Free'};
var $author$project$Board$Occ = function (a) {
	return {$: 'Occ', a: a};
};
var $author$project$BoardTree$Empty = {$: 'Empty'};
var $author$project$BoardTree$Leaf = F2(
	function (a, b) {
		return {$: 'Leaf', a: a, b: b};
	});
var $author$project$BoardTree$Parent = F3(
	function (a, b, c) {
		return {$: 'Parent', a: a, b: b, c: c};
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$Basics$pow = _Basics_pow;
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $author$project$BoardTree$fromList = function (xs) {
	var fromListHelper = F3(
		function (i, size, ys) {
			if (!ys.b) {
				return $author$project$BoardTree$Empty;
			} else {
				if (!ys.b.b) {
					var y = ys.a;
					return A2($author$project$BoardTree$Leaf, i, y);
				} else {
					var m = $elm$core$Basics$ceiling(
						A2($elm$core$Basics$logBase, 2, size));
					var halfn = A2($elm$core$Basics$pow, 2, m - 1);
					var j = i + halfn;
					var lastHalf = A2($elm$core$List$drop, halfn, ys);
					var firstHalf = A2($elm$core$List$take, halfn, ys);
					return A3(
						$author$project$BoardTree$Parent,
						j,
						A3(fromListHelper, i, halfn, firstHalf),
						A3(fromListHelper, j, size - halfn, lastHalf));
				}
			}
		});
	return A3(
		fromListHelper,
		0,
		$elm$core$List$length(xs),
		xs);
};
var $author$project$Board$initPawnCount = 7;
var $elm$core$Basics$modBy = _Basics_modBy;
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $author$project$Board$initBoard = function () {
	var sorter = function (n) {
		return (!A2($elm$core$Basics$modBy, 2, n)) ? $author$project$Board$White : $author$project$Board$Black;
	};
	var pawnRange = A2($elm$core$List$range, 0, (2 * $author$project$Board$initPawnCount) - 1);
	var pawns = A2(
		$elm$core$List$map,
		function (i) {
			return $author$project$Board$Occ(
				sorter(i));
		},
		pawnRange);
	var squareList = _Utils_ap(
		pawns,
		A2($elm$core$List$repeat, 30 - (2 * $author$project$Board$initPawnCount), $author$project$Board$Free));
	return $author$project$BoardTree$fromList(squareList);
}();
var $author$project$Board$initGame = {
	blackPawnCnt: $author$project$Board$initPawnCount,
	blackPawns: A2(
		$elm$core$List$map,
		function (i) {
			return {color: $author$project$Board$Black, square: (2 * i) + 1};
		},
		A2($elm$core$List$range, 0, $author$project$Board$initPawnCount - 1)),
	board: $author$project$Board$initBoard,
	turn: $author$project$Board$Black,
	whitePawnCnt: $author$project$Board$initPawnCount,
	whitePawns: A2(
		$elm$core$List$map,
		function (i) {
			return {color: $author$project$Board$White, square: 2 * i};
		},
		A2($elm$core$List$range, 0, $author$project$Board$initPawnCount - 1))
};
var $author$project$AI$Lazy = function (a) {
	return {$: 'Lazy', a: a};
};
var $author$project$AI$N = F2(
	function (a, b) {
		return {$: 'N', a: a, b: b};
	});
var $author$project$Board$NotDone = {$: 'NotDone'};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $author$project$Board$Won = function (a) {
	return {$: 'Won', a: a};
};
var $author$project$Board$isOver = function (gs) {
	return (!gs.whitePawnCnt) ? $author$project$Board$Won($author$project$Board$White) : ((!gs.blackPawnCnt) ? $author$project$Board$Won($author$project$Board$Black) : $author$project$Board$NotDone);
};
var $author$project$AI$Thunk = function (a) {
	return {$: 'Thunk', a: a};
};
var $author$project$AI$lazy = $author$project$AI$Thunk;
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$BoardTree$getElem = F2(
	function (i, t) {
		getElem:
		while (true) {
			switch (t.$) {
				case 'Empty':
					return $elm$core$Maybe$Nothing;
				case 'Leaf':
					var j = t.a;
					var x = t.b;
					return _Utils_eq(i, j) ? $elm$core$Maybe$Just(x) : $elm$core$Maybe$Nothing;
				default:
					var j = t.a;
					var t1 = t.b;
					var t2 = t.c;
					if (_Utils_cmp(i, j) < 0) {
						var $temp$i = i,
							$temp$t = t1;
						i = $temp$i;
						t = $temp$t;
						continue getElem;
					} else {
						var $temp$i = i,
							$temp$t = t2;
						i = $temp$i;
						t = $temp$t;
						continue getElem;
					}
			}
		}
	});
var $author$project$Logic$lastFreeBy = F2(
	function (sq, gs) {
		lastFreeBy:
		while (true) {
			var _v0 = A2($author$project$BoardTree$getElem, sq, gs.board);
			if (_v0.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				if (_v0.a.$ === 'Free') {
					var _v1 = _v0.a;
					return $elm$core$Maybe$Just(sq);
				} else {
					var $temp$sq = sq - 1,
						$temp$gs = gs;
					sq = $temp$sq;
					gs = $temp$gs;
					continue lastFreeBy;
				}
			}
		}
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Board$getSquareColor = function (p) {
	if (p.$ === 'Free') {
		return $elm$core$Maybe$Nothing;
	} else {
		var col = p.a;
		return $elm$core$Maybe$Just(col);
	}
};
var $author$project$BoardTree$setElem = F3(
	function (i, x, t) {
		switch (t.$) {
			case 'Empty':
				return $elm$core$Maybe$Nothing;
			case 'Leaf':
				var j = t.a;
				var y = t.b;
				return _Utils_eq(i, j) ? $elm$core$Maybe$Just(
					A2($author$project$BoardTree$Leaf, j, x)) : $elm$core$Maybe$Nothing;
			default:
				var j = t.a;
				var t1 = t.b;
				var t2 = t.c;
				return (_Utils_cmp(i, j) < 0) ? A2(
					$elm$core$Maybe$map,
					function (left) {
						return A2(
							$author$project$BoardTree$Parent(j),
							left,
							t2);
					},
					A3($author$project$BoardTree$setElem, i, x, t1)) : A2(
					$elm$core$Maybe$map,
					function (right) {
						return A2(
							$author$project$BoardTree$Parent(j),
							t1,
							right);
					},
					A3($author$project$BoardTree$setElem, i, x, t2));
		}
	});
var $author$project$BoardTree$swap = F3(
	function (i, j, t) {
		if (t.$ === 'Empty') {
			return $elm$core$Maybe$Nothing;
		} else {
			var setter = F3(
				function (k, mw, ms) {
					return A2(
						$elm$core$Maybe$andThen,
						function (s) {
							return A2(
								$elm$core$Maybe$andThen,
								function (w) {
									return A3($author$project$BoardTree$setElem, k, w, s);
								},
								mw);
						},
						ms);
				});
			var my = A2($author$project$BoardTree$getElem, j, t);
			var mx = A2($author$project$BoardTree$getElem, i, t);
			return A3(
				setter,
				i,
				my,
				A3(
					setter,
					j,
					mx,
					$elm$core$Maybe$Just(t)));
		}
	});
var $author$project$Logic$pawnSwapHelper = F3(
	function (pn, qn, board) {
		return A3($author$project$BoardTree$swap, pn, qn, board);
	});
var $author$project$Board$updateList = F3(
	function (col, f, gs) {
		if (col.$ === 'White') {
			return _Utils_update(
				gs,
				{
					whitePawns: f(gs.whitePawns)
				});
		} else {
			return _Utils_update(
				gs,
				{
					blackPawns: f(gs.blackPawns)
				});
		}
	});
var $author$project$Logic$pawnSwap = F3(
	function (pn, qn, gs) {
		var _v0 = A2($author$project$BoardTree$getElem, qn, gs.board);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var q = _v0.a;
			var replaceHelper = F3(
				function (oldv, newv, x) {
					return _Utils_eq(x.square, oldv) ? {color: x.color, square: newv} : x;
				});
			var replace = F2(
				function (old, _new) {
					return $elm$core$List$map(
						A2(replaceHelper, old, _new));
				});
			var maybeBoard = A3($author$project$Logic$pawnSwapHelper, pn, qn, gs.board);
			var js = function () {
				var _v2 = A2(
					$elm$core$Maybe$andThen,
					$author$project$Board$getSquareColor,
					A2($author$project$BoardTree$getElem, pn, gs.board));
				if (_v2.$ === 'Just') {
					var col = _v2.a;
					return A3(
						$author$project$Board$updateList,
						col,
						A2(replace, pn, qn),
						gs);
				} else {
					return gs;
				}
			}();
			var hs = A2(
				$elm$core$Maybe$map,
				function (b) {
					return _Utils_update(
						js,
						{board: b});
				},
				maybeBoard);
			if (q.$ === 'Free') {
				return hs;
			} else {
				var destCol = q.a;
				return A2(
					$elm$core$Maybe$map,
					function (game) {
						return A3(
							$author$project$Board$updateList,
							destCol,
							A2(replace, qn, pn),
							game);
					},
					hs);
			}
		}
	});
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$Logic$legalBySquareType = F2(
	function (square, m) {
		if (square.$ === 'Spec') {
			switch (square.a.$) {
				case 'Happy':
					var _v1 = square.a;
					return true;
				case 'Water':
					var _v2 = square.a;
					return true;
				case 'Truths':
					var _v3 = square.a;
					return m === 30;
				case 'Reatoum':
					var _v4 = square.a;
					return m === 30;
				default:
					var _v5 = square.a;
					return true;
			}
		} else {
			return true;
		}
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$core$Basics$not = _Basics_not;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Logic$clearSquare = F2(
	function (board, n) {
		return A2(
			$elm$core$Maybe$withDefault,
			board,
			A3($author$project$BoardTree$setElem, n, $author$project$Board$Free, board));
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $author$project$Logic$removePawn = F2(
	function (p, gs) {
		var removeListElem = function (x) {
			return $elm$core$List$filter(
				$elm$core$Basics$neq(x));
		};
		var newBoard = A2($author$project$Logic$clearSquare, gs.board, p.square);
		var _v0 = p.color;
		if (_v0.$ === 'White') {
			return _Utils_update(
				gs,
				{
					board: newBoard,
					whitePawnCnt: gs.whitePawnCnt - 1,
					whitePawns: A2(removeListElem, p, gs.whitePawns)
				});
		} else {
			return _Utils_update(
				gs,
				{
					blackPawnCnt: gs.blackPawnCnt - 1,
					blackPawns: A2(removeListElem, p, gs.blackPawns),
					board: newBoard
				});
		}
	});
var $author$project$Board$Happy = {$: 'Happy'};
var $author$project$Board$Horus = {$: 'Horus'};
var $author$project$Board$Reatoum = {$: 'Reatoum'};
var $author$project$Board$Rebirth = {$: 'Rebirth'};
var $author$project$Board$Reg = {$: 'Reg'};
var $author$project$Board$Spec = function (a) {
	return {$: 'Spec', a: a};
};
var $author$project$Board$Truths = {$: 'Truths'};
var $author$project$Board$Water = {$: 'Water'};
var $author$project$Board$squareType = function (n) {
	switch (n) {
		case 14:
			return $author$project$Board$Rebirth;
		case 25:
			return $author$project$Board$Spec($author$project$Board$Happy);
		case 26:
			return $author$project$Board$Spec($author$project$Board$Water);
		case 27:
			return $author$project$Board$Spec($author$project$Board$Truths);
		case 28:
			return $author$project$Board$Spec($author$project$Board$Reatoum);
		case 29:
			return $author$project$Board$Spec($author$project$Board$Horus);
		default:
			return $author$project$Board$Reg;
	}
};
var $author$project$Logic$playPawn = F3(
	function (p, roll, gs) {
		var n = p.square;
		var moveToRebirth = function (_v4) {
			return A2(
				$elm$core$Maybe$andThen,
				function (d) {
					return A3($author$project$Logic$pawnSwap, n, d, gs);
				},
				A2($author$project$Logic$lastFreeBy, 14, gs));
		};
		var m = n + roll;
		var moveTo = function (destState) {
			if (destState.$ === 'Free') {
				return A3($author$project$Logic$pawnSwap, n, m, gs);
			} else {
				var destCol = destState.a;
				return (!_Utils_eq(p.color, destCol)) ? A3($author$project$Logic$pawnSwap, n, m, gs) : $elm$core$Maybe$Nothing;
			}
		};
		var skippedHappiness = (n < 25) && (m > 25);
		var legalSqType = A2(
			$author$project$Logic$legalBySquareType,
			$author$project$Board$squareType(n),
			m);
		var attemptedLeave = m >= 30;
		if (!_Utils_eq(p.color, gs.turn)) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (!legalSqType) {
				return moveToRebirth(_Utils_Tuple0);
			} else {
				if (skippedHappiness) {
					return $elm$core$Maybe$Nothing;
				} else {
					if (attemptedLeave) {
						return $elm$core$Maybe$Just(
							A2($author$project$Logic$removePawn, p, gs));
					} else {
						var _v0 = A2($author$project$BoardTree$getElem, m, gs.board);
						if (_v0.$ === 'Nothing') {
							return $elm$core$Maybe$Nothing;
						} else {
							var dest = _v0.a;
							var _v1 = $author$project$Board$squareType(m);
							if ((_v1.$ === 'Spec') && (_v1.a.$ === 'Water')) {
								var _v2 = _v1.a;
								return moveToRebirth(_Utils_Tuple0);
							} else {
								return moveTo(dest);
							}
						}
					}
				}
			}
		}
	});
var $author$project$Logic$switchTurn = function (gs) {
	var _v0 = gs.turn;
	if (_v0.$ === 'White') {
		return _Utils_update(
			gs,
			{turn: $author$project$Board$Black});
	} else {
		return _Utils_update(
			gs,
			{turn: $author$project$Board$White});
	}
};
var $author$project$Logic$makeMove = F3(
	function (p, roll, gs) {
		var endOrContinueTurn = function (_v1) {
			return $author$project$Logic$switchTurn;
		};
		var checkSquare = F2(
			function (sq, js) {
				var getsq = function (g) {
					return A2($author$project$BoardTree$getElem, sq, g.board);
				};
				var _v0 = _Utils_Tuple2(
					getsq(gs),
					getsq(js));
				if ((((_v0.a.$ === 'Just') && (_v0.a.a.$ === 'Occ')) && (_v0.b.$ === 'Just')) && (_v0.b.a.$ === 'Occ')) {
					var c1 = _v0.a.a.a;
					var c2 = _v0.b.a.a;
					return (_Utils_eq(c1, c2) && _Utils_eq(c1, p.color)) ? A2(
						$elm$core$Maybe$withDefault,
						js,
						A2(
							$elm$core$Maybe$andThen,
							function (lf) {
								return A3($author$project$Logic$pawnSwap, sq, lf, js);
							},
							A2($author$project$Logic$lastFreeBy, 14, js))) : js;
				} else {
					return js;
				}
			});
		var sendBack = function (js) {
			return A2(
				checkSquare,
				29,
				A2(
					checkSquare,
					28,
					A2(checkSquare, 27, js)));
		};
		return A2(
			$elm$core$Maybe$map,
			endOrContinueTurn(_Utils_Tuple0),
			A2(
				$elm$core$Maybe$map,
				sendBack,
				A3($author$project$Logic$playPawn, p, roll, gs)));
	});
var $author$project$BoardTree$map = F2(
	function (f, t) {
		switch (t.$) {
			case 'Empty':
				return $author$project$BoardTree$Empty;
			case 'Leaf':
				var i = t.a;
				var x = t.b;
				return A2(
					$author$project$BoardTree$Leaf,
					i,
					f(x));
			default:
				var i = t.a;
				var left = t.b;
				var right = t.c;
				return A3(
					$author$project$BoardTree$Parent,
					i,
					A2($author$project$BoardTree$map, f, left),
					A2($author$project$BoardTree$map, f, right));
		}
	});
var $author$project$AI$findMoves = function (gs) {
	var rolls = $author$project$BoardTree$fromList(
		A2($elm$core$List$range, 1, 5));
	var pawnList = function () {
		var _v2 = gs.turn;
		if (_v2.$ === 'White') {
			return gs.whitePawns;
		} else {
			return gs.blackPawns;
		}
	}();
	var listDefault = F2(
		function (def, xs) {
			if (!xs.b) {
				return _List_fromArray(
					[def]);
			} else {
				return xs;
			}
		});
	return (!_Utils_eq(
		$author$project$Board$isOver(gs),
		$author$project$Board$NotDone)) ? $author$project$BoardTree$fromList(
		A2($elm$core$List$repeat, 5, _List_Nil)) : A2(
		$author$project$BoardTree$map,
		function (roll) {
			return A2(
				listDefault,
				_Utils_Tuple2(
					$elm$core$Maybe$Nothing,
					$author$project$AI$tsWrapper(
						$author$project$Logic$switchTurn(gs))),
				A2(
					$elm$core$List$concatMap,
					function (p) {
						return A2(
							$elm$core$Maybe$withDefault,
							_List_Nil,
							A2(
								$elm$core$Maybe$map,
								function (js) {
									return _List_fromArray(
										[
											_Utils_Tuple2(
											$elm$core$Maybe$Just(p),
											$author$project$AI$tsWrapper(js))
										]);
								},
								A3($author$project$Logic$makeMove, p, roll, gs)));
					},
					pawnList));
		},
		rolls);
};
var $author$project$AI$tsWrapper = function (gs) {
	return A2(
		$author$project$AI$N,
		gs,
		$author$project$AI$Lazy(
			$author$project$AI$lazy(
				function (_v0) {
					return $author$project$AI$findMoves(gs);
				})));
};
var $author$project$AI$newNode = function (gs) {
	return A2(
		$author$project$AI$N,
		gs,
		$author$project$AI$Lazy(
			$author$project$AI$lazy(
				function (_v0) {
					return $author$project$AI$findMoves(gs);
				})));
};
var $author$project$Main$initModel = {
	blackPlayer: $author$project$Main$Human,
	gs: $author$project$Board$initGame,
	highlighted: _List_Nil,
	queuedAI: false,
	roll: $elm$core$Maybe$Nothing,
	selected: $elm$core$Maybe$Nothing,
	skippedMove: false,
	ts: $author$project$AI$newNode($author$project$Board$initGame),
	whitePlayer: $author$project$Main$Human
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Main$init = function (_v0) {
	return _Utils_Tuple2($author$project$Main$initModel, $elm$core$Platform$Cmd$none);
};
var $author$project$Main$AIRand = {$: 'AIRand'};
var $author$project$Main$QueryAI = {$: 'QueryAI'};
var $author$project$Main$QueryRandMove = {$: 'QueryRandMove'};
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 'Every', a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {processes: processes, taggers: taggers};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 'Nothing') {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.processes;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(_Utils_Tuple0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.taggers);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Main$subscriptions = function (model) {
	var currPlayer = function () {
		var _v1 = model.gs.turn;
		if (_v1.$ === 'White') {
			return model.whitePlayer;
		} else {
			return model.blackPlayer;
		}
	}();
	return model.queuedAI ? A2(
		$elm$time$Time$every,
		function () {
			switch (currPlayer.$) {
				case 'Human':
					return 10;
				case 'AIRand':
					return 150;
				case 'AILast':
					return 150;
				case 'AIFast':
					return 150;
				case 'AIMed':
					return 100;
				default:
					return 50;
			}
		}(),
		_Utils_eq(currPlayer, $author$project$Main$AIRand) ? $elm$core$Basics$always($author$project$Main$QueryRandMove) : $elm$core$Basics$always($author$project$Main$QueryAI)) : $elm$core$Platform$Sub$none;
};
var $author$project$Main$Click = function (a) {
	return {$: 'Click', a: a};
};
var $author$project$Main$GetRoll = function (a) {
	return {$: 'GetRoll', a: a};
};
var $author$project$Main$NewTurn = {$: 'NewTurn'};
var $author$project$Main$Noop = {$: 'Noop'};
var $author$project$Main$Play = {$: 'Play'};
var $author$project$Main$PlayRandMove = function (a) {
	return {$: 'PlayRandMove', a: a};
};
var $author$project$Main$QueryRoll = {$: 'QueryRoll'};
var $author$project$Main$QueueAI = {$: 'QueueAI'};
var $author$project$Main$Skip = {$: 'Skip'};
var $author$project$AI$Eval = function (a) {
	return {$: 'Eval', a: a};
};
var $author$project$AI$force = function (_v0) {
	var f = _v0.a;
	return f(_Utils_Tuple0);
};
var $author$project$AI$evalTMT = function (tmt) {
	if (tmt.$ === 'Lazy') {
		var tma = tmt.a;
		return $author$project$AI$force(tma);
	} else {
		var rma = tmt.a;
		return rma;
	}
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $author$project$AI$gsLeafVal = F2(
	function (col, gs) {
		var sign = function (sq) {
			if (sq.$ === 'Free') {
				return 0;
			} else {
				if (sq.a.$ === 'Black') {
					var _v10 = sq.a;
					return -1;
				} else {
					var _v11 = sq.a;
					return 1;
				}
			}
		};
		var rebirthSquare = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($author$project$Logic$lastFreeBy, 14, gs));
		var pawnBounty = 20;
		var valf = F2(
			function (i, sq) {
				var _v4 = $author$project$Board$squareType(i);
				_v4$4:
				while (true) {
					if (_v4.$ === 'Spec') {
						switch (_v4.a.$) {
							case 'Happy':
								var _v5 = _v4.a;
								return sign(sq) * (-3.0);
							case 'Horus':
								var _v6 = _v4.a;
								return sign(sq) * (-pawnBounty);
							case 'Reatoum':
								var _v7 = _v4.a;
								return sign(sq) * ((0.75 * (30 - rebirthSquare)) - (0.25 * pawnBounty));
							case 'Truths':
								var _v8 = _v4.a;
								return sign(sq) * ((0.625 * (30 - rebirthSquare)) - (0.375 * pawnBounty));
							default:
								break _v4$4;
						}
					} else {
						break _v4$4;
					}
				}
				return (i !== 24) ? (sign(sq) * (30 - i)) : (sign(sq) * 8.5);
			});
		var pawnAdv = pawnBounty * (gs.whitePawnCnt - gs.blackPawnCnt);
		var colCorrection = function () {
			if (col.$ === 'Black') {
				return 1;
			} else {
				return -1;
			}
		}();
		return colCorrection * function () {
			var _v0 = $author$project$Board$isOver(gs);
			if (_v0.$ === 'NotDone') {
				return A3(
					$elm$core$List$foldl,
					$elm$core$Basics$add,
					0.0,
					A2(
						$elm$core$List$map,
						function (i) {
							return A2(
								$elm$core$Maybe$withDefault,
								0.0,
								A2(
									$elm$core$Maybe$map,
									function (sq) {
										return A2(valf, i, sq) + pawnAdv;
									},
									A2($author$project$BoardTree$getElem, i, gs.board)));
						},
						A2($elm$core$List$range, 0, 29)));
			} else {
				if (_v0.a.$ === 'White') {
					var _v1 = _v0.a;
					return -1000.0;
				} else {
					var _v2 = _v0.a;
					return 1000.0;
				}
			}
		}();
	});
var $author$project$AI$mlistConcat = function (xs) {
	return A2(
		$elm$core$List$concatMap,
		function (mx) {
			if (mx.$ === 'Nothing') {
				return _List_Nil;
			} else {
				var x = mx.a;
				return _List_fromArray(
					[x]);
			}
		},
		xs);
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $author$project$AI$mmArg = F3(
	function (isMax, f, ys) {
		var minormax = isMax ? $elm$core$Basics$max : $elm$core$Basics$min;
		var mmaHelper = F3(
			function (acc, facc, xs) {
				mmaHelper:
				while (true) {
					if (xs.b) {
						var x = xs.a;
						var rest = xs.b;
						var fx = f(x);
						if (_Utils_eq(
							fx,
							A2(minormax, facc, fx))) {
							var $temp$acc = x,
								$temp$facc = fx,
								$temp$xs = rest;
							acc = $temp$acc;
							facc = $temp$facc;
							xs = $temp$xs;
							continue mmaHelper;
						} else {
							var $temp$acc = acc,
								$temp$facc = facc,
								$temp$xs = rest;
							acc = $temp$acc;
							facc = $temp$facc;
							xs = $temp$xs;
							continue mmaHelper;
						}
					} else {
						return acc;
					}
				}
			});
		if (!ys.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var y = ys.a;
			var rest = ys.b;
			return $elm$core$Maybe$Just(
				A3(
					mmaHelper,
					y,
					f(y),
					rest));
		}
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$AI$evalState = F3(
	function (col, ply, _v0) {
		var gs = _v0.a;
		var tmt = _v0.b;
		if (!ply) {
			return $elm$core$Maybe$Just(
				_Utils_Tuple2(
					A2($author$project$AI$gsLeafVal, col, gs),
					A2($author$project$AI$N, gs, tmt)));
		} else {
			var mmArgCol = $author$project$AI$mmArg(
				_Utils_eq(col, gs.turn));
			var ma = $author$project$AI$evalTMT(tmt);
			var expect = function () {
				var expHelper = F3(
					function (acc, weights, moves) {
						var _v4 = _Utils_Tuple2(weights, moves);
						_v4$2:
						while (true) {
							if (_v4.a.b) {
								if (_v4.b.b) {
									var _v5 = _v4.a;
									var w = _v5.a;
									var wrest = _v5.b;
									var _v6 = _v4.b;
									var m = _v6.a;
									var mrest = _v6.b;
									return A2(
										$elm$core$Maybe$andThen,
										function (_v7) {
											var val = _v7.a;
											return A3(expHelper, acc + (w * val), wrest, mrest);
										},
										m);
								} else {
									break _v4$2;
								}
							} else {
								if (!_v4.b.b) {
									return $elm$core$Maybe$Just(acc);
								} else {
									break _v4$2;
								}
							}
						}
						return $elm$core$Maybe$Nothing;
					});
				return A2(
					expHelper,
					0,
					_List_fromArray(
						[1 / 16, 1 / 4, 3 / 8, 1 / 4, 1 / 16]));
			}();
			var _v1 = $author$project$Board$isOver(gs);
			if (_v1.$ === 'Won') {
				var winner = _v1.a;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(
						_Utils_eq(winner, col) ? 255 : (-255),
						A2(
							$author$project$AI$N,
							gs,
							$author$project$AI$Eval(ma))));
			} else {
				var preList = A2(
					$elm$core$List$map,
					function (roll) {
						return A2(
							$elm$core$Maybe$andThen,
							function (list) {
								var tupList = $author$project$AI$mlistConcat(
									A2(
										$elm$core$List$map,
										function (_v2) {
											var mp = _v2.a;
											var ts = _v2.b;
											return A2(
												$elm$core$Maybe$map,
												function (_v3) {
													var moveVal = _v3.a;
													var newTS = _v3.b;
													return _Utils_Tuple2(
														moveVal,
														_Utils_Tuple2(mp, newTS));
												},
												A3($author$project$AI$evalState, col, ply - 1, ts));
										},
										list));
								var movesForRoll = A2($elm$core$List$map, $elm$core$Tuple$second, tupList);
								return A2(
									$elm$core$Maybe$map,
									function (mval) {
										return _Utils_Tuple2(mval, movesForRoll);
									},
									A2(mmArgCol, $elm$core$Tuple$first, tupList));
							},
							A2($author$project$BoardTree$getElem, roll - 1, ma));
					},
					A2($elm$core$List$range, 1, 5));
				var updatedMA = $author$project$BoardTree$fromList(
					$author$project$AI$mlistConcat(
						A2(
							$elm$core$List$map,
							$elm$core$Maybe$map($elm$core$Tuple$second),
							preList)));
				return A2(
					$elm$core$Maybe$map,
					function (vf) {
						return _Utils_Tuple2(
							vf,
							A2(
								$author$project$AI$N,
								gs,
								$author$project$AI$Eval(updatedMA)));
					},
					expect(
						A2(
							$elm$core$List$map,
							$elm$core$Maybe$map($elm$core$Tuple$first),
							preList)));
			}
		}
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $author$project$AI$evalRolledState = F4(
	function (col, ply, roll, _v0) {
		var gs = _v0.a;
		var tmt = _v0.b;
		if (!ply) {
			return $elm$core$Maybe$Just(
				_Utils_Tuple3(
					A2($author$project$AI$gsLeafVal, col, gs),
					$elm$core$Maybe$Nothing,
					A2($author$project$AI$N, gs, tmt)));
		} else {
			var mmCol = A2(
				$author$project$AI$mmArg,
				_Utils_eq(col, gs.turn),
				function (w) {
					return w;
				});
			var mmArgColFirst = A2(
				$author$project$AI$mmArg,
				_Utils_eq(col, gs.turn),
				$elm$core$Tuple$first);
			var ma = $author$project$AI$evalTMT(tmt);
			var _v1 = $author$project$Board$isOver(gs);
			if (_v1.$ === 'Won') {
				var winner = _v1.a;
				return $elm$core$Maybe$Just(
					_Utils_Tuple3(
						_Utils_eq(winner, col) ? 255 : (-255),
						$elm$core$Maybe$Nothing,
						A2(
							$author$project$AI$N,
							gs,
							$author$project$AI$Eval(ma))));
			} else {
				return A2(
					$elm$core$Maybe$andThen,
					function (list) {
						var preList = A2(
							$elm$core$List$filterMap,
							function (_v4) {
								var mp = _v4.a;
								var ts = _v4.b;
								return A2(
									$elm$core$Maybe$map,
									function (_v5) {
										var val = _v5.a;
										var newTS = _v5.b;
										return _Utils_Tuple2(
											val,
											_Utils_Tuple2(mp, newTS));
									},
									A3($author$project$AI$evalState, col, ply - 1, ts));
							},
							list);
						var updatedMoves = A2($elm$core$List$map, $elm$core$Tuple$second, preList);
						var optionDescription = A2(
							$elm$core$List$map,
							function (_v2) {
								var v = _v2.a;
								var _v3 = _v2.b;
								var mp = _v3.a;
								var newts = _v3.b;
								return _Utils_Tuple2(mp, v);
							},
							preList);
						var mmElem = mmArgColFirst(preList);
						return A2(
							$elm$core$Maybe$map,
							function (elem) {
								return _Utils_Tuple3(
									elem.a,
									elem.b.a,
									function () {
										var newestTS = A2(
											$author$project$AI$N,
											gs,
											$author$project$AI$Eval(
												A2(
													$elm$core$Maybe$withDefault,
													ma,
													A3($author$project$BoardTree$setElem, roll - 1, updatedMoves, ma))));
										return newestTS;
									}());
							},
							mmElem);
					},
					A2($author$project$BoardTree$getElem, roll - 1, ma));
			}
		}
	});
var $author$project$AI$aiChooseMove = F4(
	function (col, ply, roll, ts) {
		var res = A4($author$project$AI$evalRolledState, col, ply, roll, ts);
		return _Utils_Tuple2(
			A2(
				$elm$core$Maybe$andThen,
				function (_v0) {
					var mb = _v0.b;
					return mb;
				},
				res),
			A2(
				$elm$core$Maybe$withDefault,
				ts,
				A2(
					$elm$core$Maybe$map,
					function (_v1) {
						var newTS = _v1.c;
						return newTS;
					},
					res)));
	});
var $author$project$Main$clearRoll = function (model) {
	return _Utils_update(
		model,
		{roll: $elm$core$Maybe$Nothing});
};
var $elm$random$Random$Generate = function (a) {
	return {$: 'Generate', a: a};
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0.a;
		return $elm$random$Random$Generate(
			A2($elm$random$Random$map, func, generator));
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			$elm$random$Random$Generate(
				A2($elm$random$Random$map, tagger, generator)));
	});
var $author$project$Main$getPlayerType = function (model) {
	var _v0 = model.gs.turn;
	if (_v0.$ === 'White') {
		return model.whitePlayer;
	} else {
		return model.blackPlayer;
	}
};
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Board$getPawn = F2(
	function (i, gs) {
		var squarePawn = function (sq) {
			if (sq.$ === 'Free') {
				return $elm$core$Maybe$Nothing;
			} else {
				var c = sq.a;
				return $elm$core$Maybe$Just(
					{color: c, square: i});
			}
		};
		return A2(
			$elm$core$Maybe$andThen,
			squarePawn,
			A2($author$project$BoardTree$getElem, i, gs.board));
	});
var $author$project$Logic$isLegal = F3(
	function (board, p, roll) {
		var n = p.square;
		var m = n + roll;
		var skippedHappiness = (n < 25) && (m > 25);
		var legalSqType = A2(
			$author$project$Logic$legalBySquareType,
			$author$project$Board$squareType(n),
			m);
		var attemptedLeave = m >= 30;
		if (!legalSqType) {
			return false;
		} else {
			if (skippedHappiness) {
				return false;
			} else {
				if (attemptedLeave) {
					return true;
				} else {
					var _v0 = A2($author$project$BoardTree$getElem, m, board);
					if (_v0.$ === 'Nothing') {
						return false;
					} else {
						if (_v0.a.$ === 'Free') {
							var _v1 = _v0.a;
							return true;
						} else {
							var destCol = _v0.a.a;
							return !_Utils_eq(p.color, destCol);
						}
					}
				}
			}
		}
	});
var $author$project$Main$isPlayerAI = function (player) {
	if (player.$ === 'Human') {
		return false;
	} else {
		return true;
	}
};
var $author$project$Logic$legalMoves = F2(
	function (gs, roll) {
		var legals = $elm$core$List$filter(
			function (p) {
				return A3($author$project$Logic$isLegal, gs.board, p, roll);
			});
		var _v0 = gs.turn;
		if (_v0.$ === 'Black') {
			return legals(gs.blackPawns);
		} else {
			return legals(gs.whitePawns);
		}
	});
var $author$project$Main$pawnSendBack = function (model) {
	var dest = A2($author$project$Logic$lastFreeBy, 14, model.gs);
	var trySendFrom = F2(
		function (sq, js) {
			return A2(
				$elm$core$Maybe$withDefault,
				js,
				A2(
					$elm$core$Maybe$andThen,
					function (lf) {
						var _v0 = A2($author$project$BoardTree$getElem, sq, js.board);
						if ((_v0.$ === 'Just') && (_v0.a.$ === 'Occ')) {
							var col = _v0.a.a;
							return _Utils_eq(col, js.turn) ? A3($author$project$Logic$pawnSwap, sq, lf, js) : $elm$core$Maybe$Nothing;
						} else {
							return $elm$core$Maybe$Nothing;
						}
					},
					dest));
		});
	return _Utils_update(
		model,
		{
			gs: A2(
				trySendFrom,
				29,
				A2(
					trySendFrom,
					28,
					A2(trySendFrom, 27, model.gs)))
		});
};
var $author$project$Main$highlightPieces = function (model) {
	var currPlayer = function () {
		var _v2 = model.gs.turn;
		if (_v2.$ === 'White') {
			return model.whitePlayer;
		} else {
			return model.blackPlayer;
		}
	}();
	return A2(
		$elm$core$Maybe$withDefault,
		_Utils_update(
			model,
			{highlighted: _List_Nil}),
		function () {
			var _v0 = model.selected;
			if (_v0.$ === 'Nothing') {
				return A2(
					$elm$core$Maybe$map,
					function (roll) {
						if ($author$project$Main$isPlayerAI(currPlayer)) {
							return model;
						} else {
							var _v1 = A2(
								$elm$core$List$map,
								function (p) {
									return p.square;
								},
								A2($author$project$Logic$legalMoves, model.gs, roll));
							if (!_v1.b) {
								var newModel = $author$project$Main$pawnSendBack(model);
								var sm = _Utils_update(
									newModel,
									{highlighted: _List_Nil, skippedMove: true});
								return _Utils_eq(newModel, model) ? sm : $author$project$Main$highlightPieces(sm);
							} else {
								var moves = _v1;
								return _Utils_update(
									model,
									{highlighted: moves});
							}
						}
					},
					model.roll);
			} else {
				var numpawn = _v0.a;
				return A2(
					$elm$core$Maybe$andThen,
					function (roll) {
						return A2(
							$elm$core$Maybe$map,
							function (spawn) {
								return A3($author$project$Logic$isLegal, model.gs.board, spawn, roll) ? _Utils_update(
									model,
									{
										highlighted: _List_fromArray(
											[spawn.square, spawn.square + roll])
									}) : _Utils_update(
									model,
									{
										highlighted: _List_fromArray(
											[spawn.square])
									});
							},
							A2($author$project$Board$getPawn, numpawn, model.gs));
					},
					model.roll);
			}
		}());
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
				var lo = _v0.a;
				var hi = _v0.b;
				var range = (hi - lo) + 1;
				if (!((range - 1) & range)) {
					return _Utils_Tuple2(
						(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
						$elm$random$Random$next(seed0));
				} else {
					var threshhold = (((-range) >>> 0) % range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var x = $elm$random$Random$peel(seed);
							var seedN = $elm$random$Random$next(seed);
							if (_Utils_cmp(x, threshhold) < 0) {
								var $temp$seed = seedN;
								seed = $temp$seed;
								continue accountForBias;
							} else {
								return _Utils_Tuple2((x % range) + lo, seedN);
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var $author$project$Logic$skipTurn = function (gs) {
	var dest = A2($author$project$Logic$lastFreeBy, 14, gs);
	var trySendFrom = F2(
		function (sq, js) {
			return A2(
				$elm$core$Maybe$withDefault,
				js,
				A2(
					$elm$core$Maybe$andThen,
					function (lf) {
						var _v0 = A2($author$project$BoardTree$getElem, sq, js.board);
						if ((_v0.$ === 'Just') && (_v0.a.$ === 'Occ')) {
							var col = _v0.a.a;
							return _Utils_eq(col, js.turn) ? A3($author$project$Logic$pawnSwap, sq, lf, js) : $elm$core$Maybe$Nothing;
						} else {
							return $elm$core$Maybe$Nothing;
						}
					},
					dest));
		});
	return $author$project$Logic$switchTurn(
		A2(
			trySendFrom,
			29,
			A2(
				trySendFrom,
				28,
				A2(trySendFrom, 27, gs))));
};
var $author$project$AI$lastPawnAI = F3(
	function (col, roll, _v0) {
		var gs = _v0.a;
		var tmt = _v0.b;
		var mgt = F2(
			function (mx, my) {
				var _v7 = _Utils_Tuple2(mx, my);
				if (_v7.a.$ === 'Just') {
					if (_v7.b.$ === 'Just') {
						var x = _v7.a.a;
						var y = _v7.b.a;
						return _Utils_cmp(x.square, y.square) > 0;
					} else {
						var x = _v7.a.a;
						var _v8 = _v7.b;
						return true;
					}
				} else {
					if (_v7.b.$ === 'Just') {
						var _v9 = _v7.a;
						var y = _v7.b.a;
						return false;
					} else {
						var _v10 = _v7.a;
						var _v11 = _v7.b;
						return false;
					}
				}
			});
		var ma = $author$project$AI$evalTMT(tmt);
		var lastPawnHelper = F2(
			function (_v1, xs) {
				lastPawnHelper:
				while (true) {
					var max = _v1.a;
					var maxTS = _v1.b;
					if (xs.b) {
						var _v3 = xs.a;
						var mx = _v3.a;
						var xTS = _v3.b;
						var rest = xs.b;
						if (A2(mgt, mx, max)) {
							var $temp$_v1 = _Utils_Tuple2(mx, xTS),
								$temp$xs = rest;
							_v1 = $temp$_v1;
							xs = $temp$xs;
							continue lastPawnHelper;
						} else {
							var $temp$_v1 = _Utils_Tuple2(max, maxTS),
								$temp$xs = rest;
							_v1 = $temp$_v1;
							xs = $temp$xs;
							continue lastPawnHelper;
						}
					} else {
						return _Utils_Tuple2(max, maxTS);
					}
				}
			});
		var lastPawn = function (xs) {
			if (xs.b) {
				if (!xs.b.b) {
					var _v5 = xs.a;
					var mx = _v5.a;
					var newTS = _v5.b;
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(mx, newTS));
				} else {
					var _v6 = xs.a;
					var mx = _v6.a;
					var newTS = _v6.b;
					var rest = xs.b;
					return $elm$core$Maybe$Just(
						A2(
							lastPawnHelper,
							_Utils_Tuple2(mx, newTS),
							rest));
				}
			} else {
				return $elm$core$Maybe$Nothing;
			}
		};
		return A2(
			$elm$core$Maybe$withDefault,
			_Utils_Tuple2(
				$elm$core$Maybe$Nothing,
				$author$project$AI$newNode(
					$author$project$Logic$skipTurn(gs))),
			A2(
				$elm$core$Maybe$andThen,
				function (moveList) {
					return lastPawn(moveList);
				},
				A2($author$project$BoardTree$getElem, roll - 1, ma)));
	});
var $author$project$Main$opChain = F2(
	function (op, _v0) {
		var model = _v0.a;
		var cmsg = _v0.b;
		var _v1 = op(model);
		var newModel = _v1.a;
		var newMsg = _v1.b;
		return _Utils_Tuple2(
			newModel,
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[cmsg, newMsg])));
	});
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $author$project$Main$resetSticks = _Platform_outgoingPort(
	'resetSticks',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$random$Random$float = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var seed1 = $elm$random$Random$next(seed0);
				var range = $elm$core$Basics$abs(b - a);
				var n1 = $elm$random$Random$peel(seed1);
				var n0 = $elm$random$Random$peel(seed0);
				var lo = (134217727 & n1) * 1.0;
				var hi = (67108863 & n0) * 1.0;
				var val = ((hi * 134217728.0) + lo) / 9007199254740992.0;
				var scaled = (val * range) + a;
				return _Utils_Tuple2(
					scaled,
					$elm$random$Random$next(seed1));
			});
	});
var $elm$random$Random$getByWeight = F3(
	function (_v0, others, countdown) {
		getByWeight:
		while (true) {
			var weight = _v0.a;
			var value = _v0.b;
			if (!others.b) {
				return value;
			} else {
				var second = others.a;
				var otherOthers = others.b;
				if (_Utils_cmp(
					countdown,
					$elm$core$Basics$abs(weight)) < 1) {
					return value;
				} else {
					var $temp$_v0 = second,
						$temp$others = otherOthers,
						$temp$countdown = countdown - $elm$core$Basics$abs(weight);
					_v0 = $temp$_v0;
					others = $temp$others;
					countdown = $temp$countdown;
					continue getByWeight;
				}
			}
		}
	});
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $elm$random$Random$weighted = F2(
	function (first, others) {
		var normalize = function (_v0) {
			var weight = _v0.a;
			return $elm$core$Basics$abs(weight);
		};
		var total = normalize(first) + $elm$core$List$sum(
			A2($elm$core$List$map, normalize, others));
		return A2(
			$elm$random$Random$map,
			A2($elm$random$Random$getByWeight, first, others),
			A2($elm$random$Random$float, 0, total));
	});
var $author$project$Main$rollGenerator = A2(
	$elm$random$Random$weighted,
	_Utils_Tuple2(4, 1),
	_List_fromArray(
		[
			_Utils_Tuple2(6, 2),
			_Utils_Tuple2(4, 3),
			_Utils_Tuple2(1, 4),
			_Utils_Tuple2(1, 5)
		]));
var $author$project$Main$selectPiece = F2(
	function (n, model) {
		return _Utils_update(
			model,
			{
				selected: $elm$core$Maybe$Just(n)
			});
	});
var $author$project$Main$setPlayerType = F3(
	function (col, ptype, model) {
		if (col.$ === 'White') {
			return _Utils_update(
				model,
				{whitePlayer: ptype});
		} else {
			return _Utils_update(
				model,
				{blackPlayer: ptype});
		}
	});
var $author$project$Main$setRoll = F2(
	function (i, model) {
		return _Utils_update(
			model,
			{
				roll: $elm$core$Maybe$Just(i)
			});
	});
var $author$project$Main$setTS = F2(
	function (ts, model) {
		return _Utils_update(
			model,
			{ts: ts});
	});
var $author$project$Main$tryPlay = F2(
	function (n, model) {
		return A2(
			$elm$core$Maybe$andThen,
			function (r) {
				return A2(
					$elm$core$Maybe$andThen,
					function (p) {
						return A2(
							$elm$core$Maybe$map,
							function (js) {
								return $author$project$Main$clearRoll(
									_Utils_update(
										model,
										{gs: js}));
							},
							A3($author$project$Logic$makeMove, p, r, model.gs));
					},
					A2($author$project$Board$getPawn, n, model.gs));
			},
			model.roll);
	});
var $author$project$Main$unselectPiece = function (model) {
	return _Utils_update(
		model,
		{selected: $elm$core$Maybe$Nothing});
};
var $author$project$Main$update = F2(
	function (msg, model) {
		update:
		while (true) {
			switch (msg.$) {
				case 'NewTurn':
					if (!_Utils_eq(
						$author$project$Board$isOver(model.gs),
						$author$project$Board$NotDone)) {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					} else {
						var _v1 = $author$project$Main$getPlayerType(model);
						if (_v1.$ === 'Human') {
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
						} else {
							var $temp$msg = $author$project$Main$QueryRoll,
								$temp$model = model;
							msg = $temp$msg;
							model = $temp$model;
							continue update;
						}
					}
				case 'QueryRoll':
					var _v2 = model.roll;
					if (_v2.$ === 'Nothing') {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{skippedMove: false}),
							A2($elm$random$Random$generate, $author$project$Main$GetRoll, $author$project$Main$rollGenerator));
					} else {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					}
				case 'GetRoll':
					var i = msg.a;
					var newModel = $author$project$Main$highlightPieces(
						A2($author$project$Main$setRoll, i, model));
					var _v3 = $author$project$Main$getPlayerType(newModel);
					if (_v3.$ === 'Human') {
						return _Utils_Tuple2(
							newModel,
							$author$project$Main$resetSticks(_Utils_Tuple0));
					} else {
						var $temp$msg = $author$project$Main$QueueAI,
							$temp$model = newModel;
						msg = $temp$msg;
						model = $temp$model;
						continue update;
					}
				case 'Click':
					var n = msg.a;
					var unselect = function (_v6) {
						return _Utils_Tuple2(
							$author$project$Main$highlightPieces(
								$author$project$Main$unselectPiece(model)),
							$elm$core$Platform$Cmd$none);
					};
					var play = function (m) {
						return A2(
							$author$project$Main$opChain,
							$author$project$Main$update($author$project$Main$NewTurn),
							_Utils_Tuple2(
								A2(
									$elm$core$Maybe$withDefault,
									model,
									A2(
										$author$project$Main$tryPlay,
										m,
										$author$project$Main$unselectPiece(model))),
								$elm$core$Platform$Cmd$none));
					};
					var currTurn = model.gs.turn;
					var select = function (_v5) {
						var _v4 = A2($author$project$BoardTree$getElem, n, model.gs.board);
						if ((_v4.$ === 'Just') && (_v4.a.$ === 'Occ')) {
							var col = _v4.a.a;
							return _Utils_eq(col, currTurn) ? _Utils_Tuple2(
								$author$project$Main$highlightPieces(
									A2($author$project$Main$selectPiece, n, model)),
								$elm$core$Platform$Cmd$none) : unselect(_Utils_Tuple0);
						} else {
							return unselect(_Utils_Tuple0);
						}
					};
					var checkSquare = function (m) {
						return (!_Utils_eq(n, m)) ? select(_Utils_Tuple0) : unselect(_Utils_Tuple0);
					};
					return A2(
						$elm$core$Maybe$withDefault,
						select(_Utils_Tuple0),
						A2(
							$elm$core$Maybe$map,
							function (m) {
								return A2(
									$elm$core$Maybe$withDefault,
									checkSquare(m),
									A2(
										$elm$core$Maybe$map,
										function (r) {
											return (_Utils_eq(n, m + r) || (n >= 30)) ? ((!_Utils_eq(
												$elm$core$Maybe$Just(
													$author$project$Board$Occ(model.gs.turn)),
												A2($author$project$BoardTree$getElem, n, model.gs.board))) ? play(m) : select(_Utils_Tuple0)) : checkSquare(m);
										},
										model.roll));
							},
							model.selected));
				case 'QueueAI':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{queuedAI: true}),
						$elm$core$Platform$Cmd$none);
				case 'QueryAI':
					var resetModel = _Utils_update(
						model,
						{queuedAI: false});
					return A2(
						$elm$core$Maybe$withDefault,
						_Utils_Tuple2(resetModel, $elm$core$Platform$Cmd$none),
						A2(
							$elm$core$Maybe$map,
							function (roll) {
								var ts = function () {
									var _v13 = model.ts;
									var gs = _v13.a;
									var tmt = _v13.b;
									return _Utils_eq(gs, model.gs) ? model.ts : $author$project$AI$newNode(model.gs);
								}();
								var ply = function () {
									var _v11 = $author$project$Main$getPlayerType(model);
									switch (_v11.$) {
										case 'AIFast':
											return 2;
										case 'AISlow':
											return 4;
										case 'AIMed':
											var isAPieceOn = function (sq) {
												return A2(
													$elm$core$Maybe$withDefault,
													false,
													A2(
														$elm$core$Maybe$map,
														function (s) {
															if (s.$ === 'Free') {
																return false;
															} else {
																return true;
															}
														},
														A2($author$project$BoardTree$getElem, sq, model.gs.board)));
											};
											var occupancy = A3(
												$elm$core$List$foldl,
												$elm$core$Basics$or,
												false,
												A2(
													$elm$core$List$map,
													isAPieceOn,
													_List_fromArray(
														[21, 22, 23, 24])));
											return occupancy ? 4 : 2;
										default:
											return 2;
									}
								}();
								var _v7 = function () {
									var _v8 = $author$project$Main$getPlayerType(model);
									if (_v8.$ === 'AILast') {
										return A3($author$project$AI$lastPawnAI, model.gs.turn, roll, ts);
									} else {
										return A4($author$project$AI$aiChooseMove, model.gs.turn, ply, roll, ts);
									}
								}();
								var mp = _v7.a;
								var newTS = _v7.b;
								if (_Utils_eq(
									$author$project$Main$getPlayerType(model),
									$author$project$Main$Human)) {
									if (mp.$ === 'Just') {
										var p = mp.a;
										return A2(
											$author$project$Main$update,
											$author$project$Main$Click(p.square),
											resetModel);
									} else {
										return A2($author$project$Main$update, $author$project$Main$Noop, resetModel);
									}
								} else {
									if (mp.$ === 'Just') {
										var p = mp.a;
										return A2(
											$author$project$Main$opChain,
											$author$project$Main$update($author$project$Main$Play),
											A2(
												$author$project$Main$update,
												$author$project$Main$Click(p.square),
												A2($author$project$Main$setTS, newTS, resetModel)));
									} else {
										return A2(
											$author$project$Main$opChain,
											$author$project$Main$update($author$project$Main$NewTurn),
											A2(
												$author$project$Main$update,
												$author$project$Main$Skip,
												A2($author$project$Main$setTS, newTS, resetModel)));
									}
								}
							},
							model.roll));
				case 'QueryRandMove':
					var _v14 = model.roll;
					if (_v14.$ === 'Nothing') {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					} else {
						var r = _v14.a;
						var numtot = A2(
							$elm$core$Maybe$withDefault,
							0,
							A2(
								$elm$core$Maybe$map,
								$elm$core$List$length,
								A2(
									$author$project$BoardTree$getElem,
									r,
									$author$project$AI$findMoves(model.gs))));
						var moveGen = A2($elm$random$Random$int, 0, numtot - 1);
						return (!numtot) ? A2(
							$author$project$Main$update,
							$author$project$Main$NewTurn,
							$author$project$Main$highlightPieces(
								$author$project$Main$clearRoll(
									_Utils_update(
										model,
										{
											gs: $author$project$Logic$skipTurn(model.gs)
										})))) : _Utils_Tuple2(
							model,
							A2($elm$random$Random$generate, $author$project$Main$PlayRandMove, moveGen));
					}
				case 'PlayRandMove':
					var i = msg.a;
					return A2(
						$elm$core$Maybe$withDefault,
						_Utils_Tuple2(model, $elm$core$Platform$Cmd$none),
						A2(
							$elm$core$Maybe$map,
							function (r) {
								var _v15 = A2(
									$elm$core$Maybe$withDefault,
									_Utils_Tuple2(
										$elm$core$Maybe$Nothing,
										$author$project$AI$newNode(
											$author$project$Logic$skipTurn(model.gs))),
									A2(
										$elm$core$Maybe$andThen,
										function (mvs) {
											return $elm$core$List$head(
												A2($elm$core$List$drop, i, mvs));
										},
										A2(
											$author$project$BoardTree$getElem,
											r - 1,
											$author$project$AI$findMoves(model.gs))));
								var mp = _v15.a;
								var newTS = _v15.b;
								if (mp.$ === 'Just') {
									var p = mp.a;
									return A2(
										$author$project$Main$opChain,
										$author$project$Main$update($author$project$Main$Play),
										A2(
											$author$project$Main$update,
											$author$project$Main$Click(p.square),
											A2($author$project$Main$setTS, newTS, model)));
								} else {
									return A2(
										$author$project$Main$opChain,
										$author$project$Main$update($author$project$Main$NewTurn),
										A2(
											$author$project$Main$update,
											$author$project$Main$Skip,
											A2($author$project$Main$setTS, newTS, model)));
								}
							},
							model.roll));
				case 'Play':
					var newModel = A2(
						$elm$core$Maybe$andThen,
						function (nn) {
							return A2(
								$author$project$Main$tryPlay,
								nn,
								$author$project$Main$unselectPiece(model));
						},
						model.selected);
					if (newModel.$ === 'Nothing') {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					} else {
						var nm = newModel.a;
						var $temp$msg = $author$project$Main$NewTurn,
							$temp$model = nm;
						msg = $temp$msg;
						model = $temp$model;
						continue update;
					}
				case 'Skip':
					return A2(
						$author$project$Main$opChain,
						$author$project$Main$update($author$project$Main$NewTurn),
						_Utils_Tuple2(
							$author$project$Main$clearRoll(
								_Utils_update(
									model,
									{
										gs: $author$project$Logic$switchTurn(model.gs)
									})),
							$elm$core$Platform$Cmd$none));
				case 'ChangePlayer':
					var col = msg.a;
					var ptype = msg.b;
					return _Utils_Tuple2(
						A3($author$project$Main$setPlayerType, col, ptype, model),
						$elm$core$Platform$Cmd$none);
				case 'Noop':
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				case 'Reset':
					return _Utils_Tuple2(
						_Utils_update(
							$author$project$Main$initModel,
							{blackPlayer: model.blackPlayer, whitePlayer: model.whitePlayer}),
						$elm$core$Platform$Cmd$none);
				default:
					return _Utils_Tuple2(
						$author$project$Main$highlightPieces(
							$author$project$Main$unselectPiece(model)),
						$elm$core$Platform$Cmd$none);
			}
		}
	});
var $author$project$Main$AIFast = {$: 'AIFast'};
var $author$project$Main$AILast = {$: 'AILast'};
var $author$project$Main$AIMed = {$: 'AIMed'};
var $author$project$Main$AISlow = {$: 'AISlow'};
var $author$project$Main$ChangePlayer = F2(
	function (a, b) {
		return {$: 'ChangePlayer', a: a, b: b};
	});
var $author$project$Main$Reset = {$: 'Reset'};
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $author$project$Docs$href = function (value) {
	return A2($elm$virtual_dom$VirtualDom$attribute, 'href', value);
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Docs$about = A2(
	$elm$html$Html$div,
	_List_Nil,
	_List_fromArray(
		[
			A2(
			$elm$html$Html$h2,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('A bit about Senet')
				])),
			$elm$html$Html$text('\n      In Ancient Egyptian, \'\'Senet\'\' meant \"passing,\" as in passing\n      on to the afterlife. Indeed, many of our records of Senet were\n      found in tombs. Some of the oldest boards date back to\n      the Middle Kingdom (~ 2050 - 1710 BC).\n      The game was first referenced in tomb paintings around\n      the 25'),
			A3(
			$elm$html$Html$node,
			'sup',
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('th')
				])),
			$elm$html$Html$text(' century BC. The game continued to be played for a long time,\n      until it faded from use around the time Egypt came under Roman control\n      (30 BC).\n      For a game that existed for such a long time, it is not so surprising that\n      we have found so many variants of boards. Check some out \n      '),
			A2(
			$elm$html$Html$a,
			_List_fromArray(
				[
					$author$project$Docs$href('https://journals.sagepub.com/doi/full/10.1177/0307513319896288'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'target', '_blank')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('here')
				])),
			$elm$html$Html$text(' or '),
			A2(
			$elm$html$Html$a,
			_List_fromArray(
				[
					$author$project$Docs$href('https://egyptianmuseum.pastperfectonline.com/webobject/9AB20ABF-F246-475B-96C8-422545029070'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'target', '_blank')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('here')
				])),
			$elm$html$Html$text('!')
		]));
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$align = $elm$html$Html$Attributes$stringProperty('align');
var $author$project$Main$centering = $elm$html$Html$Attributes$align('center');
var $elm$html$Html$br = _VirtualDom_node('br');
var $author$project$Docs$newline = A2($elm$html$Html$br, _List_Nil, _List_Nil);
var $author$project$Docs$credits = A2(
	$elm$html$Html$div,
	_List_Nil,
	_List_fromArray(
		[
			$author$project$Docs$newline,
			$author$project$Docs$newline,
			$author$project$Docs$newline,
			$elm$html$Html$text('Developed by Oliver Tsang and Jeffrey Huang\n      in '),
			A2(
			$elm$html$Html$a,
			_List_fromArray(
				[
					$author$project$Docs$href('https://elm-lang.org/'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'target', '_blank')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Elm')
				])),
			$elm$html$Html$text(' for UChicago\'s CMSC 22300 Functional Programming class.\n      Special thanks to the '),
			A2(
			$elm$html$Html$a,
			_List_fromArray(
				[
					$author$project$Docs$href('https://oi.uchicago.edu/'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'target', '_blank')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Oriental Institute')
				])),
			$elm$html$Html$text(' for their support!')
		]));
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $author$project$Main$href = function (value) {
	return A2($elm$virtual_dom$VirtualDom$attribute, 'href', value);
};
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$image = $elm$svg$Svg$trustedNode('image');
var $elm$html$Html$Attributes$name = $elm$html$Html$Attributes$stringProperty('name');
var $author$project$Main$newline = A2($elm$html$Html$br, _List_Nil, _List_Nil);
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$html$Html$b = _VirtualDom_node('b');
var $author$project$Docs$centering = $elm$html$Html$Attributes$align('center');
var $author$project$Docs$frac = F2(
	function (n, d) {
		return _List_fromArray(
			[
				A3(
				$elm$html$Html$node,
				'sup',
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						$elm$core$String$fromInt(n))
					])),
				$elm$html$Html$text('\u002F'),
				A3(
				$elm$html$Html$node,
				'sub',
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						$elm$core$String$fromInt(d))
					]))
			]);
	});
var $elm$html$Html$h4 = _VirtualDom_node('h4');
var $author$project$Docs$myTab = $elm$html$Html$text('\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0');
var $elm$html$Html$ol = _VirtualDom_node('ol');
var $elm$html$Html$table = _VirtualDom_node('table');
var $elm$html$Html$td = _VirtualDom_node('td');
var $elm$html$Html$th = _VirtualDom_node('th');
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Docs$notes = A2(
	$elm$html$Html$div,
	_List_Nil,
	_List_fromArray(
		[
			A2(
			$elm$html$Html$h2,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Additional notes on gameplay')
				])),
			A2(
			$elm$html$Html$h4,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Computer players')
				])),
			$elm$html$Html$text('\n      There are several computer players available which can be selected\n      in the upper right-hand corner:'),
			A2(
			$elm$html$Html$ul,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$b,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Random pawn:')
						])),
					$elm$html$Html$text(' the computer chooses its move at random.'),
					$author$project$Docs$newline,
					$author$project$Docs$newline,
					A2(
					$elm$html$Html$b,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Last pawn: ')
						])),
					$elm$html$Html$text(' the computer always plays the pawn closest to the finish line.'),
					$author$project$Docs$newline,
					$author$project$Docs$newline,
					A2(
					$elm$html$Html$b,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('AI (fast): ')
						])),
					$elm$html$Html$text('\n        the computer searches for the best move to increase its\n        lead over the opponent. Looks 1 turns ahead.'),
					$author$project$Docs$newline,
					$author$project$Docs$newline,
					A2(
					$elm$html$Html$b,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('AI (medium): ')
						])),
					$elm$html$Html$text('\n        the computer searches for the best move to increase its\n        lead over the opponent. Looks 1 turn ahead unless a piece\n        is close to the House of Happiness, in which case it looks\n        ahead 2 turns.'),
					$author$project$Docs$newline,
					$author$project$Docs$newline,
					A2(
					$elm$html$Html$b,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('AI (slow): ')
						])),
					$elm$html$Html$text('\n        the computer searches for the best move to increase its\n        lead over the opponent. Looks 2 turns ahead.')
				])),
			$elm$html$Html$text('\n      The AI (other than random and last pawn) constructs a move tree\n      and  uses an approach known as\n      '),
			A2(
			$elm$html$Html$a,
			_List_fromArray(
				[
					$author$project$Docs$href('https://en.wikipedia.org/wiki/Expectiminimax'),
					A2($elm$html$Html$Attributes$attribute, 'target', '_blank')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('expectiminimax')
				])),
			$elm$html$Html$text('\n      to analyze these\n      moves. It has a heuristic value function to indicate how good\n      a given game state is for it. In this case, the value function\n      looks at the number of squares the opponent needs to move their\n      pawns end of the board, and the value function compares that to\n      the number of squares the current player or AI needs to move its\n      pawns to the end of the board.'),
			$author$project$Docs$newline,
			$author$project$Docs$myTab,
			$elm$html$Html$text('\n      The expectiminimax AI simulates all possible game states for the next\n      turn or two. The computer evaluates each terminal game state\n      (i.e., states beyond which the computer has not explored) using\n      the heuristic value function.'),
			$author$project$Docs$newline,
			$author$project$Docs$myTab,
			$elm$html$Html$text('\n      The computer then goes back and assigns a value to the rest of\n      the non-terminal states. For simplicity assume the computer is\n      playing as black, and its opponent is white. Here\'s how it\n      computes the value of a state:'),
			A2(
			$elm$html$Html$ol,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('\n        If it is black\'s turn, take the value of the state resulting\n        from the strongest move black can make.\n        This is the move with the maximum value.'),
					$author$project$Docs$newline,
					$author$project$Docs$newline,
					$elm$html$Html$text('\n        If it is white\'s turn, take the value of the state resulting\n        from the strongest move white can\n        make against black. This is the move with the minimum value.')
				])),
			$elm$html$Html$text('\n      Since the stick toss introduces randomness, the value of a state\n      is taken as a weighted average (expected value) of the minimum\n      or maximum values of the next move. Here\'s a picture to help\n      visualize the process:'),
			$author$project$Docs$newline,
			$author$project$Docs$newline,
			A2(
			$elm$html$Html$div,
			_List_fromArray(
				[$author$project$Docs$centering]),
			_List_fromArray(
				[
					$elm$html$Html$text('<picture>')
				])),
			$author$project$Docs$newline,
			A2(
			$elm$html$Html$h4,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Strategic tips')
				])),
			$elm$html$Html$text('\n      1. Try to keep a pawn on the House of Happiness as long as you can!\n      If no pawns are nearby, wait until you toss a 4 or 5 to move this\n      pawn for a guaranteed promotion.'),
			$author$project$Docs$newline,
			$author$project$Docs$newline,
			$elm$html$Html$text('\n      2. Each of the sticks has a 50/50 chance of landing light or dark.\n      The four collectively follow a '),
			A2(
			$elm$html$Html$a,
			_List_fromArray(
				[
					$author$project$Docs$href('https://www.mathsisfun.com/data/binomial-distribution.html'),
					A2($elm$html$Html$Attributes$attribute, 'target', '_blank')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('binomial distribution')
				])),
			$elm$html$Html$text(',\n      which means that not all stick tosses are equally likely.\n      Here\'s a table of the probabilities:\n      '),
			$author$project$Docs$newline,
			$author$project$Docs$newline,
			A2(
			$elm$html$Html$table,
			_List_fromArray(
				[
					$author$project$Docs$centering,
					A2($elm$html$Html$Attributes$attribute, 'border', '1')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$tr,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$th,
							_List_fromArray(
								[$author$project$Docs$centering]),
							_List_fromArray(
								[
									$elm$html$Html$text('Stick toss')
								])),
							A2(
							$elm$html$Html$th,
							_List_fromArray(
								[$author$project$Docs$centering]),
							_List_fromArray(
								[
									$elm$html$Html$text('Probability')
								]))
						])),
					A2(
					$elm$html$Html$tr,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$td,
							_List_fromArray(
								[$author$project$Docs$centering]),
							_List_fromArray(
								[
									$elm$html$Html$text('5')
								])),
							A2(
							$elm$html$Html$td,
							_List_fromArray(
								[$author$project$Docs$centering]),
							A2($author$project$Docs$frac, 1, 16))
						])),
					A2(
					$elm$html$Html$tr,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$td,
							_List_fromArray(
								[$author$project$Docs$centering]),
							_List_fromArray(
								[
									$elm$html$Html$text('1')
								])),
							A2(
							$elm$html$Html$td,
							_List_fromArray(
								[$author$project$Docs$centering]),
							A2($author$project$Docs$frac, 4, 16))
						])),
					A2(
					$elm$html$Html$tr,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$td,
							_List_fromArray(
								[$author$project$Docs$centering]),
							_List_fromArray(
								[
									$elm$html$Html$text('2')
								])),
							A2(
							$elm$html$Html$td,
							_List_fromArray(
								[$author$project$Docs$centering]),
							A2($author$project$Docs$frac, 6, 16))
						])),
					A2(
					$elm$html$Html$tr,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$td,
							_List_fromArray(
								[$author$project$Docs$centering]),
							_List_fromArray(
								[
									$elm$html$Html$text('3')
								])),
							A2(
							$elm$html$Html$td,
							_List_fromArray(
								[$author$project$Docs$centering]),
							A2($author$project$Docs$frac, 4, 16))
						])),
					A2(
					$elm$html$Html$tr,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$td,
							_List_fromArray(
								[$author$project$Docs$centering]),
							_List_fromArray(
								[
									$elm$html$Html$text('4')
								])),
							A2(
							$elm$html$Html$td,
							_List_fromArray(
								[$author$project$Docs$centering]),
							A2($author$project$Docs$frac, 1, 16))
						]))
				])),
			$author$project$Docs$newline,
			$elm$html$Html$text('\n      Throwing a 2 is the single most likely option. This means\n      that the House of Three Truths is something like 50% better than\n      the House of Re-Atoum! You can also take advantage of this\n      probability distribution to place your pawns strategically\n      to land on the House of Happiness (and avoid landing just 1 square\n      before it).\n      ')
		]));
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$svg$Svg$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$option = _VirtualDom_node('option');
var $author$project$Logic$promotablePawn = F2(
	function (pSquare, roll) {
		var _v0 = $author$project$Board$squareType(pSquare);
		if (_v0.$ === 'Spec') {
			switch (_v0.a.$) {
				case 'Happy':
					var _v1 = _v0.a;
					return roll === 5;
				case 'Water':
					var _v2 = _v0.a;
					return false;
				case 'Truths':
					var _v3 = _v0.a;
					return roll === 3;
				case 'Reatoum':
					var _v4 = _v0.a;
					return roll === 2;
				default:
					var _v5 = _v0.a;
					return true;
			}
		} else {
			return false;
		}
	});
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$svg$Svg$Attributes$rx = _VirtualDom_attribute('rx');
var $elm$svg$Svg$Attributes$ry = _VirtualDom_attribute('ry');
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						$elm$core$List$cons,
						sep,
						A2($elm$core$List$cons, x, rest));
				});
			var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
			return A2($elm$core$List$cons, hd, spersed);
		}
	});
var $author$project$Docs$svgPathParse = A2(
	$elm$core$Basics$composeR,
	$elm$core$List$intersperse(' '),
	$elm$core$String$concat);
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $author$project$Docs$boardDir = function () {
	var vAdd = F2(
		function (_v12, _v13) {
			var px = _v12.a;
			var py = _v12.b;
			var qx = _v13.a;
			var qy = _v13.b;
			return _Utils_Tuple2(px + qx, py + qy);
		});
	var sqnum = function (n) {
		return _Utils_Tuple2(
			A2($elm$core$Basics$modBy, 3, n),
			(n / 3) | 0);
	};
	var squareNums = A2(
		$elm$core$List$map,
		sqnum,
		A2($elm$core$List$range, 0, 29));
	var slen = 90;
	var sMul = F2(
		function (a, _v11) {
			var px = _v11.a;
			var py = _v11.b;
			return _Utils_Tuple2(a * px, a * py);
		});
	var rlen = 10;
	var mag = function (_v10) {
		var px = _v10.a;
		var py = _v10.b;
		return $elm$core$Basics$sqrt((px * px) + (py * py));
	};
	var norm = function (_v9) {
		var px = _v9.a;
		var py = _v9.b;
		var mag_ = mag(
			_Utils_Tuple2(px, py));
		return _Utils_Tuple2((-py) / mag_, px / mag_);
	};
	var length = 100;
	var getCornerPos = function (_v8) {
		var i = _v8.a;
		var j = _v8.b;
		return _Utils_Tuple2((j * length) + ((rlen / 2) | 0), (i * length) + ((rlen / 2) | 0));
	};
	var minisquare = function (_v7) {
		var i = _v7.a;
		var j = _v7.b;
		var _v6 = getCornerPos(
			_Utils_Tuple2(i, j));
		var x = _v6.a;
		var y = _v6.b;
		return A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x(
					$elm$core$String$fromInt(x)),
					$elm$svg$Svg$Attributes$y(
					$elm$core$String$fromInt(y)),
					$elm$svg$Svg$Attributes$width(
					$elm$core$String$fromInt(slen)),
					$elm$svg$Svg$Attributes$height(
					$elm$core$String$fromInt(slen)),
					$elm$svg$Svg$Attributes$rx(
					$elm$core$String$fromInt(rlen)),
					$elm$svg$Svg$Attributes$ry(
					$elm$core$String$fromInt(rlen)),
					$elm$svg$Svg$Attributes$stroke('gray'),
					$elm$svg$Svg$Attributes$fill('none')
				]),
			_List_Nil);
	};
	var squares = A2($elm$core$List$map, minisquare, squareNums);
	var floatify = function (_v5) {
		var px = _v5.a;
		var py = _v5.b;
		return _Utils_Tuple2(px, py);
	};
	var centerer = function (_v4) {
		var c = _v4.a;
		var d = _v4.b;
		return _Utils_Tuple2(c + ((slen / 2) | 0), d + ((slen / 2) | 0));
	};
	var background = A2(
		$elm$svg$Svg$rect,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$x('-10'),
				$elm$svg$Svg$Attributes$y('-10'),
				$elm$svg$Svg$Attributes$rx('10'),
				$elm$svg$Svg$Attributes$ry('10'),
				$elm$svg$Svg$Attributes$width('1020'),
				$elm$svg$Svg$Attributes$height('320'),
				$elm$svg$Svg$Attributes$stroke('gray'),
				$elm$svg$Svg$Attributes$fill('none')
			]),
		_List_Nil);
	var arrow = F2(
		function (_v2, _v3) {
			var i = _v2.a;
			var j = _v2.b;
			var k = _v3.a;
			var l = _v3.b;
			var _v0 = centerer(
				getCornerPos(
					_Utils_Tuple2(k, l)));
			var w = _v0.a;
			var z = _v0.b;
			var _v1 = centerer(
				getCornerPos(
					_Utils_Tuple2(i, j)));
			var x = _v1.a;
			var y = _v1.b;
			var vec = floatify(
				_Utils_Tuple2(x - w, y - z));
			var len = mag(vec);
			var nvec = A2(
				sMul,
				10,
				norm(vec));
			var setback = A2(sMul, 25.0 / len, vec);
			var pt1 = A2(
				vAdd,
				floatify(
					_Utils_Tuple2(w, z)),
				setback);
			var pt2 = A2(vAdd, pt1, nvec);
			var pt3 = A2(
				vAdd,
				pt1,
				A2(sMul, -1.0, nvec));
			return _List_fromArray(
				[
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$stroke('orange'),
							$elm$svg$Svg$Attributes$strokeWidth('3'),
							$elm$svg$Svg$Attributes$fill('orange'),
							$elm$svg$Svg$Attributes$d(
							$author$project$Docs$svgPathParse(
								_List_fromArray(
									[
										'M',
										$elm$core$String$fromInt(x),
										$elm$core$String$fromInt(y),
										'L',
										$elm$core$String$fromInt(w),
										$elm$core$String$fromInt(z)
									])))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$stroke('orange'),
							$elm$svg$Svg$Attributes$strokeWidth('2'),
							$elm$svg$Svg$Attributes$fill('orange'),
							$elm$svg$Svg$Attributes$d(
							$author$project$Docs$svgPathParse(
								_List_fromArray(
									[
										'M',
										$elm$core$String$fromInt(w),
										$elm$core$String$fromInt(z),
										'L',
										$elm$core$String$fromFloat(pt2.a),
										$elm$core$String$fromFloat(pt2.b),
										'L',
										$elm$core$String$fromFloat(pt3.a),
										$elm$core$String$fromFloat(pt3.b),
										'F',
										$elm$core$String$fromFloat(
										0.02 * mag(vec)),
										$elm$core$String$fromFloat(0.95 * len)
									])))
						]),
					_List_Nil)
				]);
		});
	return A2(
		$elm$svg$Svg$svg,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$viewBox('-50 -20 1060 350'),
				$elm$svg$Svg$Attributes$width('30%')
			]),
		A2(
			$elm$core$List$cons,
			background,
			_Utils_ap(
				squares,
				_Utils_ap(
					A2(
						arrow,
						_Utils_Tuple2(0, 0),
						_Utils_Tuple2(0, 5)),
					_Utils_ap(
						A2(
							arrow,
							_Utils_Tuple2(0, 4),
							_Utils_Tuple2(0, 9)),
						_Utils_ap(
							A2(
								arrow,
								_Utils_Tuple2(0, 9),
								_Utils_Tuple2(1, 9)),
							_Utils_ap(
								A2(
									arrow,
									_Utils_Tuple2(1, 9),
									_Utils_Tuple2(1, 4)),
								_Utils_ap(
									A2(
										arrow,
										_Utils_Tuple2(1, 5),
										_Utils_Tuple2(1, 0)),
									_Utils_ap(
										A2(
											arrow,
											_Utils_Tuple2(1, 0),
											_Utils_Tuple2(2, 0)),
										_Utils_ap(
											A2(
												arrow,
												_Utils_Tuple2(2, 0),
												_Utils_Tuple2(2, 5)),
											A2(
												arrow,
												_Utils_Tuple2(2, 4),
												_Utils_Tuple2(2, 9))))))))))));
}();
var $elm$html$Html$p = _VirtualDom_node('p');
var $author$project$Docs$plantPic = function (imgName) {
	return A2(
		$elm$svg$Svg$svg,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$viewBox('0 0 50 50'),
				$elm$svg$Svg$Attributes$width('80')
			]),
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$rect,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$width('50'),
						$elm$svg$Svg$Attributes$height('50'),
						$elm$svg$Svg$Attributes$fill('antiquewhite'),
						$elm$svg$Svg$Attributes$x('0'),
						$elm$svg$Svg$Attributes$y('0'),
						$elm$svg$Svg$Attributes$rx('5'),
						$elm$svg$Svg$Attributes$ry('5')
					]),
				_List_Nil),
				A2(
				$elm$svg$Svg$image,
				_List_fromArray(
					[
						$author$project$Docs$href(imgName),
						$elm$svg$Svg$Attributes$width('50')
					]),
				_List_Nil)
			]));
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$Sticks$Light = {$: 'Light'};
var $elm$svg$Svg$animate = $elm$svg$Svg$trustedNode('animate');
var $elm$svg$Svg$Attributes$attributeName = _VirtualDom_attribute('attributeName');
var $elm$svg$Svg$Attributes$dur = _VirtualDom_attribute('dur');
var $elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var $elm$svg$Svg$Attributes$repeatCount = _VirtualDom_attribute('repeatCount');
var $author$project$Sticks$Dark = {$: 'Dark'};
var $author$project$Sticks$retDark = function (b) {
	if (b) {
		return $author$project$Sticks$Dark;
	} else {
		return $author$project$Sticks$Light;
	}
};
var $elm$svg$Svg$Attributes$calcMode = _VirtualDom_attribute('calcMode');
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $author$project$Sticks$getStr = function (side) {
	if (side.$ === 'Light') {
		return 'floralwhite';
	} else {
		return 'rosybrown';
	}
};
var $elm$svg$Svg$Attributes$keySplines = _VirtualDom_attribute('keySplines');
var $elm$svg$Svg$Attributes$keyTimes = _VirtualDom_attribute('keyTimes');
var $author$project$Sticks$oppCol = function (side) {
	if (side.$ === 'Light') {
		return $author$project$Sticks$Dark;
	} else {
		return $author$project$Sticks$Light;
	}
};
var $elm$svg$Svg$Attributes$values = function (value) {
	return A2(
		_VirtualDom_attribute,
		'values',
		_VirtualDom_noJavaScriptUri(value));
};
var $author$project$Sticks$singleStick = F2(
	function (side, num) {
		var xInit = 15 + (30 * num);
		var xInitStr = $elm$core$String$fromInt(xInit);
		var xMid = xInit + 7;
		var xMidStr = $elm$core$String$fromInt(xMid);
		var xVals = xInitStr + (';' + (xMidStr + (';' + (xInitStr + (';' + (xMidStr + (';' + xInitStr)))))));
		var spline1 = '0 0 0.55 1;';
		var spline0 = '0 0 0.55 0;';
		var sideStr = $author$project$Sticks$getStr(side) + ';';
		var rc = '2';
		var keyTimes = '0; 0.24;0.25; 0.74;0.75; 1';
		var keySplines = _Utils_ap(
			spline0,
			_Utils_ap(
				spline1,
				_Utils_ap(spline0, spline1)));
		var flipStr = $author$project$Sticks$getStr(
			$author$project$Sticks$oppCol(side)) + ';';
		var dur = $elm$core$String$fromFloat(0.5 + (num / 6)) + 's';
		return A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$fill(
					$author$project$Sticks$getStr(side)),
					$elm$svg$Svg$Attributes$y('15'),
					$elm$svg$Svg$Attributes$x(xInitStr),
					$elm$svg$Svg$Attributes$width('15'),
					$elm$svg$Svg$Attributes$height('70'),
					$elm$svg$Svg$Attributes$class('stick')
				]),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$animate,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$attributeName('fill'),
							$elm$svg$Svg$Attributes$dur(dur),
							$elm$svg$Svg$Attributes$repeatCount(rc),
							$elm$svg$Svg$Attributes$values(
							_Utils_ap(
								sideStr,
								_Utils_ap(
									sideStr,
									_Utils_ap(
										flipStr,
										_Utils_ap(
											flipStr,
											_Utils_ap(sideStr, sideStr)))))),
							$elm$svg$Svg$Attributes$keyTimes(keyTimes)
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$animate,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$attributeName('x'),
							$elm$svg$Svg$Attributes$dur(dur),
							$elm$svg$Svg$Attributes$repeatCount(rc),
							$elm$svg$Svg$Attributes$values(xVals),
							$elm$svg$Svg$Attributes$calcMode('spline'),
							$elm$svg$Svg$Attributes$keySplines(keySplines)
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$animate,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$attributeName('width'),
							$elm$svg$Svg$Attributes$dur(dur),
							$elm$svg$Svg$Attributes$repeatCount(rc),
							$elm$svg$Svg$Attributes$values('15; 1; 15; 1; 15'),
							$elm$svg$Svg$Attributes$calcMode('spline'),
							$elm$svg$Svg$Attributes$keySplines(keySplines)
						]),
					_List_Nil)
				]));
	});
var $author$project$Sticks$singleStickStatic = F2(
	function (side, num) {
		var xInit = 15 + (30 * num);
		var xInitStr = $elm$core$String$fromInt(xInit);
		var sideStr = $author$project$Sticks$getStr(side);
		var rc = '3';
		return A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$fill(sideStr),
					$elm$svg$Svg$Attributes$y('15'),
					$elm$svg$Svg$Attributes$x(xInitStr),
					$elm$svg$Svg$Attributes$width('15'),
					$elm$svg$Svg$Attributes$height('70'),
					$elm$svg$Svg$Attributes$class('stick')
				]),
			_List_Nil);
	});
var $author$project$Sticks$svgSticks = F4(
	function (anim, roll, op, attrs) {
		var clickable = A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('0'),
					$elm$svg$Svg$Attributes$y('0'),
					$elm$svg$Svg$Attributes$rx('10'),
					$elm$svg$Svg$Attributes$ry('10'),
					$elm$svg$Svg$Attributes$height('100'),
					$elm$svg$Svg$Attributes$width('135'),
					$elm$svg$Svg$Attributes$fill('transparent'),
					$elm$svg$Svg$Attributes$stroke('none'),
					$elm$svg$Svg$Attributes$id('clickable'),
					$elm$html$Html$Events$onClick(op)
				]),
			_List_Nil);
		var backgroundStatic = A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('0'),
					$elm$svg$Svg$Attributes$y('0'),
					$elm$svg$Svg$Attributes$rx('10'),
					$elm$svg$Svg$Attributes$ry('10'),
					$elm$svg$Svg$Attributes$height('100'),
					$elm$svg$Svg$Attributes$width('135'),
					$elm$svg$Svg$Attributes$fill('antiquewhite'),
					$elm$svg$Svg$Attributes$stroke('none')
				]),
			_List_Nil);
		var backgroundPulsing = A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('0'),
					$elm$svg$Svg$Attributes$y('0'),
					$elm$svg$Svg$Attributes$rx('10'),
					$elm$svg$Svg$Attributes$ry('10'),
					$elm$svg$Svg$Attributes$height('100'),
					$elm$svg$Svg$Attributes$width('135'),
					$elm$svg$Svg$Attributes$fill('antiquewhite'),
					$elm$svg$Svg$Attributes$stroke('palegreen'),
					$elm$svg$Svg$Attributes$strokeWidth('0')
				]),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$animate,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$attributeName('stroke-width'),
							$elm$svg$Svg$Attributes$dur('4s'),
							$elm$svg$Svg$Attributes$repeatCount('indefinite'),
							$elm$svg$Svg$Attributes$values('1.5; 4; 1.5')
						]),
					_List_Nil)
				]));
		var contents = function () {
			if (roll.$ === 'Just') {
				var r = roll.a;
				var r0 = A2($elm$core$Basics$modBy, 5, r);
				var side0 = $author$project$Sticks$retDark(r0 > 0);
				var side1 = $author$project$Sticks$retDark(r0 > 1);
				var side2 = $author$project$Sticks$retDark(r0 > 2);
				var side3 = $author$project$Sticks$retDark(r0 > 3);
				return anim ? _List_fromArray(
					[
						backgroundStatic,
						A2($author$project$Sticks$singleStick, side0, 0),
						A2($author$project$Sticks$singleStick, side1, 1),
						A2($author$project$Sticks$singleStick, side2, 2),
						A2($author$project$Sticks$singleStick, side3, 3),
						clickable
					]) : _List_fromArray(
					[
						backgroundStatic,
						A2($author$project$Sticks$singleStickStatic, side0, 0),
						A2($author$project$Sticks$singleStickStatic, side1, 1),
						A2($author$project$Sticks$singleStickStatic, side2, 2),
						A2($author$project$Sticks$singleStickStatic, side3, 3),
						clickable
					]);
			} else {
				return _List_fromArray(
					[
						backgroundPulsing,
						A2($author$project$Sticks$singleStickStatic, $author$project$Sticks$Light, 0),
						A2($author$project$Sticks$singleStickStatic, $author$project$Sticks$Light, 1),
						A2($author$project$Sticks$singleStickStatic, $author$project$Sticks$Light, 2),
						A2($author$project$Sticks$singleStickStatic, $author$project$Sticks$Light, 3),
						clickable
					]);
			}
		}();
		return A2(
			$elm$svg$Svg$svg,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$viewBox('-5 -5 145 110'),
						$elm$svg$Svg$Attributes$id('sticks')
					]),
				attrs),
			contents);
	});
var $author$project$Docs$rules = function (op) {
	return A2(
		$elm$html$Html$p,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$h2,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Rules of Senet')
					])),
				$elm$html$Html$text('\n      The set of rules used by Ancient Egyptians have been long lost.\n      However, some historians have proposed rules that seem reasonable.\n      Here, we implement rules based on\n      '),
				A2(
				$elm$html$Html$a,
				_List_fromArray(
					[
						$author$project$Docs$href('https://www.startwithabook.org/content/pdfs/EgyptianSenetGame.pdf'),
						A2($elm$html$Html$Attributes$attribute, 'target', '_blank')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Kendall\'s rules')
					])),
				$elm$html$Html$text('.\n      The objective of Senet is to promote all your pieces\n      from the board. Pawns move across the board in a snake\n      pattern:\n      '),
				$author$project$Docs$newline,
				$author$project$Docs$newline,
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[$author$project$Docs$centering]),
				_List_fromArray(
					[$author$project$Docs$boardDir])),
				$author$project$Docs$newline,
				$elm$html$Html$text('\n      At each move, you begin by throwing four sticks to determine\n      how many squares you can move your pawns.\n      '),
				A2(
				$elm$html$Html$h4,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Throwing sticks')
					])),
				$elm$html$Html$text('\n      There are four sticks, each with one light side and one dark side.\n      Each dark side corresponds to a 1, and each light side\n      corresponds to a 0. After throwing the four sticks,\n      we add up the resulting 0/1 values. This is the number of\n      squares you can move your pawns.\n      However, if all sides come up dark, i.e. the total is 0,\n      the pawns can be moved by 5 squares.\n      '),
				$author$project$Docs$newline,
				$author$project$Docs$newline,
				A2(
				$elm$html$Html$table,
				_List_fromArray(
					[$author$project$Docs$centering]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '120'),
										$author$project$Docs$centering
									]),
								_List_fromArray(
									[
										A4(
										$author$project$Sticks$svgSticks,
										false,
										$elm$core$Maybe$Just(2),
										op,
										_List_Nil)
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('\n          Throw the sticks by pressing the picture of the sticks in the\n          upper right-hand corner, or press the \'\'roll\'\' button.\n          ')
									]))
							]))
					])),
				$author$project$Docs$newline,
				A2(
				$elm$html$Html$h4,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Moving your pawn')
					])),
				$elm$html$Html$text('\n      In general, you can choose to move any pawn by the result of\n      the stick toss (between 1 and 5). If your pawn lands on an\n      opponent\'s pawn, then the two pawns swap positions.\n      However, there are special rules restricting movement in the\n      last five squares. (This is done to make pawn promotion more\n      interesting)\n      '),
				$author$project$Docs$newline,
				$author$project$Docs$newline,
				A2(
				$elm$html$Html$table,
				_List_fromArray(
					[$author$project$Docs$centering]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '20%'),
										$author$project$Docs$centering
									]),
								_List_fromArray(
									[
										$author$project$Docs$plantPic('images/rebirth.svg')
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$b,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('House of Rebirth: ')
											])),
										$elm$html$Html$text('This is where pawns are sent for\n          \'\'rebirth.\'\' If this square is occupied, the pawn\n          is sent to the first unoccupied square before this square.\n          ')
									]))
							])),
						A2(
						$elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '20%'),
										$author$project$Docs$centering
									]),
								_List_fromArray(
									[
										$author$project$Docs$plantPic('images/happy.svg')
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$b,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('House of Happiness:')
											])),
										$elm$html$Html$text('\n          All pawns must pass through\n          this square. Pawns are not allowed to jump over this\n          square, and they must land precisely on this square.\n          ')
									]))
							])),
						A2(
						$elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '20%'),
										$author$project$Docs$centering
									]),
								_List_fromArray(
									[
										$author$project$Docs$plantPic('images/water.svg')
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$b,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('House of Water:')
											])),
										$elm$html$Html$text('\n          If a pawn lands on this square, it must return to\n          the House of Rebirth.\n          ')
									]))
							])),
						A2(
						$elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '20%'),
										$author$project$Docs$centering
									]),
								_List_fromArray(
									[
										$author$project$Docs$plantPic('images/three.svg')
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$b,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('House of Three Truths:')
											])),
										$elm$html$Html$text('\n          If a pawn lands on this square, the following turn\n          you can promote that pawn if you throw a 3 and play it.\n          Otherwise, the pawn will be sent back to the House of Rebirth.\n          ')
									]))
							])),
						A2(
						$elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '20%'),
										$author$project$Docs$centering
									]),
								_List_fromArray(
									[
										$author$project$Docs$plantPic('images/two.svg')
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$b,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('House of Re-Atoum:')
											])),
										$elm$html$Html$text('\n          If a pawn lands on this square, the following turn\n          you can promote that pawn if you throw a 2 and play it.\n          Otherwise, the pawn will be sent back to the House of Rebirth.\n          ')
									]))
							])),
						A2(
						$elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '20%'),
										$author$project$Docs$centering
									]),
								_List_fromArray(
									[
										$author$project$Docs$plantPic('images/horus.svg')
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$b,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('House of Horus:')
											])),
										$elm$html$Html$text('\n          If a pawn lands on this square, the following turn\n          you can promote it with any stick toss.\n          However, if you do not play that pawn, it will be\n          sent back to the House of Rebirth.')
									]))
							]))
					])),
				$author$project$Docs$newline,
				$elm$html$Html$text('\n      Occasionally there may be no legal moves available. In this case,\n      just skip your turn and sit tight until it\'s your turn again (and\n      hope for better luck)!')
			]));
};
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $author$project$Main$scoreboard = F2(
	function (n, color) {
		var rlen = ((2 * 90) / 7) | 0;
		var spacing = ((2 * rlen) / 3) | 0;
		var width = (((($author$project$Board$initPawnCount - 1) + 2) * spacing) + (2 * rlen)) + 4;
		var outline = A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('0'),
					$elm$svg$Svg$Attributes$y('0'),
					$elm$svg$Svg$Attributes$rx('10'),
					$elm$svg$Svg$Attributes$ry('10'),
					$elm$svg$Svg$Attributes$height('100'),
					$elm$svg$Svg$Attributes$fill('antiquewhite'),
					$elm$svg$Svg$Attributes$stroke('none'),
					$elm$svg$Svg$Attributes$width(
					$elm$core$String$fromInt(width)),
					$elm$html$Html$Events$onClick(
					$author$project$Main$Click(30))
				]),
			_List_Nil);
		var makepiece = F2(
			function (x, y) {
				return _List_fromArray(
					[
						A2(
						$elm$svg$Svg$circle,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$cx(
								$elm$core$String$fromInt(x + 1)),
								$elm$svg$Svg$Attributes$cy(
								$elm$core$String$fromInt(y + 50)),
								$elm$svg$Svg$Attributes$r(
								$elm$core$String$fromInt(rlen)),
								$elm$svg$Svg$Attributes$stroke('black'),
								$elm$svg$Svg$Attributes$fill(
								_Utils_eq(color, $author$project$Board$White) ? 'snow' : 'slategray')
							]),
						_List_Nil)
					]);
			});
		var centers = (!A2($elm$core$Basics$modBy, 2, n)) ? A2(
			$elm$core$List$map,
			function (x) {
				return ((width / 2) | 0) + ((x - ((n / 2) | 0)) * spacing);
			},
			A2($elm$core$List$range, 1, n)) : A2(
			$elm$core$List$map,
			function (x) {
				return ((width / 2) | 0) + ((x - (((n + 1) / 2) | 0)) * spacing);
			},
			A2($elm$core$List$range, 1, n));
		return A2(
			$elm$svg$Svg$svg,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$width('80%'),
					$elm$svg$Svg$Attributes$viewBox(
					'0 0 ' + ($elm$core$String$fromInt(width) + ' 100'))
				]),
			A2(
				$elm$core$List$cons,
				outline,
				A2(
					$elm$core$List$concatMap,
					function (x) {
						return A2(makepiece, x, 0);
					},
					centers)));
	});
var $elm$html$Html$select = _VirtualDom_node('select');
var $author$project$Main$selectionColor = 'palegreen';
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$Main$svgSquare = F5(
	function (length, model, n, i, j) {
		var rlen = (length / 10) | 0;
		var slen = length - rlen;
		var x = (j * length) + ((rlen / 2) | 0);
		var y = (i * length) + ((rlen / 2) | 0);
		var sqRect = _List_fromArray(
			[
				A2(
				$elm$svg$Svg$rect,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$x(
						$elm$core$String$fromInt(x)),
						$elm$svg$Svg$Attributes$y(
						$elm$core$String$fromInt(y)),
						$elm$svg$Svg$Attributes$width(
						$elm$core$String$fromInt(slen)),
						$elm$svg$Svg$Attributes$height(
						$elm$core$String$fromInt(slen)),
						$elm$svg$Svg$Attributes$rx(
						$elm$core$String$fromInt(rlen)),
						$elm$svg$Svg$Attributes$ry(
						$elm$core$String$fromInt(rlen)),
						$elm$svg$Svg$Attributes$stroke(
						A2($elm$core$List$member, n, model.highlighted) ? $author$project$Main$selectionColor : 'black'),
						$elm$svg$Svg$Attributes$strokeWidth(
						A2($elm$core$List$member, n, model.highlighted) ? '5' : '1'),
						$elm$svg$Svg$Attributes$fill(
						(!A2($elm$core$Basics$modBy, 2, n)) ? 'beige' : 'burlywood'),
						$elm$svg$Svg$Events$onClick(
						$author$project$Main$Click(n))
					]),
				_List_Nil)
			]);
		var piece = function (mp) {
			if (mp.$ === 'Just') {
				var p = mp.a;
				return _List_fromArray(
					[
						A2(
						$elm$svg$Svg$circle,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$cx(
								$elm$core$String$fromInt(x + ((slen / 2) | 0))),
								$elm$svg$Svg$Attributes$cy(
								$elm$core$String$fromInt(y + ((slen / 2) | 0))),
								$elm$svg$Svg$Attributes$r(
								$elm$core$String$fromInt(((2 * slen) / 7) | 0)),
								$elm$svg$Svg$Attributes$stroke('black'),
								$elm$svg$Svg$Attributes$fill(
								_Utils_eq(p.color, $author$project$Board$White) ? 'snow' : 'slategray'),
								$elm$svg$Svg$Events$onClick(
								$author$project$Main$Click(n))
							]),
						_List_Nil)
					]);
			} else {
				return _List_Nil;
			}
		};
		var picSize = '90';
		var sqImage = function () {
			var attrList = _List_fromArray(
				[
					$elm$svg$Svg$Attributes$x(
					$elm$core$String$fromInt(x)),
					$elm$svg$Svg$Attributes$y(
					$elm$core$String$fromInt(y)),
					$elm$svg$Svg$Attributes$width(picSize),
					$elm$svg$Svg$Attributes$height(picSize),
					$elm$svg$Svg$Events$onClick(
					$author$project$Main$Click(n))
				]);
			var _v0 = $author$project$Board$squareType(n);
			switch (_v0.$) {
				case 'Rebirth':
					return _List_fromArray(
						[
							A2(
							$elm$svg$Svg$image,
							A2(
								$elm$core$List$cons,
								$author$project$Main$href('images/rebirth.svg'),
								attrList),
							_List_Nil)
						]);
				case 'Spec':
					switch (_v0.a.$) {
						case 'Happy':
							var _v1 = _v0.a;
							return _List_fromArray(
								[
									A2(
									$elm$svg$Svg$image,
									A2(
										$elm$core$List$cons,
										$author$project$Main$href('images/happy.svg'),
										attrList),
									_List_Nil)
								]);
						case 'Water':
							var _v2 = _v0.a;
							return _List_fromArray(
								[
									A2(
									$elm$svg$Svg$image,
									A2(
										$elm$core$List$cons,
										$author$project$Main$href('images/water.svg'),
										attrList),
									_List_Nil)
								]);
						case 'Truths':
							var _v3 = _v0.a;
							return _List_fromArray(
								[
									A2(
									$elm$svg$Svg$image,
									A2(
										$elm$core$List$cons,
										$author$project$Main$href('images/three.svg'),
										attrList),
									_List_Nil)
								]);
						case 'Reatoum':
							var _v4 = _v0.a;
							return _List_fromArray(
								[
									A2(
									$elm$svg$Svg$image,
									A2(
										$elm$core$List$cons,
										$author$project$Main$href('images/two.svg'),
										attrList),
									_List_Nil)
								]);
						default:
							var _v5 = _v0.a;
							return _List_fromArray(
								[
									A2(
									$elm$svg$Svg$image,
									A2(
										$elm$core$List$cons,
										$author$project$Main$href('images/horus.svg'),
										attrList),
									_List_Nil)
								]);
					}
				default:
					return _List_Nil;
			}
		}();
		var maybePawn = A2($author$project$Board$getPawn, n, model.gs);
		return A2(
			$elm$svg$Svg$svg,
			_List_Nil,
			_Utils_ap(
				sqRect,
				_Utils_ap(
					sqImage,
					piece(maybePawn))));
	});
var $author$project$Main$svgBoard = function (model) {
	var ls = A2($elm$core$List$range, 0, 9);
	var length = 100;
	var line1 = A2(
		$elm$core$List$map,
		function (i) {
			return A5($author$project$Main$svgSquare, length, model, i, 0, i);
		},
		ls);
	var line2 = A2(
		$elm$core$List$map,
		function (i) {
			return A5($author$project$Main$svgSquare, length, model, 19 - i, 1, i);
		},
		ls);
	var line3 = A2(
		$elm$core$List$map,
		function (i) {
			return A5($author$project$Main$svgSquare, length, model, 20 + i, 2, i);
		},
		ls);
	var background = A2(
		$elm$svg$Svg$rect,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$x('-10'),
				$elm$svg$Svg$Attributes$y('-10'),
				$elm$svg$Svg$Attributes$rx('10'),
				$elm$svg$Svg$Attributes$ry('10'),
				$elm$svg$Svg$Attributes$width('1020'),
				$elm$svg$Svg$Attributes$height('320'),
				$elm$svg$Svg$Attributes$fill('gray')
			]),
		_List_Nil);
	return A2(
		$elm$svg$Svg$svg,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$viewBox('-50 -20 1060 350')
			]),
		_Utils_ap(
			_List_fromArray(
				[background]),
			_Utils_ap(
				line1,
				_Utils_ap(line2, line3))));
};
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Main$view = function (model) {
	var wscoreboard = function () {
		var wpawn = $author$project$Board$initPawnCount - model.gs.whitePawnCnt;
		return A2(
			$elm$html$Html$h3,
			_List_fromArray(
				[$author$project$Main$centering]),
			_List_fromArray(
				[
					$elm$html$Html$text(
					'White: ' + $elm$core$String$fromInt(wpawn))
				]));
	}();
	var turn = A2(
		$elm$html$Html$h2,
		_List_fromArray(
			[$author$project$Main$centering]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				function () {
					var _v5 = $author$project$Board$isOver(model.gs);
					if (_v5.$ === 'NotDone') {
						return _Utils_eq(model.gs.turn, $author$project$Board$White) ? 'White\'s turn' : 'Black\'s turn';
					} else {
						if (_v5.a.$ === 'White') {
							var _v6 = _v5.a;
							return 'White won!';
						} else {
							var _v7 = _v5.a;
							return 'Black won!';
						}
					}
				}())
			]));
	var title = A2(
		$elm$html$Html$h1,
		_List_fromArray(
			[$author$project$Main$centering]),
		_List_fromArray(
			[
				$elm$html$Html$text('Senet')
			]));
	var selector = function (col) {
		return A2(
			$elm$html$Html$select,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$name(
					function () {
						if (col.$ === 'Black') {
							return 'Player 1 (Black)';
						} else {
							return 'Player 2 (White)';
						}
					}())
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$option,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$value('Human'),
							$elm$html$Html$Events$onClick(
							A2($author$project$Main$ChangePlayer, col, $author$project$Main$Human))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Human')
						])),
					A2(
					$elm$html$Html$option,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$value('AILast'),
							$elm$html$Html$Events$onClick(
							A2($author$project$Main$ChangePlayer, col, $author$project$Main$AIRand))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Random pawn')
						])),
					A2(
					$elm$html$Html$option,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$value('AILast'),
							$elm$html$Html$Events$onClick(
							A2($author$project$Main$ChangePlayer, col, $author$project$Main$AILast))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Last pawn')
						])),
					A2(
					$elm$html$Html$option,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$value('AIFast'),
							$elm$html$Html$Events$onClick(
							A2($author$project$Main$ChangePlayer, col, $author$project$Main$AIFast))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('AI (fast)')
						])),
					A2(
					$elm$html$Html$option,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$value('AIMed'),
							$elm$html$Html$Events$onClick(
							A2($author$project$Main$ChangePlayer, col, $author$project$Main$AIMed))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('AI (medium)')
						])),
					A2(
					$elm$html$Html$option,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$value('AISlow'),
							$elm$html$Html$Events$onClick(
							A2($author$project$Main$ChangePlayer, col, $author$project$Main$AISlow))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('AI (slow)')
						]))
				]));
	};
	var promotionImminent = A2(
		$elm$core$Maybe$withDefault,
		false,
		A2(
			$elm$core$Maybe$andThen,
			function (p) {
				return A2(
					$elm$core$Maybe$map,
					function (roll) {
						return A2($author$project$Logic$promotablePawn, p, roll);
					},
					model.roll);
			},
			model.selected));
	var currPlayer = function () {
		var _v3 = model.gs.turn;
		if (_v3.$ === 'White') {
			return model.whitePlayer;
		} else {
			return model.blackPlayer;
		}
	}();
	var centerHeader = A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				title,
				turn,
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[$author$project$Main$centering]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Main$QueryRoll),
								$elm$html$Html$Attributes$disabled(
								(!_Utils_eq(
									$author$project$Board$NotDone,
									$author$project$Board$isOver(model.gs))) || (!_Utils_eq(model.roll, $elm$core$Maybe$Nothing)))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								function () {
									var _v1 = model.roll;
									if (_v1.$ === 'Just') {
										var r = _v1.a;
										return 'Toss: ' + $elm$core$String$fromInt(r);
									} else {
										return 'Toss!';
									}
								}())
							])),
						$elm$html$Html$text('\t'),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								_Utils_eq(currPlayer, $author$project$Main$AIRand) ? $elm$html$Html$Events$onClick($author$project$Main$QueryRandMove) : $elm$html$Html$Events$onClick($author$project$Main$QueryAI),
								$elm$html$Html$Attributes$disabled(
								(!_Utils_eq(
									$author$project$Board$NotDone,
									$author$project$Board$isOver(model.gs))) || model.queuedAI)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								model.queuedAI ? 'Thinking...' : 'Ask the AI!')
							])),
						$author$project$Main$newline,
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Main$Play),
								$elm$html$Html$Attributes$disabled(
								_Utils_eq($elm$core$Maybe$Nothing, model.selected) || (!_Utils_eq(
									$author$project$Board$NotDone,
									$author$project$Board$isOver(model.gs))))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								function () {
									var _v2 = model.selected;
									if (_v2.$ === 'Just') {
										var s = _v2.a;
										return promotionImminent ? ('Promote pawn on square ' + $elm$core$String$fromInt(s + 1)) : ('Play pawn on square ' + $elm$core$String$fromInt(s + 1));
									} else {
										return 'Select a piece';
									}
								}())
							])),
						$elm$html$Html$text('\t'),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Main$Skip),
								$elm$html$Html$Attributes$disabled(
								(!model.skippedMove) || (!_Utils_eq(
									$author$project$Board$NotDone,
									$author$project$Board$isOver(model.gs))))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Skip turn')
							]))
					]))
			]));
	var bscoreboard = function () {
		var bpawn = $author$project$Board$initPawnCount - model.gs.blackPawnCnt;
		return A2(
			$elm$html$Html$h3,
			_List_fromArray(
				[$author$project$Main$centering]),
			_List_fromArray(
				[
					$elm$html$Html$text(
					'Black: ' + $elm$core$String$fromInt(bpawn))
				]));
	}();
	var header = A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$table,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'width', '100%')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '30%')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$table,
										_List_fromArray(
											[
												A2($elm$html$Html$Attributes$style, 'width', '100%')
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$td,
												_List_fromArray(
													[
														A2($elm$html$Html$Attributes$style, 'width', '10%'),
														$author$project$Main$centering
													]),
												_List_Nil),
												A2(
												$elm$html$Html$td,
												_List_fromArray(
													[
														A2($elm$html$Html$Attributes$style, 'width', '40%'),
														$author$project$Main$centering
													]),
												_List_fromArray(
													[
														bscoreboard,
														A2($author$project$Main$scoreboard, $author$project$Board$initPawnCount - model.gs.blackPawnCnt, $author$project$Board$Black)
													])),
												A2(
												$elm$html$Html$td,
												_List_fromArray(
													[
														A2($elm$html$Html$Attributes$style, 'width', '40%'),
														$author$project$Main$centering
													]),
												_List_fromArray(
													[
														wscoreboard,
														A2($author$project$Main$scoreboard, $author$project$Board$initPawnCount - model.gs.whitePawnCnt, $author$project$Board$White)
													]))
											]))
									])),
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '10%')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '20%')
									]),
								_List_fromArray(
									[centerHeader])),
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '100'),
										$author$project$Main$centering
									]),
								_List_fromArray(
									[
										A4(
										$author$project$Sticks$svgSticks,
										!$author$project$Main$isPlayerAI(currPlayer),
										model.roll,
										$author$project$Main$QueryRoll,
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$width('200')
											])),
										A2(
										$elm$html$Html$h3,
										_List_fromArray(
											[$author$project$Main$centering]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												function () {
													var _v0 = model.roll;
													if (_v0.$ === 'Nothing') {
														return 'Toss sticks!';
													} else {
														var r = _v0.a;
														return 'Tossed a ' + $elm$core$String$fromInt(r);
													}
												}())
											]))
									])),
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '20%'),
										$author$project$Main$centering
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Player 1 (Black): '),
										selector($author$project$Board$Black),
										$author$project$Main$newline,
										$elm$html$Html$text('\tPlayer 2 (White): '),
										selector($author$project$Board$White),
										$author$project$Main$newline,
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick($author$project$Main$Reset),
												$elm$html$Html$Attributes$disabled(
												_Utils_eq(
													$author$project$Board$NotDone,
													$author$project$Board$isOver(model.gs)))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('New game')
											]))
									]))
							]))
					]))
			]));
	var afterlifeRect = A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$elm$html$Html$text(
				promotionImminent ? 'Promotion available!' : 'No promotion available'),
				A2(
				$elm$svg$Svg$svg,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$x('0'),
						$elm$svg$Svg$Attributes$y('0'),
						$elm$svg$Svg$Attributes$rx('10'),
						$elm$svg$Svg$Attributes$ry('10'),
						$elm$svg$Svg$Attributes$width('100%'),
						$elm$svg$Svg$Attributes$viewBox('-10 -10 220 295'),
						$elm$svg$Svg$Events$onClick(
						$author$project$Main$Click(30))
					]),
				_List_fromArray(
					[
						A2(
						$elm$svg$Svg$image,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$x('0'),
								$elm$svg$Svg$Attributes$y('0'),
								$author$project$Main$href('images/afterlife.jfif'),
								$elm$svg$Svg$Events$onClick(
								$author$project$Main$Click(30))
							]),
						_List_Nil),
						A2(
						$elm$svg$Svg$rect,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$x('0'),
								$elm$svg$Svg$Attributes$y('0'),
								$elm$svg$Svg$Attributes$rx('10'),
								$elm$svg$Svg$Attributes$ry('10'),
								$elm$svg$Svg$Attributes$width('190'),
								$elm$svg$Svg$Attributes$height('265'),
								$elm$svg$Svg$Attributes$fill('none'),
								$elm$svg$Svg$Attributes$strokeWidth('5'),
								$elm$svg$Svg$Attributes$stroke(
								promotionImminent ? $author$project$Main$selectionColor : 'none')
							]),
						_List_Nil)
					]))
			]));
	var pawnView = A2(
		$elm$html$Html$div,
		_List_fromArray(
			[$author$project$Main$centering]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$table,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'width', '80%')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$tr,
						_List_fromArray(
							[$author$project$Main$centering]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '35%')
									]),
								_List_fromArray(
									[
										bscoreboard,
										A2($author$project$Main$scoreboard, $author$project$Board$initPawnCount - model.gs.blackPawnCnt, $author$project$Board$Black)
									])),
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '35%')
									]),
								_List_fromArray(
									[
										wscoreboard,
										A2($author$project$Main$scoreboard, $author$project$Board$initPawnCount - model.gs.whitePawnCnt, $author$project$Board$White)
									])),
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '30%')
									]),
								_List_fromArray(
									[afterlifeRect]))
							]))
					]))
			]));
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				header,
				A2(
				$elm$html$Html$table,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'width', '100%')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '85%')
									]),
								_List_fromArray(
									[
										$author$project$Main$svgBoard(model)
									])),
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '15%'),
										$author$project$Main$centering
									]),
								_List_fromArray(
									[afterlifeRect]))
							]))
					])),
				A2(
				$elm$html$Html$table,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'width', '100%')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '20%')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '60%')
									]),
								_List_fromArray(
									[
										$author$project$Docs$about,
										$author$project$Docs$rules($author$project$Main$Noop),
										$author$project$Docs$notes
									])),
								A2(
								$elm$html$Html$td,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '20%')
									]),
								_List_Nil)
							]))
					])),
				$author$project$Docs$credits
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{init: $author$project$Main$init, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));