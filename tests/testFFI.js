var
  p = require('../lib/ffiCparser.js'),
  util= require('util');
  

var 
  types = p.parseSource('test.h');

console.log(util.inspect(types,true,11));  
