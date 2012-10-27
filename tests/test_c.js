var
  p = require('./c.js'),
  fs = require('fs'),
  source = fs.readFileSync('test.h'),
  res = p.parse(source.toString()),
  util = require('util');
  
  console.log(util.inspect(res,true,11));
  
  
  
  
  
 