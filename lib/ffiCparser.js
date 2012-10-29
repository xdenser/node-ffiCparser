var parser = require('./c'),
    fs = require('fs');


var 
 types = {},
 structs = {},
 funcs = {},
 defines = {},
 names = {};


function _parseSource(Hfile){
   var
     source = readFileSync(Hfile),
     tree = parser.parse(source.toString());
   tree.forEach(function(def){
      if(def.type && (def.type=='declaration')){
            parseDeclaration(def.data);          
      } 
   });          
}

function parseDeclaration(dec){
    var hasTypeDef = false, Identifier = null, typeSpec = null;
    dec.specifiers.forEach(function(spec){
        if( (spec.type=='StorageClassSpecifier') &&
            (spec.value=='typedef')) hasTypeDef = true;
        if( spec.type=='TypeSpecifier' || spec.type=='userTypeSpecifier'){
            typeSpec = spec.value;
        }    
    });
    dec.initList.forEach(function(init){
        if(init.type='Identifier') Identifier=init.name;
    });
    if(hasTypeDef && Identifier && typeSpec){
        types[Identifier] = typeSpec;
    }
}

exports.parseSource = _parseSource;
