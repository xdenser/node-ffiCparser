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
     source = fs.readFileSync(Hfile),
     tree = parser.parse(source.toString());
   tree.forEach(function(def){
      if(def.type && (def.type=='declaration')){
            parseDeclaration(def.data);          
      } 
   }); 
   return {
        types: types,
        funcs: funcs
   }              
}

function parseDeclaration(dec){
    var hasTypeDef = false, Identifier = null, typeSpec = null, typeSpecs = [], hasParams = null;
    dec.specifiers.forEach(function(spec){
        if( (spec.type=='StorageClassSpecifier') &&
            (spec.value=='typedef')) hasTypeDef = true;
        if( spec.type=='TypeSpecifier' ){
            typeSpec = spec.value;
            typeSpecs.push(spec.value);
        }
        if( spec.type=='userTypeSpecifier'){
            typeSpec = spec.value;
            typeSpecs.push({type:spec.value});
        }    
    });
    if(dec.initList) dec.initList.forEach(function(init){
        if(init.type=='Identifier') Identifier = {
             name: init.name,
             isRef: init.isRef
        }    
        if(init.how && init.how.length){
            if(init.how[0].type=='params'){
                hasParams = init.how[0].params; 
            }
        }
    });
    if(typeSpec.type=='struct'){
        if(!structs[typeSpec.name] && typeSpec.fields ) structs[typeSpec.name] = typeSpec.fields; 
    }
    
    if(hasTypeDef && Identifier && typeSpec){
        types[Identifier.name] = typeSpec2ref(typeSpecs);
        types[Identifier.name].isRef = Identifier.isRef; 
    };
    if(!hasTypeDef && hasParams && Identifier && typeSpec){
        typeSpecs.isRef = Identifier.isRef; 
        funcs[Identifier.name] = {
            input: hasParams,
            output: typeSpec2ref(typeSpecs)  
        };
    }
    
}

function typeSpec2ref(ts){
    var spec = {};
    ts.forEach(function(s){
        if(typeof s === 'string'){
          spec[s] = spec[s]?spec[s]++:1;
          spec.name = s;  
        } else {
          //console.log(s); 
          spec[s.type] = s;  
        } 
    });
    var name = (spec.char?'char':false)||(spec.short?'short':false) || ((spec.long==1)?'long':false) || ((spec.long==2)?'longlong':false) || (spec.int?'int':false); 
    if(name && spec.unsigned) return 'u'+name;
    if(name) return name;
    if(spec.name) return spec.name;
    if(spec.struct){
        var f = spec.struct.fields || structs[spec.struct.name];
        if(!f) throw new Error('Unknown struct '+spec.struct.name);  
        return {
            type: 'struct',
            fields: structFields2ref(f)
        };     
    }
}

function structFields2ref(fields){
    var fs = [];
    fields.forEach(function(f){
       var typeSpec,typeSpecs=[];
       f.spec.forEach(function(s){
        if( s.type=='TypeSpecifier' ){
            typeSpec = s.value;
            typeSpecs.push(s.value);
        }
        if( s.type=='userTypeSpecifier'){
            typeSpec = s.value;
            typeSpecs.push({type:s.value});
        }    
       }); 
       if(typeSpec){
           var type = typeSpec2ref(typeSpecs);
           f.dec.forEach(function(d){
              if(d.type=='Identifier'){ 
                fs.push({
                    name: d.name,
                    type: type,
                    isRef: d.isRef
                }); 
              } 
           });
       }
    });
    return fs;
}

exports.parseSource = _parseSource;
