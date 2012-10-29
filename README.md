### Overview
C header files parser. A try to automate import function declaration for node-ffi from C header files.


The general idea is having C header like this .h:

``` c
double    do_some_number_fudging(double a, int b);
myobj *   create_object();
double    do_stuff_with_object(myobj *obj);
void      use_string_with_object(myobj *obj, char *value);
void      delete_object(myobj *obj);
```

to get imported library with JavaScript code like this

``` js
var libMylibrary = ffiCparser.import('libmylibrary','sample.h')
```

### Header adaptation
It looks like direct use of C header files is not possible as that would require preprocessing.
So module will rather use specially adapted header files made by copy/pasting needed declarations from real ones.

### Defines
Maybe support of parsing #define directives will be needed as usefull constants usually defined in header files with that directive.
So use case of module will change like that:

``` js
var libDefinition = ffiCparser.parse('sample.h'),
    libMylibrary = ffi.Library('libmylibrary',libDefinition.functions),
     
    dbl = libMylibrary.do_some_number_fudging(0.5,libDefinition.defines.SOME_INTEGER_DEFINITION);
```    

### Current state

Still working on proper PEGJS grammar to get suitable C parser. Library ffiCparser is not ready yet.   
   
     