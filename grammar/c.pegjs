/*
 * A try to make c grammar for PEGJs parser generator.
 * based on Mouse parser generator grammar 
 * http://www.romanredz.se/freesoft.htm  
 *
*/ 

start 
   = TranslationUnit

//-------------------------------------------------------------------------
//  A.2.4  External definitions
//-------------------------------------------------------------------------

TranslationUnit
    = Spacing ed:ExternalDeclaration+ EOT {
       return ed;
    };

ExternalDeclaration
    = func:FunctionDefinition { return { type: 'function', data: func }; }
    / decl:Declaration { return { type: 'declaration', data: decl }; } 
    ;

FunctionDefinition
    = spec:DeclarationSpecifiers decl:Declarator list:DeclarationList? stat:CompoundStatement {
        return {
          specifiers: spec,
          declarator: decl,
          declarationList: list,
          statement: stat
        };
    };

DeclarationList
    = Declaration+ 
    ;


//-------------------------------------------------------------------------
//  A.2.2  Declarations
//-------------------------------------------------------------------------

Declaration
    = spec:DeclarationSpecifiers init:InitDeclaratorList? SEMI {
         var r = { 
           specifiers: spec
         };
         if(init) r['initList'] = init;
         return r;
    };

DeclarationSpecifiers
    = (head:( StorageClassSpecifier 
       / TypeQualifier
       / FunctionSpecifier 
       )*
       type: TypedefName
       tail:( StorageClassSpecifier 
       / TypeQualifier 
       / FunctionSpecifier 
       )* { var a=head; a=a.concat(type); a=a.concat(tail); return a; }
      )    // {DeclarationSpecifiers}
    / ( StorageClassSpecifier 
      / TypeSpecifier 
      / TypeQualifier 
      / FunctionSpecifier 
      )+    //{DeclarationSpecifiers}
    ;

InitDeclaratorList
    = head:InitDeclarator tail:(COMMA it:InitDeclarator { return it;} )* { return [head].concat(tail);}
    ;

InitDeclarator
    = dec:Declarator init:(EQU init:Initializer {return init;})? { if(init) dec.initializer = init; return dec;}
    ;

StorageClassSpecifier
    = sc: (TYPEDEF
    / EXTERN
    / STATIC
    / AUTO
    / REGISTER
    / ATTRIBUTE LPAR LPAR (!RPAR .)* RPAR RPAR) {
      return {
         type: 'StorageClassSpecifier',
         value: sc
      };
    }
    ;

TypeSpecifier
    = ts:(VOID
    / CHAR
    / SHORT
    / INT
    / LONG
    / FLOAT
    / DOUBLE
    / SIGNED
    / UNSIGNED
    / BOOL
    / COMPLEX
    / StructOrUnionSpecifier
    / EnumSpecifier){
       return {
          type: 'TypeSpecifier',
          value: ts
       };
    };

StructOrUnionSpecifier
    = type:StructOrUnion
      others:( name: Identifier? LWING fields:StructDeclaration+ RWING {return {name: name, fields: fields}}
      / name:Identifier {return {name:name};}
      ) {
        var r = { type: type};
        if(others.name) r.name = others.name;
        if(others.fields) r.fields = others.fields;
        return r;
      }
    ;

StructOrUnion
    = STRUCT
    / UNION
    ;

StructDeclaration
    = spec:SpecifierQualifierList dec: StructDeclaratorList SEMI {
      return {
        spec: spec,
        dec: dec
      }
    };

SpecifierQualifierList
    = ( head:TypeQualifier*
        type:TypedefName
        tail:TypeQualifier*
      ) { return head.concat(type,tail);}
    / ( TypeSpecifier
      / TypeQualifier
      )+
    ;

StructDeclaratorList
    = head:StructDeclarator tail:(COMMA StructDeclarator)* {
      var r = [head];
      if(tail) r = r.concat(tail);
      return r;
    };

StructDeclarator
    = dec: Declarator? COLON val:ConstantExpression { dec.value = val; return dec;}
    / Declarator
    ;

EnumSpecifier
    = type:ENUM
      others:( name: Identifier? LWING list:EnumeratorList COMMA? RWING { return { name: name, list: list }; }
      / name: Identifier { return {name: name}; }
      ){
        var r = { type: type};
        if(others.name) r.name = others.name;
        if(others.list) r.values = others.list;
        return r;
      };

EnumeratorList
    = head:Enumerator tail:(COMMA Enumerator)* {
      var r = [head];
      if(tail) r = r.concat(tail);
      return r;
    };

Enumerator
    = name:EnumerationConstant value:(EQU ConstantExpression)? {
      return {
        name: name,
        value: value
      }; 
    };

TypeQualifier
    = tq:(CONST
    / RESTRICT
    / VOLATILE
    / DECLSPEC LPAR Identifier RPAR ) {
      return {
        type: 'TypeQualifier',
        value: tq
      }
    };

FunctionSpecifier
    = fs:(INLINE
    / STDCALL) {
      return {
        type: 'FunctionSpecifier',
        value: fs
      }
    };

Declarator
    = ref:Pointer? dec:DirectDeclarator {
      if(ref) dec.isRef = true;
      else dec.isRef = false;
      return dec;
    };

DirectDeclarator
    = what:( name:Identifier {return { type: 'Identifier', name: name}; } 
      / LPAR dec:Declarator RPAR { return { type: 'Declarator', value: dec};}
      )
      how:(
        LBRK tq:TypeQualifier* exp:AssignmentExpression? RBRK { return {type: 'array', idxtype:tq, dim:exp }; }
      / LBRK STATIC tq:TypeQualifier* exp:AssignmentExpression RBRK { return {type: 'Sarray', idxtype:tq, dim:exp }; }
      / LBRK tq:TypeQualifier+ STATIC exp:AssignmentExpression RBRK { return {type: 'arrayS', idxtype:tq, dim:exp }; }
      / LBRK tq:TypeQualifier* STAR RBRK { return {type: 'arrayP', idxtype:tq }; }
      / LPAR par:ParameterTypeList RPAR { return {type: 'params', params:par }; }
      / LPAR par:IdentifierList? RPAR { return {type: 'idents', params:par }; }
      )* {
      
      var r = what;
      if(how.length) r.how = how;
      return r;
    };

Pointer
    = ( STAR TypeQualifier* )+
    ;

ParameterTypeList
    = params:ParameterList varlist:(COMMA ELLIPSIS)? {
       if(varlist) params.variable = true;
       return params;
    };

ParameterList
    = head:ParameterDeclaration tail:(COMMA p:ParameterDeclaration { return p;} )* {
      var r = [head];
      if(tail) r = r.concat(tail);
      return r;
    };

ParameterDeclaration
    = decs:DeclarationSpecifiers
      dec:( Declarator
      / AbstractDeclarator
      )? {
         if(dec) decs.push(dec);
         return decs;
      }
    ;

IdentifierList
    = header:Identifier tail:(COMMA Identifier)* {
      var r = [head];
      if(tail) r = r.concat(tail);
      return r;
    };
    

TypeName
    = SpecifierQualifierList AbstractDeclarator?
    ;

AbstractDeclarator
    = Pointer? DirectAbstractDeclarator
    / Pointer
    ;

DirectAbstractDeclarator
    = ( LPAR AbstractDeclarator RPAR
      / LBRK (AssignmentExpression / STAR)? RBRK
      / LPAR ParameterTypeList? RPAR
      )
      ( LBRK (AssignmentExpression / STAR)? RBRK
      / LPAR ParameterTypeList? RPAR
      )*
    ;

TypedefName
    = id:Identifier { return {type:'userTypesSpecifier', value: id};}
    ;

Initializer
    = AssignmentExpression
    / LWING InitializerList COMMA? RWING
    ;

InitializerList
    = Designation? Initializer (COMMA Designation? Initializer)*
    ;

Designation
    = Designator+ EQU
    ;

Designator
    = LBRK ConstantExpression RBRK
    / DOT Identifier
    ;


//-------------------------------------------------------------------------
//  A.2.3  Statements
//-------------------------------------------------------------------------

Statement
    = LabeledStatement
    / CompoundStatement
    / ExpressionStatement
    / SelectionStatement
    / IterationStatement
    / JumpStatement
    ;

LabeledStatement
    = Identifier COLON Statement
    / CASE ConstantExpression COLON Statement
    / DEFAULT COLON Statement
    ;

CompoundStatement
    = LWING ( Declaration / Statement )* RWING
    ;

ExpressionStatement
    = Expression? SEMI
    ;

SelectionStatement
    = IF LPAR Expression RPAR Statement (ELSE Statement)?
    / SWITCH LPAR Expression RPAR Statement
    ;

IterationStatement
    = WHILE LPAR Expression RPAR Statement
    / DO Statement WHILE LPAR Expression RPAR SEMI
    / FOR LPAR Expression? SEMI Expression? SEMI Expression? RPAR Statement
    / FOR LPAR Declaration Expression? SEMI Expression? RPAR Statement
    ;

JumpStatement
    = GOTO Identifier SEMI
    / CONTINUE SEMI
    / BREAK SEMI
    / RETURN Expression? SEMI
    ;


//-------------------------------------------------------------------------
//  A.2.1  Expressions
//-------------------------------------------------------------------------

PrimaryExpression
    = Identifier
    / Constant
    / StringLiteral
    / LPAR Expression RPAR
    ;

PostfixExpression
    = ( PrimaryExpression
      / LPAR TypeName RPAR LWING InitializerList COMMA? RWING
      )
      ( LBRK Expression RBRK
      / LPAR ArgumentExpressionList? RPAR
      / DOT Identifier
      / PTR Identifier
      / INC
      / DEC
      )*
    ;

ArgumentExpressionList
    = AssignmentExpression (COMMA AssignmentExpression)*
    ;

UnaryExpression
    = PostfixExpression
    / INC UnaryExpression
    / DEC UnaryExpression
    / UnaryOperator CastExpression
    / SIZEOF (UnaryExpression / LPAR TypeName RPAR )
    ;

UnaryOperator
    = AND
    / STAR
    / PLUS
    / MINUS
    / TILDA
    / BANG
    ;

CastExpression
    = (LPAR TypeName RPAR)* UnaryExpression
    ;

MultiplicativeExpression
    = CastExpression ((STAR / DIV / MOD) CastExpression)*
    ;

AdditiveExpression
    = MultiplicativeExpression ((PLUS / MINUS) MultiplicativeExpression)*
    ;

ShiftExpression
    = AdditiveExpression ((LEFT / RIGHT) AdditiveExpression)*
    ;

RelationalExpression
    = ShiftExpression ((LE / GE / LT / GT) ShiftExpression)*
    ;

EqualityExpression
    = RelationalExpression ((EQUEQU / BANGEQU) RelationalExpression)*
    ;

ANDExpression
    = EqualityExpression (AND EqualityExpression)*
    ;

ExclusiveORExpression
    = ANDExpression (HAT ANDExpression)*
    ;

InclusiveORExpression
    = ExclusiveORExpression (OR ExclusiveORExpression)*
    ;

LogicalANDExpression
    = InclusiveORExpression (ANDAND InclusiveORExpression)*
    ;

LogicalORExpression
    = LogicalANDExpression (OROR LogicalANDExpression)*
    ;

ConditionalExpression
    = LogicalORExpression (QUERY Expression COLON LogicalORExpression)*
    ;

AssignmentExpression
    = UnaryExpression AssignmentOperator AssignmentExpression
    / ConditionalExpression
    ;

AssignmentOperator
    = EQU
    / STAREQU
    / DIVEQU
    / MODEQU
    / PLUSEQU
    / MINUSEQU
    / LEFTEQU
    / RIGHTEQU
    / ANDEQU
    / HATEQU
    / OREQU
    ;

Expression
    = AssignmentExpression (COMMA AssignmentExpression)*
    ;

ConstantExpression
    = ConditionalExpression
    ;


//-------------------------------------------------------------------------
//  A.1.1  Lexical elements
//  Tokens are: Keyword, Identifier, Constant, StringLiteral, Punctuator.
//  Tokens are separated by Spacing.
//-------------------------------------------------------------------------

Spacing
    = ( WhiteSpace
      / LongComment
      / LineComment
      / Pragma
      )*
    ;

WhiteSpace  = [ \n\r\t\u000B\u000C] ; // 7.4.1.10

LongComment = "/*" (!"*/" .)* "*/" ;   // 6.4.9

LineComment = "//" (!"\n" .)* ;       // 6.4.9

Pragma      = "#"  (!"\n" .)* ;       // Treat pragma as comment


//-------------------------------------------------------------------------
//  A.1.2  Keywords
//-------------------------------------------------------------------------

AUTO      = n:"auto"       !IdChar Spacing {return n;};
BREAK     = n:"break"      !IdChar Spacing {return n;};
CASE      = n:"case"       !IdChar Spacing {return n;};
CHAR      = n:"char"       !IdChar Spacing {return n;};
CONST     = n:"const"      !IdChar Spacing {return n;};
CONTINUE  = n:"continue"   !IdChar Spacing {return n;};
DEFAULT   = n:"default"    !IdChar Spacing {return n;};
DOUBLE    = n:"double"     !IdChar Spacing {return n;};
DO        = n:"do"         !IdChar Spacing {return n;};
ELSE      = n:"else"       !IdChar Spacing {return n;};
ENUM      = n:"enum"       !IdChar Spacing {return n;};
EXTERN    = n:"extern"     !IdChar Spacing {return n;};
FLOAT     = n:"float"      !IdChar Spacing {return n;};
FOR       = n:"for"        !IdChar Spacing {return n;};
GOTO      = n:"goto"       !IdChar Spacing {return n;};
IF        = n:"if"         !IdChar Spacing {return n;};
INT       = n:"int"        !IdChar Spacing {return n;};
INLINE    = n:"inline"     !IdChar Spacing {return n;};
LONG      = n:"long"       !IdChar Spacing {return n;};
REGISTER  = n:"register"   !IdChar Spacing {return n;};
RESTRICT  = n:"restrict"   !IdChar Spacing {return n;};
RETURN    = n:"return"     !IdChar Spacing {return n;};
SHORT     = n:"short"      !IdChar Spacing {return n;};
SIGNED    = n:"signed"     !IdChar Spacing {return n;};
SIZEOF    = n:"sizeof"     !IdChar Spacing {return n;};
STATIC    = n:"static"     !IdChar Spacing {return n;};
STRUCT    = n:"struct"     !IdChar Spacing {return n;};
SWITCH    = n:"switch"     !IdChar Spacing {return n;};
TYPEDEF   = n:"typedef"    !IdChar Spacing {return n;};
UNION     = n:"union"      !IdChar Spacing {return n;};
UNSIGNED  = n:"unsigned"   !IdChar Spacing {return n;};
VOID      = n:"void"       !IdChar Spacing {return n;};
VOLATILE  = n:"volatile"   !IdChar Spacing {return n;};
WHILE     = n:"while"      !IdChar Spacing {return n;};
BOOL      = n:"_Bool"      !IdChar Spacing {return n;};
COMPLEX   = n:"_Complex"   !IdChar Spacing {return n;};
STDCALL   = n:"_stdcall"   !IdChar Spacing {return n;};
DECLSPEC  = n:"__declspec" !IdChar Spacing {return n;};
ATTRIBUTE = n:"__attribute__" !IdChar Spacing {return n;};

Keyword
    = ( "auto"
      / "break"
      / "case"
      / "char"
      / "const"
      / "continue"
      / "default"
      / "double"
      / "do"
      / "else"
      / "enum"
      / "extern"
      / "float"
      / "for"
      / "goto"
      / "if"
      / "int"
      / "inline"
      / "long"
      / "register"
      / "restrict"
      / "return"
      / "short"
      / "signed"
      / "sizeof"
      / "static"
      / "struct"
      / "switch"
      / "typedef"
      / "union"
      / "unsigned"
      / "void"
      / "volatile"
      / "while"
      / "_Bool"
      / "_Complex"
      / "_Imaginary"
      / "_stdcall"
      / "__declspec"
      / "__attribute__"
      )
    !IdChar ;


//-------------------------------------------------------------------------
//  A.1.3  Identifiers
//  The standard does not explicitly state that identifiers must be
//  distinct from keywords, but it seems so.
//-------------------------------------------------------------------------

Identifier 
    = !Keyword first:IdNondigit chars:IdChar* Spacing {
               var id = [first];
               id = id.concat(chars);
               return id.join("");           
    } ;

IdNondigit
    = [a-z] / [A-Z] / [_]
    / UniversalCharacter
    ;

IdChar
    = [a-z] / [A-Z] / [0-9] / [_]
    / UniversalCharacter
    ;


//-------------------------------------------------------------------------
//  A.1.4  Universal character names
//-------------------------------------------------------------------------

UniversalCharacter
    = "\\u" HexQuad
    / "\\U" HexQuad HexQuad
    ;

HexQuad = HexDigit HexDigit HexDigit HexDigit ;


//-------------------------------------------------------------------------
//  A.1.5  Constants
//-------------------------------------------------------------------------

Constant
    = FloatConstant
    / IntegerConstant       // Note: can be a prefix of Float Constant!
    / EnumerationConstant
    / CharacterConstant
    ;

IntegerConstant
    = ( DecimalConstant
      / HexConstant
      / OctalConstant
      )
    IntegerSuffix? Spacing
    ;

DecimalConstant = [1-9][0-9]* ;

OctalConstant   = "0"[0-7]* ;

HexConstant     = HexPrefix HexDigit+ ;

HexPrefix       = "0x" / "0X" ;

HexDigit        = [a-f] / [A-F] / [0-9] ;

IntegerSuffix
    = [uU] Lsuffix?
    / Lsuffix [uU]?
    ;

Lsuffix
    = "ll"
    / "LL"
    / [lL]
    ;

FloatConstant
    = ( DecimalFloatConstant
      / HexFloatConstant
      )
    FloatSuffix? Spacing
    ;

DecimalFloatConstant
    = Fraction Exponent?
    / [0-9]+ Exponent
    ;

HexFloatConstant
    = HexPrefix HexFraction BinaryExponent?
    / HexPrefix HexDigit+ BinaryExponent
    ;

Fraction
    = [0-9]* "." [0-9]+
    / [0-9]+ "."
    ;

HexFraction
    = HexDigit* "." HexDigit+
    / HexDigit+ "."
    ;

Exponent = [eE][+\-]? [0-9]+ ;

BinaryExponent = [pP][+\-]? [0-9]+ ;

FloatSuffix = [flFL] ;

EnumerationConstant = Identifier ;

CharacterConstant = "L"? "'" Char* "'" Spacing ;

Char = Escape / !['\n\\] . ;

Escape
    = SimpleEscape
    / OctalEscape
    / HexEscape
    / UniversalCharacter
    ;

SimpleEscape = "\\" ['\"?\\abfnrtv] ;
OctalEscape  = "\\" [0-7][0-7]?[0-7]? ;
HexEscape    = "\\x" HexDigit+ ;


//-------------------------------------------------------------------------
//  A.1.6  String Literals
//-------------------------------------------------------------------------

StringLiteral = "L"? (["] StringChar* ["] Spacing)+ ;

StringChar = Escape / ![\"\n\\] . ;


//-------------------------------------------------------------------------
//  A.1.7  Punctuators
//-------------------------------------------------------------------------

LBRK       =  "["         Spacing ;
RBRK       =  "]"         Spacing ;
LPAR       =  "("         Spacing ;
RPAR       =  ")"         Spacing ;
LWING      =  "{"         Spacing ;
RWING      =  "}"         Spacing ;
DOT        =  "."         Spacing ;
PTR        =  "->"        Spacing ;
INC        =  "++"        Spacing ;
DEC        =  "--"        Spacing ;
AND        =  "&"  ![&]   Spacing ;
STAR       =  "*"  ![=]   Spacing ;
PLUS       =  "+"  ![+=]  Spacing ;
MINUS      =  "-"  ![\-=>]Spacing ;
TILDA      =  "~"         Spacing ;
BANG       =  "!"  ![=]   Spacing ;
DIV        =  "/"  ![=]   Spacing ;
MOD        =  "%"  ![=>]  Spacing ;
LEFT       =  "<<" ![=]   Spacing ;
RIGHT      =  ">>" ![=]   Spacing ;
LT         =  "<"  ![=]   Spacing ;
GT         =  ">"  ![=]   Spacing ;
LE         =  "<="        Spacing ;
GE         =  ">="        Spacing ;
EQUEQU     =  "=="        Spacing ;
BANGEQU    =  "!="        Spacing ;
HAT        =  "^"  ![=]   Spacing ;
OR         =  "|"  ![=]   Spacing ;
ANDAND     =  "&&"        Spacing ;
OROR       =  "||"        Spacing ;
QUERY      =  "?"         Spacing ;
COLON      =  ":"  ![>]   Spacing ;
SEMI       =  ";"         Spacing ;
ELLIPSIS   =  "..."       Spacing ;
EQU        =  "="  !"="   Spacing ;
STAREQU    =  "*="        Spacing ;
DIVEQU     =  "/="        Spacing ;
MODEQU     =  "%="        Spacing ;
PLUSEQU    =  "+="        Spacing ;
MINUSEQU   =  "-="        Spacing ;
LEFTEQU    =  "<<="       Spacing ;
RIGHTEQU   =  ">>="       Spacing ;
ANDEQU     =  "&="        Spacing ;
HATEQU     =  "^="        Spacing ;
OREQU      =  "|="        Spacing ;
COMMA      =  ","         Spacing ;

EOT        =  !.    ;


