# FALSEdsl
DSL for http://strlen.com/false-language/ in Haskell

+---------------------------------------+
|	FALSE language overview.	|
+---------------------------------------+

syntax:		syntax(ghc):	pops:		pushes:		example:
				-->top		-->top
--------------- --------------- --------------- --------------- -------------------------------

{comment}	not supported	-		-			{ this is a comment }
[code]		[code]		-		function	[1+]	{ (lambda (x) (+ x 1)) }
a .. z		a .. z		-		varadr		a	{ use a: or a; }
integer		integer		-		value		1
'char		PushChar	-		value		'A	{ 65 }
num`		num`		-		-		0`	{ emitword(0) } -- won't support inlinse asm

:		(\:)		n,varadr	-		1a:	{ a:=1 }
;		sc		varadr		varvalue	a;	{ a }
!		(!)		function	-		f;!	{ f() }

+		(+)		n1,n1		n1+n2		1 2+	{ 1+2 }
-		(-)		n1,n2		n1-n2		1 2-
*		(*)		n1,n2		n1*n2		1 2*
/		(/)		n1,n2		n1/n2		1 2/
_		mm		n		-n		1_	{ -1 }

=		(==)		n1,n1		n1=n2		1 2=~	{ 1<>2 }
>		(>)		n1,n2		n1>n2		1 2>

&		(&)		n1,n2		n1 and n2	1 2&	{ 1 and 2 }
|		(\|)		n1,n2		n1 or n2	1 2|
~		(\~)		n		not n		0~	{ -1,TRUE }

$		($)		n		n,n		1$	{ dupl. top stack }
%		(%)		n		-		1%	{ del. top stack }
\		(\\)		n1,n2		n2,n1		1 2\	{ swap }
@		(\@)		n,n1,n2		n1,n2,n		1 2 3@	{ rot }
ø (alt-o)	ao		n		v		1 2 1ø	{ pick }


?		(?)		bool,fun	-		a;2=[1f;!]?  { if a=2 then f(1) }
#		(#)		boolf,fun	-		1[$100<][1+]# { while a<100 do a:=a+1 }

.		(.)		n		-		1.	{ printnum(1) }
"string"	"string"	-		-		"hi!"	{ printstr("hi!") }
,		cc		ch		-		10,	{ putc(10) }
^		(^)		-		ch		^	{ getc() }
ß (alt-s)	ab (alt-s)	-		-		ß	{ flush() }

In GHC commands must be separated with comma(,) and the program must be between brackets [ and ].
Example factorial:

Original
```false
[$1=$[\%1\]?~[$1-f;!*]?]f:
```

GHC embedded:
```haskell
[[($),1,(==),($),[(\\),(%),1,(\\)],(?),(\~),[($),1,(-),f,(;),(!),(*)],(?)],f,(\:)]
```

