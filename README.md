# FALSEdsl
DSL for http://strlen.com/false-language/ in Haskell

+---------------------------------------+
|	FALSE language overview.	|
+---------------------------------------+

syntax:		pops:		pushes:		example:
		-->top		-->top
--------------- --------------- --------------- -------------------------------

{comment}	-		-			{ this is a comment }
[code]		-		function	[1+]	{ (lambda (x) (+ x 1)) }
a .. z		-		varadr		a	{ use a: or a; }
integer		-		value		1
'char		-		value		'A	{ 65 }
num`		-		-		0`	{ emitword(0) }

:		n,varadr	-		1a:	{ a:=1 }
;		varadr		varvalue	a;	{ a }
!		function	-		f;!	{ f() }

+		n1,n1		n1+n2		1 2+	{ 1+2 }
-		n1,n2		n1-n2		1 2-
*		n1,n2		n1*n2		1 2*
/		n1,n2		n1/n2		1 2/
_		n		-n		1_	{ -1 }

=		n1,n1		n1=n2		1 2=~	{ 1<>2 }
>		n1,n2		n1>n2		1 2>

&		n1,n2		n1 and n2	1 2&	{ 1 and 2 }
|		n1,n2		n1 or n2	1 2|
~		n		not n		0~	{ -1,TRUE }

$		n		n,n		1$	{ dupl. top stack }
%		n		-		1%	{ del. top stack }
\		n1,n2		n2,n1		1 2\	{ swap }
@		n,n1,n2		n1,n2,n		1 2 3@	{ rot }
ø (alt-o)	n		v		1 2 1ø	{ pick }


?		bool,fun	-		a;2=[1f;!]?
						{ if a=2 then f(1) }
#		boolf,fun	-		1[$100<][1+]#
						{ while a<100 do a:=a+1 }

.		n		-		1.	{ printnum(1) }
"string"	-		-		"hi!"	{ printstr("hi!") }
,		ch		-		10,	{ putc(10) }
^		-		ch		^	{ getc() }
ß (alt-s)	-		-		ß	{ flush() }

