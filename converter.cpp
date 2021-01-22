#include <iostream>
#include <stdio.h>


#define COMM(str) std::cout << sep << " " str; break;

int main()
{


  char c;
int comment = 0;
bool string = false;
bool number = false;
const char *sep = "";

  while ((c = getc(stdin)) != EOF)
{
if (c=='{') { ++comment; continue; }
if (c=='}') { --comment; continue; }
if (comment>0) continue;
if (string && c != '"') { putc(c,stdout); continue; }

if ('a' <= c && c <= 'z') {
  std::cout << sep << " PushChar '"; putc( c,stdout); std::cout << "'";
  continue;
}

if ('0' <= c && c <= '9') {
  if (!number) {std::cout << sep << " PushInteger "; putc(c,stdout); }
  else putc( c, stdout );
  number=true;

  continue;
}

number=false;

  switch (c) {
case '"': string=!string; if (string) std::cout << sep << " PrintStr "; std::cout << "\""; break;
case '[': std::cout << sep << " PushFunction [ " ;break;
case ']': std::cout << "]"; break;
case ':': std::cout << sep << " AssignVar"; break;
case ';': std::cout << sep << " PushVar"; break;
case '!': std::cout << sep << " RunFunction"; break;
case '+': COMM("Add");
case '-': COMM("Sub");
case '*': COMM("Mul");
case '/': COMM("Div");
case '_': COMM("Minus");
case '=': COMM("Equal");
case '>': COMM("Larger");
case '&': COMM("And");
case '|': COMM("Or");
case '~': COMM("Not");
case '$': COMM("Dup");
case '%': COMM("Del");
case '\\': COMM("Swap");
case '@': COMM("Rot");
case L'ø': COMM("Pick");
case ' ': putc(' ', stdout); break;
case '\n': putc(' ', stdout); break;
case '\r': putc(' ', stdout); break;
case '?' : COMM("If");
case '#' : COMM("While");
case '.' : COMM("PrintNum");
case ',' : COMM("PrintCh");
case '^' : COMM("ReadCh");
case L'ß' : COMM("Flush");



//a .. z
//integer
//num
default:
  std::cout << sep << c; break;
  }
sep = ", ";
}


}
