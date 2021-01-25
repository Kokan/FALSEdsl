#include <iostream>
#include <stdio.h>


#define COMM(str) std::cout << sep << "(" str << ") "; break;

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
  std::cout << sep; putc( c,stdout);
  continue;
}

if ('0' <= c && c <= '9') {
  if (!number) {std::cout << sep; putc(c,stdout); }
  else putc( c, stdout );
  number=true;

  continue;
}

number=false;

  switch (c) {
case '"': string=!string; if (string) std::cout << sep; std::cout << "\""; break;
case '[': std::cout << sep << " [ " ; sep = ""; continue;
case ']': std::cout << "]"; continue;
case ':': COMM("\\:");
case ';': std::cout << sep << " sc"; break;
case '!': COMM("!");
case '+': COMM("+");
case '-': COMM("-");
case '*': COMM("*");
case '/': COMM("/");
case '_': COMM("-");
case '=': COMM("==");
case '>': COMM(">");
case '&': COMM("&");
case '|': COMM("|");
case '~': COMM("\\~");
case '$': COMM("$");
case '%': COMM("%");
case '\\': COMM("\\\\");
case '@': COMM("@");
case L'ø': std::cout << sep << " ao"; break;
case ' ': putc(' ', stdout); continue;
case '\n': putc(' ', stdout); continue;
case '\r': putc(' ', stdout); continue;
case '?' : COMM("?");
case '#' : COMM("#");
case '.' : COMM(".");
case ',' : std::cout << sep << " cc"; break;
case '^' : COMM("^");
case L'ß' : std::cout << sep << " ab"; break;
case -33 : std::cout << sep << " ab"; break;
case '\t': putc(c, stdout); continue;




//a .. z
//integer
//num
default:
  std::cout << sep << int(c); break;
  }
sep = ", ";
}


}
