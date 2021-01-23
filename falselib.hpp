#include <stack>
#include <iostream>
#include <sstream>
#include <map>
#include <memory>
#include <exception>
#include <functional>
#include <vector>

class UnsupportedOperation : public std::exception {};

struct Universe;

class StackEntry {
public:
  StackEntry() {}
  virtual ~StackEntry() {}
  virtual int getAsInt() { throw UnsupportedOperation(); };
  virtual char getAsChar() { throw UnsupportedOperation(); };
  virtual char getAsVaradr() { throw UnsupportedOperation(); };
  virtual void call(Universe &universe) { throw UnsupportedOperation(); }
  virtual StackEntry* clone() = 0;
  virtual std::string show() = 0;
};

typedef std::unique_ptr<StackEntry> SEP;

struct Universe
{
   std::stack<SEP,std::vector<SEP>> stack;
   std::map<char, SEP> variables;
};


class Integer : public StackEntry {
public:
  Integer(int i) : i(i) {}
  virtual ~Integer() {}

  virtual int getAsInt() { return this->i; }
  virtual char getAsChar() { return this->i; }

  virtual StackEntry* clone() { return new Integer(this->i); };
  virtual std::string show() { std::stringstream ss; ss << this->i; return ss.str();  }

private:
  const int i;
};
class Varadr : public StackEntry {
public:
  Varadr(char c) : ch(c) {}
  virtual ~Varadr() {}

  virtual StackEntry* clone() { return new Varadr(this->ch); };

  virtual char getAsChar() { return this->ch; }
  virtual std::string show() { std::stringstream ss; ss << this->ch; return ss.str();  }
private:
  const char ch;
};
class Char : public StackEntry {
public:
  Char(char c) : ch(c) {}
  virtual ~Char() { }

  virtual StackEntry* clone() { return new Char(this->ch); };

  virtual int getAsInt() { return this->ch; };
  virtual char getAsChar() { return this->ch; }
  virtual std::string show() { std::stringstream ss; ss << this->ch; return ss.str();  }
private:
  const char ch;
};
class Function : public StackEntry {
public:
  Function(std::function<void(Universe&)> f) : func(f) {}
  virtual ~Function() {}

  virtual StackEntry* clone() { return new Function(this->func); };
  virtual void call(Universe &universe) { func(universe); }
  virtual std::string show() { return std::string(""); }
private:
  std::function<void(Universe&)> func;
};

SEP make_integer(int i) { return std::unique_ptr<Integer>(new Integer(i)); }
SEP make_varadr(char c) { return std::unique_ptr<Varadr>(new Varadr(c)); }
SEP make_char(char i) { return std::unique_ptr<Char>(new Char(i)); }
SEP make_function(std::function<void(Universe&)> f) { return std::unique_ptr<Function>(new Function(f)); }

SEP clone(StackEntry *self)
{
   StackEntry *clone = self->clone();

   return SEP(clone);
}

void push(Universe &universe, SEP entry)
{
   universe.stack.push(std::move(entry));
}

SEP popAny(std::stack<SEP,std::vector<SEP>> &stack)
{
   StackEntry* res = stack.top().release();
   stack.pop();
   return std::unique_ptr<StackEntry>(res);
}

SEP pop(Universe &universe)
{
   return popAny(universe.stack);
}

static inline void Rot_command(Universe &universe)
{
   auto x = pop(universe);
   auto y = pop(universe);
   auto z = pop(universe);

   push(universe, std::move(y));
   push(universe, std::move(x));
   push(universe, std::move(z));
}

static inline void AssignVar_command(Universe &universe)
{
   SEP key = pop(universe);
   SEP value = pop(universe);

   universe.variables.emplace(key->getAsChar(), std::move(value));
}

static inline void PushVar_command(Universe &universe)
{
   SEP key = pop(universe);

   auto value = universe.variables.at(key->getAsChar())->clone();

   push(universe, SEP(value));
}

static inline void While_command(Universe &universe)
{
  SEP body = pop(universe);
  SEP condf = pop(universe);

  condf->call(universe);
  bool cond = pop(universe)->getAsInt();
  while (cond) {
     body->call(universe);

     condf->call(universe);
     cond = pop(universe)->getAsInt();
  }
}

static inline void If_command(Universe &universe)
{
  SEP body = pop(universe);
  SEP cond = pop(universe);

  if (cond->getAsInt()) {
     body->call(universe);
  }
}

static inline void RunFunction_command(Universe &universe)
{
  SEP function = pop(universe);
  function->call(universe);
}

static inline void Dup_command(Universe &universe)
{
  SEP a = pop(universe);
  SEP cloned = SEP(a->clone());

  push(universe, std::move(a));
  push(universe, std::move(cloned));
}

static inline void Pick_command(Universe &universe)
{
  //TODO: this is shitty code, replace it with direct indexing
  std::vector<SEP> tmp;
  SEP idx = pop(universe);
  int index = idx->getAsInt();

  if (index==0) { Dup_command(universe); return; }

  for (int i = 0; i <= index; ++i)
  {
     tmp.push_back(pop(universe));
  }
  SEP cloned = SEP(tmp.back()->clone());
  for (int i = tmp.size()-1; i >= 0; --i)
  {
     push(universe, std::move(tmp[i]));
  }
  push(universe, std::move(cloned));
}

static inline void Flush_command(Universe &universe)
{
  std::flush(std::cout);
}

static inline void ReadCh_command(Universe &universe)
{
  char a;
  std::cin >> a;

  push(universe, make_char(a));
}

static inline void PrintCh_command(Universe &universe)
{
   SEP a = pop(universe);
   std::cout << a->getAsChar();
}

static inline void Swap_command(Universe &universe)
{
   SEP a = pop(universe);
   SEP b = pop(universe);

   push(universe, std::move(a));
   push(universe, std::move(b));
}

static inline void Del_command(Universe &universe)
{
   pop(universe);
}

static inline void PrintNum_command(Universe &universe)
{
   SEP a = pop(universe);
   std::cout << a->getAsInt();
}


static inline void  debug_universe(Universe &universe)
{
  //TODO: this is shitty code, replace it with direct indexing
  std::cout << "   |stack=[ ";
  if (universe.stack.empty()) return;
  std::vector<SEP> tmp;
  while (!universe.stack.empty())
  {
     tmp.push_back(pop(universe));
  }
  for (int i = tmp.size()-1; i >= 0; --i)
  {
     std::cout << tmp[i]->show() << " ";
     push(universe, std::move(tmp[i]));
  }
}

