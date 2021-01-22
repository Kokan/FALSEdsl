#include "falselib.hpp"

#include <iostream>

static void test01()
{
  Universe universe;

  push(universe, make_integer(1));
  push(universe, make_integer(0));

  pick_command(universe);


  SEP top = pop(universe);

  if (top->getAsInt() != 1) { std::cout << "test01 failed" << std::endl; return; }

  top = pop(universe);
  if (top->getAsInt() != 1) { std::cout << "test01 failed" << std::endl; return; }

  std::cout << "test01 passed" << std::endl;
}

static void test02()
{
  Universe universe;

  int stack_nums = 5;
  for (int i = stack_nums; i > 0; --i)
      push(universe, make_integer(i));

  push(universe, make_integer(4));

  pick_command(universe);


  SEP top = pop(universe);
  if (top->getAsInt() != 5) { std::cout << "test02 failed" << std::endl; return; }

  for (int i = 1; i <= stack_nums; ++i) {
   top = pop(universe);
    if (top->getAsInt() != i)  { std::cout << "test02 failed" << std::endl; return; }
  }
  std::cout << "test02 passed" << std::endl;
}

static void test03()
{
  Universe universe;

  int stack_nums = 100;
  for (int i = stack_nums; i > 0; --i)
      push(universe, make_integer(i));

  push(universe, make_integer(32));

  pick_command(universe);


  SEP top = pop(universe);
  if (top->getAsInt() != 33) { std::cout << "test03 failed" << std::endl; return; }

  for (int i = 1; i <= stack_nums; ++i) {
   top = pop(universe);
    if (top->getAsInt() != i)  { std::cout << "test03 failed" << std::endl; return; }
  }
  std::cout << "test03 passed" << std::endl;
}

static void test04()
{
  Universe universe;

  push(universe, make_integer(1));
  push(universe, make_integer(2));
  push(universe, make_integer(1));

  pick_command(universe);

  SEP top = pop(universe);
  if (top->getAsInt() != 1) { std::cout << "test03 failed" << std::endl; return; }

  top = pop(universe);
  if (top->getAsInt() != 2) { std::cout << "test03 failed" << std::endl; return; }

  top = pop(universe);
  if (top->getAsInt() != 1) { std::cout << "test03 failed" << std::endl; return; }

  std::cout << "test03 passed" << std::endl;
}

int main()
{
 test01();
 test02();
 test03();
 test04();

}

