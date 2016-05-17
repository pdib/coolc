#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <map>
#include <vector>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

struct MethodDeclaration {
    Symbol return_type;
    std::vector<Symbol> argument_types;

    static MethodDeclaration from_method_class(method_class * method);
    bool has_same_args(MethodDeclaration & other);
    bool matches(std::vector<Symbol>& args);
    std::vector<Symbol> get_undeclared_types(SymbolTable<Symbol, Class__class> & types);
};

using MethodDeclarations_ = std::vector<MethodDeclaration>;
using MethodDeclarations = MethodDeclarations_*;

class ClassTable {
private:
  int semant_errors;
  Classes install_basic_classes(Classes classes);
  ostream& error_stream;
  SymbolTable<Symbol, Class__class> classes_table;
  SymbolTable<Symbol, Entry> symbol_table;
  SymbolTable<Symbol, MethodDeclarations_> method_table;
  
  std::map<Symbol, SymbolTable<Symbol, MethodDeclarations_>> method_tables_by_classes;
  std::map<Symbol, SymbolTable<Symbol, Entry>> symbol_tables_by_classes;

  void decl_class(class__class * current_class);
  void type_check_class(class__class * current_class);
  void type_check_attr(attr_class* current_attr, class__class * current_class);
  Symbol type_check_expression(Expression expr, class__class* current_class, Symbol expected_type);
  void type_check_method(method_class* current_method, class__class * current_class);
  void decl_attr(attr_class* current_attr, class__class* current_class);
  void decl_method(method_class* current_method, class__class* current_class);
  bool is_descendant(Symbol desc, Symbol ancestor, class__class* current_class);

  Symbol type_check_assign(assign_class* assign, class__class* current_class);
  Symbol type_check_static_dispatch(static_dispatch_class* static_dispatch, class__class* current_class);
  Symbol type_check_dispatch(dispatch_class* dispatch, class__class* current_class);
  Symbol type_check_cond(cond_class* cond, class__class* current_class);
  Symbol type_check_typcase(typcase_class* typcase, class__class* current_class);
  Symbol type_check_block(block_class*  block, class__class* current_class);
  Symbol type_check_object(object_class* object, class__class* current_class);
  Symbol type_check_let(let_class* let, class__class* current_class);
  Symbol type_check_new_(new__class* new_, class__class* current_class);
  void type_check_loop(loop_class* loop, class__class* current_class);
  void type_check_eq(eq_class* eq, class__class* current_class);

  Symbol handle_dispatch(
    Expression expr,
    Symbol name,
    Expressions arguments,
    Symbol dispatch_type,
    class__class* current_class);

  Symbol type_union(Symbol t1, Symbol t2, class__class* current_class);
public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif

