#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
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

struct MethodDefinition {
    Symbol return_type;
    std::vector<Symbol> argument_types;

    static MethodDefinition from_method_class(method_class * method);
    bool has_same_args(MethodDefinition const & other);
    std::vector<Symbol> get_undeclared_types(SymbolTable<Symbol, Class__class> & types);
};

using MethodDefinitions_ = std::vector<MethodDefinition>;
using MethodDefinitions = MethodDefinitions_*;

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  SymbolTable<Symbol, Class__class> classes_table;
  SymbolTable<Symbol, Entry> symbol_table;
  SymbolTable<Symbol, MethodDefinitions_> method_table;
  
  void type_check_class(class__class * current_class);
  void type_check_attr(attr_class* current_attr, class__class * current_class);
  Symbol type_check_expression(Expression expr, class__class* current_class, Symbol expected_type);
  void type_check_method(method_class* current_method, class__class * current_class);
  void decl_attr(attr_class* current_attr, class__class* current_class);
  void decl_method(method_class* current_method, class__class* current_class);
  bool is_descendant(Symbol desc, Symbol ancestor);

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif

