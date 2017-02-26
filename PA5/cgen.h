#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <map>
#include <vector>
#include <memory>

enum Basicness
{
  Basic,
  NotBasic
};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol, CgenNode>
{
private:
  List<CgenNode> *nds;
  ostream &str;
  int stringclasstag;
  int intclasstag;
  int boolclasstag;

  // The following methods emit code for
  // constants and global declarations.

  void code_global_data();
  void code_global_text();
  void code_bools(int);
  void code_select_gc();
  void code_constants();

  // The following creates an inheritance graph from
  // a list of classes.  The graph is implemented as
  // a tree of `CgenNode', and class names are placed
  // in the base class symbol table.

  void install_basic_classes();
  void install_class(CgenNodeP nd);
  void install_classes(Classes cs);
  void build_inheritance_tree();
  void set_relations(CgenNodeP nd);

public:
  CgenClassTable(Classes, ostream &str);
  void code();
  CgenNodeP root();
};

struct SymbolLocation {
  virtual void load_symbol(char const * reg, ostream & s) = 0;
  virtual void store_to_symbol(char const * reg, ostream & s) = 0;
};

struct MemberLocation : SymbolLocation{
  MemberLocation(int offset) : offset(offset) {}
  void load_symbol(char const * reg, ostream & s) override;
  void store_to_symbol(char const * reg, ostream & s) override;
  int offset;
};

struct StackLocation : SymbolLocation{
  StackLocation(int offset, char const* from_reg) : offset(offset), from_reg(from_reg) {}
  void load_symbol(char const * reg, ostream & s) override;
  void store_to_symbol(char const * reg, ostream & s) override;
  int offset;
  char const *  from_reg;
};


class CgenNode : public class__class
{
public:
  static int Classtags;
  CgenNode(Class_ c,
           Basicness bstatus,
           CgenClassTableP class_table);

  void add_child(CgenNodeP child);
  List<CgenNode> *get_children() { return children; }
  void set_parentnd(CgenNodeP p);
  CgenNodeP get_parentnd() { return parentnd; }
  int basic() { return (basic_status == Basic); }

  CgenNodeP parentnd;       // Parent of class
  List<CgenNode> *children; // Children of class
  Basicness basic_status;   // `Basic' if class is basic
                            // `NotBasic' otherwise

  void code(ostream &str);
  void emit_method_def(ostream &str, std::string const &label, method_class *method);

  void register_node(ostream &str);
  void register_attribute(attr_class* attr);
  void register_method(method_class *method);

  void emit_init_def(ostream &str);

  // Look for method_name in the method offsets table.
  // If not found, returns the current method offsets table size.
  size_t look_for_method(Symbol method_name);

  int size = 3;
  std::map<attr_class*, int> offsets;
  std::vector<std::string> method_offsets;
  std::map<std::string, method_class *> methods;
  int classtag = CgenNode::Classtags++;

  bool visited = false;

  SymbolTable<Symbol, SymbolLocation> scope;
};

class BoolConst
{
private:
  int val;

public:
  BoolConst(int);
  void code_def(ostream &, int boolclasstag);
  void code_ref(ostream &) const;
};
