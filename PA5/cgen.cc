
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include <stack>
#include <iostream>
#include <sstream>
#include "cgen.h"
#include "cgen_gc.h"
#include <unordered_map>
#include <stack>

extern void emit_string_constant(ostream &str, char *s);
extern int cgen_debug;

static std::unordered_map<Symbol, CgenNodeP> s_ClassNodes;
static SymbolTable<Symbol, SymbolLocation> *current_scope;
static std::stack<int> stack_frame;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg = idtable.add_string("arg");
  arg2 = idtable.add_string("arg2");
  Bool = idtable.add_string("Bool");
  concat = idtable.add_string("concat");
  cool_abort = idtable.add_string("abort");
  copy = idtable.add_string("copy");
  Int = idtable.add_string("Int");
  in_int = idtable.add_string("in_int");
  in_string = idtable.add_string("in_string");
  IO = idtable.add_string("IO");
  length = idtable.add_string("length");
  Main = idtable.add_string("Main");
  main_meth = idtable.add_string("main");
  //   _no_class is a symbol that can't be the name of any
  //   user-defined class.
  No_class = idtable.add_string("_no_class");
  No_type = idtable.add_string("_no_type");
  Object = idtable.add_string("Object");
  out_int = idtable.add_string("out_int");
  out_string = idtable.add_string("out_string");
  prim_slot = idtable.add_string("_prim_slot");
  self = idtable.add_string("self");
  SELF_TYPE = idtable.add_string("SELF_TYPE");
  Str = idtable.add_string("String");
  str_field = idtable.add_string("_str_field");
  substr = idtable.add_string("substr");
  type_name = idtable.add_string("type_name");
  val = idtable.add_string("_val");
}

static char *gc_init_names[] =
    {"_NoGC_Init", "_GenGC_Init", "_ScnGC_Init"};
static char *gc_collect_names[] =
    {"_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect"};

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os)
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes, os);

  os << "\n# end of generated code\n";
}

//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char const *dest_reg, int offset, char const *source_reg, ostream &s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
    << endl;
}

template <typename T>
static void emit_store(T source_reg, int offset, char const *dest_reg, ostream &s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
    << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream &s)
{
  s << LI << dest_reg << " " << val << endl;
}

static void emit_load_address(char *dest_reg, char const *address, ostream &s)
{
  s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char *dest_reg, ostream &s)
{
  s << LA << dest_reg << " ";
}

static void emit_load_bool(char *dest, const BoolConst &b, ostream &s)
{
  emit_partial_load_address(dest, s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream &s)
{
  emit_partial_load_address(dest, s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream &s)
{
  emit_partial_load_address(dest, s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream &s)
{
  s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char *dest, char *src1, ostream &s)
{
  s << NEG << dest << " " << src1 << endl;
}

static void emit_add(char *dest, char *src1, char *src2, ostream &s)
{
  s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char *dest, char *src1, char *src2, ostream &s)
{
  s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(char *dest, char *src1, int imm, ostream &s)
{
  s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char *dest, char *src1, char *src2, ostream &s)
{
  s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char *dest, char *src1, char *src2, ostream &s)
{
  s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char *dest, char *src1, char *src2, ostream &s)
{
  s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char *dest, char *src1, int num, ostream &s)
{
  s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char *dest, ostream &s)
{
  s << JALR << "\t" << dest << endl;
}

static void emit_jal(char const *address, ostream &s)
{
  s << JAL << address << endl;
}

template <typename T>
static void emit_j(T address, ostream &s)
{
  s << "J " << address << endl;
}

static void emit_return(ostream &s)
{
  s << RET << endl;
}

static void emit_gc_assign(ostream &s)
{
  s << JAL << "_GenGC_Assign" << endl;
}

static void emit_disptable_ref(Symbol sym, ostream &s)
{
  s << sym << DISPTAB_SUFFIX;
}

static void emit_init_ref(Symbol sym, ostream &s)
{
  s << sym << CLASSINIT_SUFFIX;
}

template <typename T>
static void emit_label_ref(T l, ostream &s)
{
  s /*<< "label"*/ << l;
}

static void emit_protobj_ref(Symbol sym, ostream &s)
{
  s << sym << PROTOBJ_SUFFIX;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream &s)
{
  s << classname << METHOD_SEP << methodname;
}

template <typename T>
static void emit_label_def(T l, ostream &s)
{
  emit_label_ref(l, s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_branch(int l, ostream &s)
{
  s << BRANCH;
  emit_label_ref(l, s);
  s << endl;
}

static void emit_xor(char *dest, char *src1, char *src2, ostream &s)
{
  s << "xor " << dest << " " << src1 << " " << src2;
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream &str)
{
  emit_store(reg, 0, SP, str);
  emit_addiu(SP, SP, -4, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream &s)
{
  emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream &s)
{
  emit_store(source, DEFAULT_OBJFIELDS, dest, s);
}

static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s);  // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP, SP, 4, s);
  emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char *)A1)
    emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream &s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream &s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                          // label
    << WORD << stringclasstag << endl // tag

    << WORD
    << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + WORD_SIZE) / WORD_SIZE) // size
    << endl

    << WORD << 0 << endl // dispatch table
    << WORD;
  lensym->code_ref(s);
  s << endl;                    // string length
  emit_string_constant(s, str); // ascii string
  s << ALIGN;                   // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream &s, int stringclasstag)
{
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s, stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                           // label
    << WORD << intclasstag << endl                     // class tag
    << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl // object size
    << WORD << 0 << endl                               // dispatch table
    << WORD << str << endl;                            // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s, intclasstag);
}

//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream &s) const
{
  s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream &s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                            // label
    << WORD << boolclasstag << endl                     // class tag
    << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl // object size
    << WORD << 0 << endl                                // dispatch table
    << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main = idtable.lookup_string(MAINNAME);
  Symbol string = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n"
      << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL;
  emit_protobj_ref(main, str);
  str << endl;
  str << GLOBAL;
  emit_protobj_ref(integer, str);
  str << endl;
  str << GLOBAL;
  emit_protobj_ref(string, str);
  str << endl;
  str << GLOBAL;
  falsebool.code_ref(str);
  str << endl;
  str << GLOBAL;
  truebool.code_ref(str);
  str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL
      << WORD << stringclasstag << endl;
}

//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Int"), str);
  str << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("String"), str);
  str << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"), str);
  str << endl
      << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl
      << GLOBAL;
  str << "main" << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str, boolclasstag);
  truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str, stringclasstag);
  inttable.code_string_table(str, intclasstag);
  code_bools(boolclasstag);
}

CgenClassTable::CgenClassTable(Classes classes, ostream &s) : nds(NULL), str(s)
{
  stringclasstag = 2 /* Change to your String class tag here */;
  intclasstag = 1 /* Change to your Int class tag here */;
  boolclasstag = 0 /* Change to your Bool class tag here */;

  enterscope();
  if (cgen_debug)
    cout << "Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  code();
  exitscope();
}

void CgenClassTable::install_basic_classes()
{

  // The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

  //
  // A few special class names are installed in the lookup table but not
  // the class list.  Thus, these classes exist, but are not part of the
  // inheritance hierarchy.
  // No_class serves as the parent of Object and the other special classes.
  // SELF_TYPE is the self class; it cannot be redefined or inherited.
  // prim_slot is a class known to the code generator.
  //
  addid(No_class,
        new CgenNode(class_(No_class, No_class, nil_Features(), filename),
                     Basic, this));
  addid(SELF_TYPE,
        new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename),
                     Basic, this));
  addid(prim_slot,
        new CgenNode(class_(prim_slot, No_class, nil_Features(), filename),
                     Basic, this));

  //
  // The Object class has no parent class. Its methods are
  //        cool_abort() : Object    aborts the program
  //        type_name() : Str        returns a string representation of class name
  //        copy() : SELF_TYPE       returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.
  //
  install_class(
      new CgenNode(
          class_(Object,
                 No_class,
                 append_Features(
                     append_Features(
                         single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                         single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                     single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
                 filename),
          Basic, this));

  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE          writes a string to the output
  //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
  //        in_string() : Str                    reads a string from the input
  //        in_int() : Int                         "   an int     "  "     "
  //
  install_class(
      new CgenNode(
          class_(IO,
                 Object,
                 append_Features(
                     append_Features(
                         append_Features(
                             single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                                    SELF_TYPE, no_expr())),
                             single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                                    SELF_TYPE, no_expr()))),
                         single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                     single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
                 filename),
          Basic, this));

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  install_class(
      new CgenNode(
          class_(Int,
                 Object,
                 single_Features(attr(val, prim_slot, no_expr())),
                 filename),
          Basic, this));

  //
  // Bool also has only the "val" slot.
  //
  install_class(
      new CgenNode(
          class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename),
          Basic, this));

  //
  // The class Str has a number of slots and operations:
  //       val                                  ???
  //       str_field                            the string itself
  //       length() : Int                       length of the string
  //       concat(arg: Str) : Str               string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring
  //
  install_class(
      new CgenNode(
          class_(Str,
                 Object,
                 append_Features(
                     append_Features(
                         append_Features(
                             append_Features(
                                 single_Features(attr(val, Int, no_expr())),
                                 single_Features(attr(str_field, prim_slot, no_expr()))),
                             single_Features(method(length, nil_Formals(), Int, no_expr()))),
                         single_Features(method(concat,
                                                single_Formals(formal(arg, Str)),
                                                Str,
                                                no_expr()))),
                     single_Features(method(substr,
                                            append_Formals(single_Formals(formal(arg, Int)),
                                                           single_Formals(formal(arg2, Int))),
                                            Str,
                                            no_expr()))),
                 filename),
          Basic, this));
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
  {
    return;
  }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd, nds);
  addid(name, nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for (int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i), NotBasic, this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for (List<CgenNode> *l = nds; l; l = l->tl())
    set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

void CgenClassTable::code()
{
  if (cgen_debug)
    cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug)
    cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug)
    cout << "coding constants" << endl;
  code_constants();

  //                 Add your code to emit
  //                   - prototype objects
  //                   - class_nameTab
  //                   - dispatch tables
  //

  root()->register_node(str);

  if (cgen_debug)
    cout << "coding global text" << endl;
  code_global_text();

  root()->code(str);
  //                 Add your code to emit
  //                   - object initializer
  //                   - the class methods
  //                   - etc...
}

CgenNodeP CgenClassTable::root()
{
  return probe(Object);
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

int CgenNode::Classtags = 3;

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct)
    : class__class((const class__class &)*nd),
      parentnd(NULL),
      children(NULL),
      basic_status(bstatus)
{
  stringtable.add_string(name->get_string()); // Add class name to string table
}

void CgenNode::register_node(ostream &str)
{
  s_ClassNodes.emplace(this->name, this);

  // get the parent dispatch table and attributes
  // this is to guarantee the child class has the same offsets in the dispatch table as its parent.
  this->size = this->get_parentnd()->size;
  this->offsets = this->get_parentnd()->offsets;
  this->method_offsets = this->get_parentnd()->method_offsets;
  this->scope = this->get_parentnd()->scope;
  this->scope.enterscope();

  for (int n = 0; n < features->len(); n++)
  {
    auto attr = dynamic_cast<attr_class *>(features->nth(n));
    if (attr)
    {
      register_attribute(attr);
    }

    auto method = dynamic_cast<method_class *>(features->nth(n));
    if (method)
    {
      register_method(method);
    }
  }

  // emit dispatch table
  std::stringstream s;
  s << this->name << "_dispatch_table";
  emit_label_def(s.str(), str);
  for (auto method_name : method_offsets)
  {
    str << WORD << method_name << endl;
  }

  for (auto childs = get_children(); childs; childs = childs->tl())
  {
    childs->hd()->register_node(str);
  }
}

void CgenNode::register_attribute(attr_class *attr)
{
  if (offsets.find(attr) == std::end(offsets))
  {
    offsets.emplace(attr, ++size);
    scope.addid(attr->name, new MemberLocation{size - 1});
  }
}

size_t CgenNode::look_for_method(Symbol method_name)
{
  // Look in the current dispatch table if we find the same method name.
  // If so, replace with this occurence.
  size_t index;
  for (index = 0; index < method_offsets.size(); index++)
  {
    auto mthd_name = method_offsets[index];
    auto stripped_mthd_name = mthd_name.substr(mthd_name.find(".") + 1, mthd_name.length());
    if (stripped_mthd_name == std::string(method_name->get_string()))
    {
      return index;
    }
  }
  return index;
}

void CgenNode::register_method(method_class *method)
{
  std::stringstream s;
  s << this->name << "." << method->name;
  methods.emplace(s.str(), method);

  size_t index = this->look_for_method(method->name);

  if (index == method_offsets.size())
  {
    method_offsets.push_back(s.str());
  }
  else
  {
    method_offsets[index] = s.str();
  }
}

void CgenNode::code(ostream &str)
{
  current_scope = &this->scope;
  this->emit_init_def(str);

  for (auto const &method : this->methods)
  {
    this->emit_method_def(str, method.first, method.second);
  }

  for (
      List<CgenNode> *childs = get_children();
      childs != nullptr;
      childs = childs->tl())
  {
    childs->hd()->code(str);
  }
}

void CgenNode::emit_method_def(ostream &str, std::string const &label, method_class *method)
{
  cout << "Emitting code for " << label << endl;
  emit_label_def(label, str);

  if (label == "Main.main")
  {
    emit_label_def("main", str);
  }

  // emit code for new stack frame
  emit_addiu(SP, SP, -1 * WORD_SIZE, str);
  emit_store(FP, 0, SP, str);
  emit_move(FP, SP, str);

  stack_frame.push(0);
  current_scope->enterscope();
  current_scope->addid(self, new StackLocation{2, FP});
  for (int i = 0; i < method->formals->len(); i++)
  {
    auto formal = static_cast<formal_class *>(method->formals->nth(i));
    current_scope->addid(formal->name, new StackLocation{2 + method->formals->len() - i, FP});
  }

  // emit code for block
  method->expr->code(str);

  // remove stack frame
  emit_move(SP, FP, str);
  emit_load(FP, 0, FP, str);
  emit_addiu(SP, SP, 1 * WORD_SIZE, str);

  stack_frame.pop();
  current_scope->exitscope();
  // emit return code
  emit_return(str);
}

void CgenNode::emit_init_def(ostream &str)
{
  std::stringstream s;
  s << this->name << CLASSINIT_SUFFIX;
  cout << "Emitting code for init method " << s.str() << endl;
  emit_label_def(s.str(), str);

  std::stringstream dispatch_table_sstream;
  dispatch_table_sstream << this->name << "_dispatch_table";

  // Let's try to do this without creating a stack frame here.
  // We know that value stored at the address in 4($sp) is the 'this' pointer here.
  emit_load(T1, 1, SP, str);

  current_scope->enterscope();
  stack_frame.push(0);
  current_scope->addid(self, new StackLocation{1, SP});

  // Store the class header:
  // - the class tag
  emit_addiu(ACC, ZERO, this->classtag, str);
  emit_store(ACC, 0, T1, str);

  // - the object size
  emit_addiu(ACC, ZERO, this->size, str);
  emit_store(ACC, 1, T1, str);

  // - the class dispatch table
  emit_load_address(ACC, dispatch_table_sstream.str().c_str(), str);
  emit_store(ACC, 2, T1, str);

  // init the variables
  for (auto attr_kvp : this->offsets)
  {
    auto attr = attr_kvp.first;
    auto offset = attr_kvp.second;
    attr->init->code(str);

    // result is in ACC. Reload *this* into T1 (might have been modified by attribute init)  
    emit_load(T1, 1, SP, str);
    emit_store(ACC, offset - 1, T1, str);
  }

  // put a marker -1 at the end of the data block.
  // That's to enable GC to reclaim this block.
  emit_addiu(ACC, ZERO, -1, str);
  emit_store(ACC, this->size, T1, str);
  stack_frame.pop();
  current_scope->exitscope();

  emit_return(str);
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void static_dispatch_class::code(ostream &s)
{
  // put arguments on the stack in reverse order
  for (int i = actual->len() - 1; i >= 0; i--)
  {
    actual->nth(i)->code(s);

    emit_addiu(SP, SP, -1 * WORD_SIZE, s);
    emit_store(ACC, 0, SP, s);
  }

  expr->code(s);

  // last argument on the stack is the this pointer
  emit_addiu(SP, SP, -1 * WORD_SIZE, s);
  emit_store(ACC, 0, SP, s);

  // put return address on the stack
  emit_addiu(SP, SP, -1 * WORD_SIZE, s);
  emit_store(RA, 0, SP, s);

  // Jump to label
  std::stringstream method_name;
  method_name << this->type_name << "." << this->name;
  emit_jal(method_name.str().c_str(), s);

  // Pop the return adress to $ra, the this pointer and all the arguments
  emit_load(RA, 0, SP, s);
  emit_addiu(SP, SP, (2 + actual->len()) * WORD_SIZE, s);
}

void dispatch_class::code(ostream &s)
{
  // put arguments on the stack in reverse order
  for (int i = actual->len() - 1; i >= 0; i--)
  {
    actual->nth(i)->code(s);

    emit_addiu(SP, SP, -1 * WORD_SIZE, s);
    emit_store(ACC, 0, SP, s);
  }

  expr->code(s);

  // last argument on the stack is the this pointer
  emit_addiu(SP, SP, -1 * WORD_SIZE, s);
  emit_store(ACC, 0, SP, s);

  // put return address on the stack
  emit_addiu(SP, SP, -1 * WORD_SIZE, s);
  emit_store(RA, 0, SP, s);

  // result should be in ACC now. Let's get its class dispatch table
  // Dispatch table is at 2*WORD_SIZE(ACC)
  emit_load(ACC, 2, ACC, s);

  // Dispatch table address is now in ACC. The method we want is at a fixed offset in that table
  auto node = s_ClassNodes[expr->get_type()];
  size_t offset = node->look_for_method(this->name);

  emit_load(ACC, offset, ACC, s);

  // Call the method
  emit_jalr(ACC, s);

  // Pop the return adress to $ra, the this pointer and all the arguments
  emit_load(RA, 0, SP, s);
  emit_addiu(SP, SP, (2 + actual->len()) * WORD_SIZE, s);
}

int cond_class::s_count = 0;
void cond_class::code(ostream &s)
{
  std::stringstream send, selse, sif;
  sif << "if_" << cond_class::s_count;
  selse << "else_" << cond_class::s_count;
  send << "endif_" << cond_class::s_count;

  pred->code(s);
  emit_beqz(ACC, 2, s);
  emit_j(sif.str(), s);
  emit_j(selse.str(), s);

  emit_label_def(sif.str(), s);
  then_exp->code(s);
  emit_j(send.str(), s);

  emit_label_def(selse.str(), s);
  else_exp->code(s);
  emit_j(send.str(), s);

  emit_label_def(send.str(), s);

  cond_class::s_count++;
}

int loop_class::s_count = 0;
void loop_class::code(ostream &s)
{
  std::stringstream send, sbegin;
  sbegin << "loop_pred_" << loop_class::s_count;
  send << "loop_end_" << loop_class::s_count;

  emit_label_def(sbegin.str(), s);
  pred->code(s);
  emit_bne(ACC, ZERO, 2, s);
  emit_j(send.str(), s);

  body->code(s);
  emit_j(sbegin.str(), s);
  emit_label_def(send.str(), s);

  loop_class::s_count++;
}

int typcase_class::s_count = 0;
void typcase_class::code(ostream &s)
{
  std::stringstream send;
  send << "typcase_end_" << typcase_class::s_count;
  expr->code(s);

  // Load the type identifier of the expression in T1
  emit_load(T1, 0, ACC, s);

  // Store ACC somewhere that will be referenced in the selected case branch
  int offset = ++stack_frame.top();
  emit_store(ACC, offset, FP, s);
  emit_addiu(SP, SP, -1 * WORD_SIZE, s);

  for (int i = 0; i < cases->len(); i++)
  {
    auto branch = static_cast<branch_class *>(cases->nth(i));
    std::stringstream scase;
    scase << "typcase_branch_" << typcase_class::s_count << "_" << branch->type_decl;
    emit_addiu(ACC, ZERO, s_ClassNodes[branch->type_decl]->classtag, s);
    emit_bne(ACC, ZERO, 2, s);
    emit_j(scase.str(), s);
  }

  // Need to emit a runtime error here if no branch matched

  for (int i = 0; i < cases->len(); i++)
  {
    auto branch = static_cast<branch_class *>(cases->nth(i));
    std::stringstream scase;
    scase << "typcase_branch_" << typcase_class::s_count << "_" << branch->type_decl;

    // need to create a new name in the scope that refers to the previously computed expression.
    current_scope->enterscope();
    current_scope->addid(branch->name, new StackLocation{offset, FP});
    emit_label_def(scase.str(), s);
    branch->expr->code(s);
    emit_j(send.str(), s);

    current_scope->exitscope();
  }

  emit_addiu(SP, SP, 1 * WORD_SIZE, s);
  --stack_frame.top();
  emit_label_def(send.str(), s);
  typcase_class::s_count++;
}

void block_class::code(ostream &s)
{
  for (int i = body->first(); i < body->len(); i++)
  {
    body->nth(i)->code(s);
  }
}

void assign_class::code(ostream &s)
{
  expr->code(s);
  emit_move(T1, ACC, s);
  auto location = current_scope->lookup(name);
  location->load_symbol(ACC, s);
}

void let_class::code(ostream &s)
{
  current_scope->enterscope();
  init->code(s);
  int offset = ++stack_frame.top();
  emit_store(ACC, offset, FP, s);
  emit_addiu(SP, SP, -1 * WORD_SIZE, s);
  current_scope->addid(identifier, new StackLocation{offset, FP});
  
  body->code(s);

  emit_addiu(SP, SP, 1 * WORD_SIZE, s);
  --stack_frame.top();
  current_scope->exitscope();
}

void plus_class::code(ostream &s)
{
  e1->code(s);
  emit_move(T1, ACC, s);
  e2->code(s);
  emit_add(ACC, T1, ACC, s);
}

void sub_class::code(ostream &s)
{
  e1->code(s);
  emit_move(T1, ACC, s);
  e2->code(s);
  emit_sub(ACC, T1, ACC, s);
}

void mul_class::code(ostream &s)
{
  e1->code(s);
  emit_move(T1, ACC, s);
  e2->code(s);
  emit_mul(ACC, T1, ACC, s);
}

void divide_class::code(ostream &s)
{
  e1->code(s);
  emit_move(T1, ACC, s);
  e2->code(s);
  emit_div(ACC, T1, ACC, s);
}

void neg_class::code(ostream &s)
{
  e1->code(s);
  emit_xor(T1, T1, T1, s);
  emit_sub(ACC, T1, ACC, s);
}

void lt_class::code(ostream &s)
{
  e1->code(s);
  emit_move(T1, ACC, s);
  e2->code(s);

  emit_blt(ACC, T1, 3, s);
  emit_xor(ACC, ACC, ACC, s);
  emit_beq(ACC, ZERO, 2, s);
  emit_addiu(ACC, ZERO, 1, s);
}

void eq_class::code(ostream &s)
{
  e1->code(s);
  emit_move(T1, ACC, s);
  e2->code(s);

  emit_beq(ACC, T1, 3, s);
  emit_xor(ACC, ACC, ACC, s);
  emit_beq(ACC, ZERO, 2, s);
  emit_addiu(ACC, ZERO, 1, s);
}

void leq_class::code(ostream &s)
{
  e1->code(s);
  emit_move(T1, ACC, s);
  e2->code(s);

  emit_bleq(ACC, T1, 3, s);
  emit_xor(ACC, ACC, ACC, s);
  emit_beq(ACC, ZERO, 2, s);
  emit_addiu(ACC, ZERO, 1, s);
}

void comp_class::code(ostream &s)
{
  e1->code(s);
  emit_addiu(T1, ZERO, 1, s);
  emit_xor(ACC, ACC, T1, s);
}

void int_const_class::code(ostream &s)
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
  emit_load(ACC, 3, ACC, s);
}

void string_const_class::code(ostream &s)
{
  emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(ostream &s)
{
  emit_load_bool(ACC, BoolConst(val), s);
  emit_load(ACC, 3, ACC, s);
}

void new__class::code(ostream &s)
{
  emit_addiu(SP, SP, -1 * WORD_SIZE, s);
  emit_load_address(ACC, "heap_start", s);
  emit_store(ACC, 0, SP, s);

  // put return address on the stack
  emit_addiu(SP, SP, -1 * WORD_SIZE, s);
  emit_store(RA, 0, SP, s);

  std::stringstream init_method_label;
  init_method_label << this->type_name << "_init";
  emit_jal(init_method_label.str().c_str(), s);

  // Pop the return adress to $ra and all the arguments
  // Pop the this pointer to $a0
  emit_load(RA, 0, SP, s);
  emit_addiu(SP, SP, 1 * WORD_SIZE, s);
  emit_load(ACC, 0, SP, s);
  emit_addiu(SP, SP, 1 * WORD_SIZE, s);
}

void isvoid_class::code(ostream &s)
{
  // 0 == null
  e1->code(s);
  emit_beq(ACC, ZERO, 3, s);
  emit_xor(ACC, ACC, ACC, s);
  emit_beq(ACC, ZERO, 2, s);
  emit_addiu(ACC, ZERO, 1, s);
}

void no_expr_class::code(ostream &s)
{
  emit_xor(ACC, ACC, ACC, s);
}

void object_class::code(ostream &s)
{
  auto location = current_scope->lookup(name);
  location->load_symbol(ACC, s);
}

void MemberLocation::load_symbol(char const *reg, ostream &s)
{
  auto self_location = current_scope->lookup(self);
  self_location->load_symbol(reg, s);
  emit_load(reg, offset, reg, s);
}

void MemberLocation::store_to_symbol(char const *reg, ostream &s)
{
  auto self_location = current_scope->lookup(self);
  self_location->load_symbol(T2, s);
  emit_store(reg, offset, T2, s);
}

void StackLocation::load_symbol(char const *reg, ostream &s)
{
  emit_load(reg, offset, from_reg, s);
}

void StackLocation::store_to_symbol(char const *reg, ostream &s)
{
  emit_store(reg, offset, from_reg, s);
}
