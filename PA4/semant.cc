#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <vector>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

#define LOG_ERROR(node) semant_error(); error_stream << "Error in file " << node->get_filename() \
    << " at line " << node->get_line_number() << ": "

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
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
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    classes_table.enterscope();
    symbol_table.enterscope();
    method_table.enterscope();
    install_basic_classes();

    int classes_len = classes->len();
    
    // Install all classes
    for (int i =  0; i < classes_len; i++) {
        class__class* current_class = static_cast<class__class*>(classes->nth(i));
        class__class* previous_def = static_cast<class__class*>(
            classes_table.lookup(current_class->get_name()));
        if (previous_def != NULL) {
            LOG_ERROR(current_class)
                << "Class " << current_class->get_name() << " is already defined.";
        }
        classes_table.addid(current_class->get_name(), current_class);
    }

    // Check inheritance graph
    bool is_inheritance_graph_valid = true;
    for (int i = 0; i < classes_len; i++) {
        class__class* current_class = static_cast<class__class*>(classes->nth(i));
        std::vector<Symbol> parents;
        while (current_class->get_name() != Object) {
            parents.push_back(current_class->get_name());
            Symbol parent_symbol = current_class->get_parent();
            class__class* parent = static_cast<class__class*>(classes_table.lookup(parent_symbol));
            if (parent == NULL)  {
                is_inheritance_graph_valid = false;
                LOG_ERROR(current_class)
                             << "Class " << current_class->get_name()
                             << " inherits from " << parent_symbol
                             << " which was not found."<< endl;
                break;
            }

            if (parent_symbol == Int ||
                parent_symbol == Str ||
                parent_symbol == Bool) {
                is_inheritance_graph_valid = false;
                LOG_ERROR(current_class)
                             << "Class " << current_class->get_name()
                             << " inherits from " << parent_symbol
                             << ", which is not allowed."<< endl;
                break;
            }

            bool inheritance_cycle = false;
            for (size_t j = 0; j < parents.size(); j++) {
                if (parents[j] ==  parent_symbol) {
                    inheritance_cycle = true;
                    LOG_ERROR(current_class)
                             << "Class " << current_class->get_name()
                             << " is involved in an inheritance cycle." << endl;
                    break;
                }
            }

            if (inheritance_cycle) {
                is_inheritance_graph_valid = false;
                break;
            }

            if (!parent->has_child(current_class->get_name())) {
                parent->children.push_back(current_class->get_name());
            }
            
            current_class = parent;
        }
    }

    if (!is_inheritance_graph_valid) {
        error_stream << "Invalid inheritance graph. Aborting compilation." << endl;
        exit(1);
    }

    // For each class, traverse the AST and add
    Class__class* root_class = classes_table.lookup(Object);
    type_check_class(static_cast<class__class*>(root_class));
    
    // Check for Main class presence and for main() method inside it
}

void ClassTable::type_check_class(class__class * current_class) {
    Features features = current_class->get_features();
    int features_len = features->len();
    symbol_table.enterscope();
    method_table.enterscope();
    
    for (int i = 0; i < features_len; i++) {
        Feature feature = features->nth(i);
        switch (feature->feature_type()) {
        case FeatureType::attr:
            decl_attr(static_cast<attr_class*>(feature), current_class);
            break;
        case FeatureType::method:
            decl_method(static_cast<method_class*>(feature), current_class);
            break;
        }
    }

    // Type check children classes
    for(Symbol child: current_class->children) {
        Class__class* child_class = classes_table.lookup(child);
        type_check_class(static_cast<class__class*>(child_class));
    }

    method_table.exitscope();
    symbol_table.exitscope();
}

void ClassTable::decl_attr(attr_class* current_attr, class__class* current_class) {
    Symbol previous_def = symbol_table.lookup(current_attr->get_name());

    if (previous_def != NULL) {
        LOG_ERROR(current_class)
            << "Class " << current_class->get_name()
            << " is redefining attribute " << current_attr->get_name() << endl;
        return;
    }

    MethodDefinitions method_def = method_table.lookup(current_attr->get_name());

    if (method_def != NULL) {
        LOG_ERROR(current_class)
            << "Class " << current_class->get_name()
            << " is trying to redefine method " << current_attr->get_name() << " as an attribute." << endl;
        return;
    }

    Symbol attr_type = current_attr->get_type();
    Class_ type = classes_table.lookup(attr_type);
    // SELF_TYPE is  allowed as an attribute type
    if (attr_type != SELF_TYPE && type == NULL) {
        LOG_ERROR(current_class)
            << "Undeclared type " << attr_type << endl;
        return;
    }
    
    symbol_table.addid(current_attr->get_name(), current_attr->get_type());
}

void ClassTable::decl_method(method_class* current_method, class__class* current_class)  {
    Symbol previous_def = symbol_table.lookup(current_method->get_name());

    if (previous_def != NULL) {
        LOG_ERROR(current_class)
            << "Class " << current_class->get_name()
            << " is trying to redefine attribute " << current_method->get_name() << " as an method." << endl;
        return;
    }

    MethodDefinitions method_def = method_table.lookup(current_method->get_name());
    MethodDefinition new_def = MethodDefinition::from_method_class(current_method);

    std::vector<Symbol> undeclared_types = new_def.get_undeclared_types(classes_table);
    if (undeclared_types.size() > 0) {
        for (Symbol type: undeclared_types) {
            LOG_ERROR(current_class)
                << "Undeclared type " << type << endl;
        }
        return;
    }
    
    if (method_def == NULL)  {
        method_def = new std::vector<MethodDefinition>;
        method_table.addid(current_method->get_name(), method_def);
    } else {
        for (MethodDefinition def : *method_def) {
            if (new_def.has_same_args(def)) {
                if (def.return_type != new_def.return_type) {
                    LOG_ERROR(current_class)
                        << "Class " << current_class->get_name()
                        << " is trying to redefine method " << current_method->get_name() << endl;
                    return;
                }
            }
        }
    }
    method_def->push_back(new_def);
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
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
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
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
	       filename);

    classes_table.addid(Object, Object_class);
    classes_table.addid(IO, IO_class);
    classes_table.addid(Int, Int_class);
    classes_table.addid(Bool, Bool_class);
    classes_table.addid(Str, Str_class);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


MethodDefinition MethodDefinition::from_method_class(method_class * method) {
    MethodDefinition result;
    result.return_type = method->get_return_type();
    Formals formals = method->get_formals();
        
    for (int i = 0; i < formals->len(); i++ ) {
        formal_class* formal = static_cast<formal_class*>(formals->nth(i));
        result.argument_types.push_back(formal->get_type());
    }
        
    return result;
}

bool MethodDefinition::has_same_args(MethodDefinition const & other) {
    if (this->argument_types.size() != other.argument_types.size())  {
        return false;
    }
    for (size_t i = 0; i < this->argument_types.size(); i++)  {
        if (this->argument_types[i] != other.argument_types[i]) {
            return false;
        }
    }
    return true;
}

std::vector<Symbol> MethodDefinition::get_undeclared_types(SymbolTable<Symbol, Class__class> & types) {
    std::vector<Symbol> undeclared_types;
    // SELF_TYPE is allowed as a  return type
    if (return_type != SELF_TYPE && types.lookup(return_type) == NULL) {
        undeclared_types.push_back(return_type);
    }
    for (Symbol arg_type : argument_types) {
        if (types.lookup(arg_type) == NULL) {
            undeclared_types.push_back(arg_type);
        }
    }
    return undeclared_types;
}
