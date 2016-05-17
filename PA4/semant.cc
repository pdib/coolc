#include <algorithm>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <vector>
#include <stack>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

#define LOG_ERROR(node)                                 \
    semant_error();                                     \
    error_stream                                        \
    << node->get_filename()         \
    << ":" << node->get_line_number() << ": "

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
    classes = install_basic_classes(classes);

    int classes_len = classes->len();
    
    // Install all classes
    for (int i =  0; i < classes_len; i++) {
        class__class* current_class = static_cast<class__class*>(classes->nth(i));
        class__class* previous_def = static_cast<class__class*>(
            classes_table.lookup(current_class->get_name()));
        if (current_class->get_name() == SELF_TYPE) {
            LOG_ERROR(current_class)
                << "SELF_TYPE is an invalid name for a class." << endl;
            return;
        }

        if (previous_def != NULL) {
            LOG_ERROR(current_class)
                << "Class " << current_class->get_name() << " is already defined." <<endl;
            continue;
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
                             << " cannot inherit class " << parent_symbol
                             << " which was not found."<< endl;
                break;
            }

            if (parent_symbol == Int ||
                parent_symbol == Str ||
                parent_symbol == Bool ||
                parent_symbol == SELF_TYPE) {
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
        return;
    }

    // For each class, traverse the AST and add
    class__class* root_class = static_cast<class__class*>(classes_table.lookup(Object));
    decl_class(root_class);
    type_check_class(root_class);
    
    // Check for Main class presence and for main() method inside it
    class__class* main_class = static_cast<class__class*>(classes_table.lookup(Main));
    if (main_class == NULL) {
        semant_error();
        error_stream << "Class Main is not defined." << endl;
    }
}

void ClassTable::decl_class(class__class * current_class) {
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
    method_tables_by_classes.emplace(current_class->get_name(), method_table);
    symbol_tables_by_classes.emplace(current_class->get_name(), symbol_table);

    for(Symbol child: current_class->children) {
        Class__class* child_class = classes_table.lookup(child);
        decl_class(static_cast<class__class*>(child_class));
    }
    
    method_table.exitscope();
    symbol_table.exitscope();
}

void ClassTable::type_check_class(class__class* current_class) {
    Symbol class_name = current_class->get_name();
    bool should_type_check = class_name != Object &&
        class_name != Int &&
        class_name != Str &&
        class_name != Bool &&
        class_name != IO;
    if (should_type_check) {
        Features features = current_class->get_features();
        int features_len = features->len();
        symbol_table = symbol_tables_by_classes[class_name];
        method_table = method_tables_by_classes[class_name];
        for (int i = 0; i < features_len; i++) {
            Feature feature = features->nth(i);
            switch (feature->feature_type()) {
            case FeatureType::attr:
                type_check_attr(static_cast<attr_class*>(feature), current_class);
                break;
            case FeatureType::method:
                type_check_method(static_cast<method_class*>(feature), current_class);
                break;
            }
        }
    }

    // Type check children classes
    for(Symbol child: current_class->children) {
        Class__class* child_class = classes_table.lookup(child);
        type_check_class(static_cast<class__class*>(child_class));
    }
}

void ClassTable::decl_attr(attr_class* current_attr, class__class* current_class) {
    Symbol attr_type = current_attr->get_type();
    Class_ type = classes_table.lookup(attr_type);
    // SELF_TYPE is  allowed as an attribute type
    if (attr_type != SELF_TYPE && type == NULL) {
        LOG_ERROR(current_class)
            << "Undeclared type " << attr_type << endl;
        return;
    }

    // self is not allowed as an attribute name
    if (current_attr->get_name() == self) {
        LOG_ERROR(current_class)
            << "Class" << current_class->get_name()
            << " has an attribute named self, which is forbidden." << endl;
        return;
    }
    
    Symbol previous_def = symbol_table.lookup(current_attr->get_name());

    if (previous_def != NULL) {
        LOG_ERROR(current_class)
            << "Class " << current_class->get_name()
            << " is redefining attribute " << current_attr->get_name() << endl;
        return;
    }

    MethodDeclarations method_def = method_table.lookup(current_attr->get_name());

    if (method_def != NULL) {
        LOG_ERROR(current_class)
            << "Class " << current_class->get_name()
            << " is trying to redefine method " << current_attr->get_name() << " as an attribute." << endl;
        return;
    }

    symbol_table.addid(current_attr->get_name(), current_attr->get_type());
}

void ClassTable::decl_method(method_class* current_method, class__class* current_class)  {
    Formals formals = current_method->get_formals();
    int formals_len = formals->len();
    std::vector<Symbol> arg_names;
    for(int i = 0; i < formals_len; i++) {
        Symbol name = (static_cast<formal_class*>(formals->nth(i)))->get_name();
        if (name == self) {
            LOG_ERROR(current_class)
                << "Method " << current_class->get_name() << "::" << current_method->get_name()
                << "cannot bind self as a parameter." << endl;
        }
        for (Symbol other_name : arg_names)  {
            if (name == other_name) {
                LOG_ERROR(current_class)
                    << "Method " << current_class->get_name() << "::" << current_method->get_name()
                    << " has several arguments with the name " << name << endl;
                return;
            }
        }
        arg_names.push_back(name);
    }

    MethodDeclaration new_def = MethodDeclaration::from_method_class(current_method);

    std::vector<Symbol> undeclared_types = new_def.get_undeclared_types(classes_table);
    if (undeclared_types.size() > 0) {
        for (Symbol type: undeclared_types) {
            LOG_ERROR(current_class)
                << "Undeclared type " << type << endl;
        }
        return;
    }

    Symbol previous_def = symbol_table.lookup(current_method->get_name());

    if (previous_def != NULL) {
        LOG_ERROR(current_class)
            << "Class " << current_class->get_name()
            << " is trying to redefine attribute " << current_method->get_name() << " as an method." << endl;
        return;
    }

    MethodDeclarations method_def = method_table.lookup(current_method->get_name());
    
    if (method_def == NULL)  {
        method_def = new std::vector<MethodDeclaration>;
        method_table.addid(current_method->get_name(), method_def);
    } else {
        for (MethodDeclaration def : *method_def) {
            // No overriding method with different arguments.
            if (!new_def.has_same_args(def)) {
                    LOG_ERROR(current_class)
                        << "Class " << current_class->get_name()
                        << " is trying to redefine method " << current_method->get_name() << endl;
                    return;
            }
        }
    }
    method_def->push_back(new_def);
}

void ClassTable::type_check_attr(attr_class* current_attr, class__class* current_class) {
    Expression init = current_attr->get_init();
    Symbol attr_type = current_attr->get_type();
    if (init->expression_type() != ExpressionType::no_expr) {
        type_check_expression(init, current_class, attr_type);
    }
}

void ClassTable::type_check_method(method_class* current_method, class__class* current_class) {
    Expression expr = current_method->get_expression();
    Symbol return_type = current_method->get_return_type();
    symbol_table.enterscope();
    Formals formals = current_method->get_formals();
    int formals_len = formals->len();
    for(int i = 0; i < formals_len; i++) {
        Symbol name = (static_cast<formal_class*>(formals->nth(i)))->get_name();
        Symbol type = (static_cast<formal_class*>(formals->nth(i)))->get_type();
        symbol_table.addid(name, type);
    }
    
    type_check_expression(expr, current_class, return_type);
    symbol_table.exitscope();
}

#define TYPE_CHECK_ARITH_EXPR_SAME_TYPE(EXPR, CLASS, TYPE)  TYPE_CHECK_ARITH_EXPR(EXPR, CLASS, TYPE,  TYPE)

#define TYPE_CHECK_ARITH_EXPR(EXPR, CLASS, TYPE, OP_TYPE)   {   \
    final_type = TYPE; \
    Expression left_operand = static_cast<CLASS*>(EXPR)->get_left_operand(); \
    Expression right_operand = static_cast<CLASS*>(EXPR)->get_right_operand(); \
    type_check_expression(left_operand, current_class, OP_TYPE); \
    type_check_expression(right_operand, current_class, OP_TYPE); \
    }

Symbol ClassTable::type_check_expression(Expression expr, class__class* current_class, Symbol expected_type) {
    Symbol final_type;
    
    // Handle different expressions
    switch(expr->expression_type()) {
    case ExpressionType::assign:
        final_type = type_check_assign(static_cast<assign_class*>(expr), current_class);
        break;
    case ExpressionType::static_dispatch:
        final_type = type_check_static_dispatch(static_cast<static_dispatch_class*>(expr), current_class);
        break;
    case ExpressionType::dispatch:
        final_type = type_check_dispatch(static_cast<dispatch_class*>(expr), current_class);
        break;
    case ExpressionType::cond:
        final_type = type_check_cond(static_cast<cond_class*>(expr), current_class);
        break;
    case ExpressionType::typcase:
        final_type = type_check_typcase(static_cast<typcase_class*>(expr), current_class);
        break;
    case ExpressionType::block:
        final_type = type_check_block(static_cast<block_class*>(expr), current_class);
        break;
    case ExpressionType::loop:
        final_type =  Object;
        type_check_loop(static_cast<loop_class*>(expr), current_class);
        break;
    case ExpressionType::new_:
        final_type = type_check_new_(static_cast<new__class*>(expr), current_class);
        break;
    case ExpressionType::object:
        final_type = type_check_object(static_cast<object_class*>(expr), current_class);
        break;
    case ExpressionType::let:
        final_type = type_check_let(static_cast<let_class*>(expr), current_class);
        break;
    case ExpressionType::eq:
        final_type = Bool;
        type_check_eq(static_cast<eq_class*>(expr), current_class);
        break;
    case ExpressionType::lt:
        TYPE_CHECK_ARITH_EXPR(expr, lt_class, Bool, Int);
        break;
    case ExpressionType::leq:
        TYPE_CHECK_ARITH_EXPR(expr, leq_class, Bool, Int);
        break;
    case ExpressionType::neg:
        final_type = Int;
        type_check_expression((static_cast<neg_class*>(expr))->get_operand(), current_class, Int);
        break;
    case ExpressionType::comp:
        final_type = Bool ;
        type_check_expression((static_cast<comp_class*>(expr))->get_operand(), current_class, Bool);
        break;
    case ExpressionType::plus:
        TYPE_CHECK_ARITH_EXPR_SAME_TYPE(expr, plus_class, Int);
        break;
    case ExpressionType::sub:
        TYPE_CHECK_ARITH_EXPR_SAME_TYPE(expr, sub_class, Int);
        break;
    case ExpressionType::mul:
        TYPE_CHECK_ARITH_EXPR_SAME_TYPE(expr, mul_class, Int);
        break;
    case ExpressionType::divide:
        TYPE_CHECK_ARITH_EXPR_SAME_TYPE(expr, divide_class, Int);
        break;
    case ExpressionType::isvoid:
        final_type = Bool;
        type_check_expression(static_cast<isvoid_class*>(expr)->get_operand(), current_class, Object);
        break;
    case ExpressionType::string_const:
        final_type = Str;
        break;
    case ExpressionType::bool_const:
        final_type = Bool;
        break;
    case ExpressionType::int_const:
        final_type = Int;
        break;
    case ExpressionType::no_expr:
        final_type = No_type;
        break;
    case ExpressionType::invalid:
        final_type = Object;
        cerr << "invalid expression" << endl;
        break;
    default:
        cerr << "Unhandled expression type " << endl;
        break;
    }

    // Don't check No_type.
    if (No_type != final_type && !is_descendant(final_type, expected_type, current_class)) {
        LOG_ERROR(current_class)
            << "Cannot convert from " << final_type << " to " << expected_type << endl;
        final_type = Object;
    }
    expr->type = final_type;
    return final_type;
}

bool invalid_comparison(Symbol a, Symbol b) {
    bool has_basic_type = a == Int || a == Str || a == Bool;
    return has_basic_type  && a != b;
}

void ClassTable::type_check_eq(eq_class* eq, class__class* current_class) {
    Symbol left_type = type_check_expression(eq->get_left_operand(), current_class, Object);
    Symbol right_type = type_check_expression(eq->get_right_operand(), current_class, Object);
    if (invalid_comparison(left_type, right_type) || invalid_comparison(right_type, left_type)) {
            LOG_ERROR(current_class)
                << "Illegal comparison between " << left_type << " and " << right_type << endl;
    }
}

Symbol ClassTable::type_check_assign(assign_class* assign, class__class* current_class) {
    Symbol type = Object;
    Symbol name = assign->get_name();
    if (name == self) {
        LOG_ERROR(current_class)
            << "Trying to assign to 'self'." <<  endl;
    } else {        
        type = symbol_table.lookup(name);
        if (type == NULL) {
            LOG_ERROR(current_class)
                << "Undeclared identifier in assignment: " << name << endl;
        }
    }
    Symbol expr_type = type_check_expression(assign->get_expression(), current_class, type);
    return expr_type;
}

Symbol ClassTable::type_check_static_dispatch(static_dispatch_class* static_dispatch, class__class* current_class) {
    // static dispatch part
    Symbol dispatch_type = static_dispatch->get_type();
    class__class* type_ptr = static_cast<class__class*>(classes_table.lookup(dispatch_type));
    if (type_ptr == NULL) {
        LOG_ERROR(current_class)
            << "Undefined type in static dispatch " << dispatch_type << endl;
        return Object;
    }

    Symbol return_type = handle_dispatch(
        static_dispatch->get_object(),
        static_dispatch->get_name(),
        static_dispatch->get_arguments(),
        dispatch_type,
        current_class);

    Expression expr = static_dispatch->get_object();
    Symbol object_type = type_check_expression(expr, current_class, dispatch_type);
    if (return_type == SELF_TYPE) {
        return_type = object_type;
    }
    return return_type;
}

Symbol ClassTable::type_check_dispatch(dispatch_class* dispatch, class__class* current_class) {
    Symbol object_type = type_check_expression(dispatch->get_object(), current_class, Object);
    
    Symbol return_type = handle_dispatch(
        dispatch->get_object(),
        dispatch->get_name(),
        dispatch->get_arguments(),
        object_type,
        current_class);
    
    if (return_type == SELF_TYPE) {
        return_type = object_type;
    }
    return return_type;
}

Symbol ClassTable::handle_dispatch(
    Expression expr,
    Symbol name,
    Expressions arguments,
    Symbol dispatch_type,
    class__class* current_class) {
    std::vector<Symbol> args;
    for (int i = 0; i < arguments->len(); i++) {
        Expression arg = arguments->nth(i);
        Symbol arg_type = type_check_expression(arg, current_class, Object);
        if (arg_type == SELF_TYPE) {
            arg_type = current_class->get_name();
        }
        args.push_back(arg_type);
    }

    if (dispatch_type == SELF_TYPE) {
        dispatch_type = current_class->get_name();
    }
    
    if (method_tables_by_classes.find(dispatch_type) == std::end(method_tables_by_classes))  {
        LOG_ERROR(current_class)
            << "Undefined method call: " << dispatch_type << "::" << name << endl;
        return Object;
    }
    SymbolTable<Symbol, MethodDeclarations_> table = method_tables_by_classes[dispatch_type];
    MethodDeclarations method_decls = table.lookup(name);
    
    if (method_decls == NULL) {
        LOG_ERROR(current_class)
            << "Undefined method call: " << dispatch_type << "::" << name << endl;
        return Object;
    }

    class__class* dispatch_class = static_cast<class__class*>(classes_table.lookup(dispatch_type));
    Symbol return_type = nullptr;
    for (MethodDeclaration current_decl : *method_decls) {
        bool matches = true;
        for (size_t i = 0; i < args.size(); i++) {
            if (!is_descendant(args[i], current_decl.argument_types[i], dispatch_class)) {
                matches = false;
                break;
            }
        }

        if (matches) {
            return_type = current_decl.return_type;
            break;
        }
    }

    if (return_type == nullptr) {
        LOG_ERROR(current_class)
            << "No matching declaration found for " << name
            << " with argument types ( ";
        for(Symbol type : args) {
            error_stream << type << " ";
        }
        error_stream << ")" << endl;
        return_type = Object;
    }

    return return_type;   
}

Symbol ClassTable::type_check_cond(cond_class* cond, class__class* current_class) {
    type_check_expression(cond->get_predicate(), current_class, Bool);
    Symbol then_type = type_check_expression(cond->get_then_expression(), current_class, Object);
    Symbol else_type = type_check_expression(cond->get_else_expression(), current_class, Object);
    return type_union(then_type, else_type, current_class);
}

Symbol ClassTable::type_union(Symbol t1, Symbol t2, class__class* current_class) {
    std::stack<Symbol> t1_heritance_stack, t2_heritance_stack;
    class__class* curr = static_cast<class__class*>(classes_table.lookup(t1));
    while (curr != NULL) {
        t1_heritance_stack.push(curr->get_name());
        curr = static_cast<class__class*>(classes_table.lookup(curr->get_parent()));
    }
    curr = static_cast<class__class*>(classes_table.lookup(t2));
    while (curr != NULL) {
        t2_heritance_stack.push(curr->get_name());
        curr = static_cast<class__class*>(classes_table.lookup(curr->get_parent()));
    }

    Symbol head_type_t1 = t1_heritance_stack.top();
    t1_heritance_stack.pop();
    Symbol head_type_t2 = t2_heritance_stack.top();
    t2_heritance_stack.pop();
    Symbol common_type = head_type_t1;
    while (head_type_t2 == head_type_t1 && !t1_heritance_stack.empty() && !t2_heritance_stack.empty()) {
         head_type_t1 = t1_heritance_stack.top();
         t1_heritance_stack.pop();
         head_type_t2 = t2_heritance_stack.top();
         t2_heritance_stack.pop();
         if (head_type_t2 == head_type_t1) {
             common_type = head_type_t1;
         }
    }
    return common_type;
}

Symbol ClassTable::type_check_typcase(typcase_class* typcase, class__class* current_class) {
    type_check_expression(typcase->get_expression(), current_class, Object);
    Cases cases = typcase->get_cases();
    Symbol return_type = nullptr;
    std::vector<Symbol> branches_types;
    for (int i = 0 ; i < cases->len() ; i++) {
        branch_class* branch = static_cast<branch_class*>(cases->nth(i));
        Symbol identifier = branch->get_name();
        Symbol type = branch->get_type();
        class__class* type_ptr = static_cast<class__class*>(classes_table.lookup(type));
        if (type_ptr == NULL) {
            LOG_ERROR(current_class)
                << "Undefined type in case branch " << type << endl;
            continue;
        }
        if (std::find(begin(branches_types), end(branches_types), type) != end(branches_types)) {
            LOG_ERROR(current_class)
                << "Several branches with the type " << type << endl;
            continue;
        }
        branches_types.push_back(type);
        symbol_table.enterscope();
        symbol_table.addid(identifier, type);
        Symbol branch_type = type_check_expression(branch->get_expression(), current_class, Object);
        symbol_table.exitscope();
        if (return_type == nullptr) {
            return_type = branch_type;
        }
        return_type = type_union(return_type, branch_type, current_class);
    }
    return return_type;
}

Symbol ClassTable::type_check_block(block_class*  block, class__class* current_class) {
    Symbol last_type;
    for (int i = 0; i < block->get_body()->len(); i++) {
        last_type = type_check_expression(block->get_body()->nth(i), current_class, Object);
    }
    return last_type;
}

Symbol ClassTable::type_check_object(object_class* object, class__class* current_class) {
    if (object->get_name() == self) {
        return SELF_TYPE;
    }
    Symbol type = symbol_table.lookup(object->get_name());
    if (type == NULL) {
        LOG_ERROR(current_class)
            << "Undeclared identifier " << object->get_name() << endl;
        type = Object;
    }
    return type;
}

Symbol ClassTable::type_check_let(let_class* let, class__class* current_class) {
    symbol_table.enterscope();
    Symbol type = let->get_type();
    class__class* type_ptr = static_cast<class__class*>(classes_table.lookup(type));
    Symbol identifier = let->get_identifier();
    //  SELF_TYPE is allowed as a let binding
    if (type != SELF_TYPE && type_ptr == NULL) {
        LOG_ERROR(current_class)
            << "Undefined type in let binding " << type << endl;
        return Object;
    }
    if (identifier == self) {
        LOG_ERROR(current_class)
            << "Trying to bind self in let binding" << endl;
        return Object;
    }
    symbol_table.addid(identifier, type);
    type_check_expression(let->get_init(), current_class, type);
    Symbol let_type = type_check_expression(let->get_body(), current_class, Object);
    symbol_table.exitscope();
    return let_type;
}

Symbol ClassTable::type_check_new_(new__class* new_, class__class* current_class) {
    if (new_->get_type() == SELF_TYPE) {
        return SELF_TYPE;
    }
    Symbol type = new_->get_type();
    class__class* type_ptr = static_cast<class__class*>(classes_table.lookup(type));
    if (type_ptr == NULL) {
        LOG_ERROR(current_class)
            << "Undefined type in new expression " << type << endl;
        return Object;
    }
    return type;
}

void ClassTable::type_check_loop(loop_class* loop, class__class* current_class) {
    type_check_expression(loop->get_predicate(), current_class, Bool);
    type_check_expression(loop->get_body(), current_class, Object);
}

bool ClassTable::is_descendant(Symbol desc, Symbol ancestor, class__class* current_class) {
    if (desc == ancestor) {
        return true;
    }
    if (desc == SELF_TYPE) {
        desc = current_class->get_name();
    }
    class__class *current_type = static_cast<class__class*>(classes_table.lookup(desc));
    while (current_type != NULL) {
        if (current_type->get_name()  == ancestor) {
            return true;
        }
        Symbol parent_symbol = current_type->get_parent();
        class__class *parent_type = static_cast<class__class*>(classes_table.lookup(parent_symbol));
        current_type = parent_type;
    }
    return false;
}

Classes ClassTable::install_basic_classes(Classes classes) {
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

    classes_table.addid(prim_slot, class_(prim_slot, NULL, nil_Features(), filename));

    return append_Classes(
        single_Classes(Object_class),
        append_Classes(
            single_Classes(IO_class),
            append_Classes(
                single_Classes(Int_class),
                append_Classes(
                    single_Classes(Bool_class),
                    append_Classes(
                        single_Classes(Str_class),
                        classes)))));
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
     errors. Part 2)  be done in a second stage, when you want
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

MethodDeclaration MethodDeclaration::from_method_class(method_class * method) {
    MethodDeclaration result;
    result.return_type = method->get_return_type();
    Formals formals = method->get_formals();
        
    for (int i = 0; i < formals->len(); i++ ) {
        formal_class* formal = static_cast<formal_class*>(formals->nth(i));
        result.argument_types.push_back(formal->get_type());
    }
        
    return result;
}

bool MethodDeclaration::has_same_args(MethodDeclaration  & other) {
    return matches(other.argument_types);
}

std::vector<Symbol> MethodDeclaration::get_undeclared_types(SymbolTable<Symbol, Class__class> & types) {
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

bool MethodDeclaration::matches(std::vector<Symbol> & args) {
    if (this->argument_types.size() != args.size())  {
        return false;
    }
  for (size_t i = 0; i < this->argument_types.size(); i++)  {
        if (this->argument_types[i] != args[i]) {
            return false;
        }
    }
  return true;
}
