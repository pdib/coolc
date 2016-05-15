//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include <vector>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

#define Program_EXTRAS                          \
virtual void semant() = 0;			\
virtual void dump_with_types(ostream&, int) = 0; 



#define program_EXTRAS                          \
void semant();     				\
void dump_with_types(ostream&, int);            

#define Class__EXTRAS                   \
virtual Symbol get_filename() = 0;      \
virtual void dump_with_types(ostream&,int) = 0; 


#define class__EXTRAS                                 \
Symbol get_filename() { return filename; }             \
void dump_with_types(ostream&,int);                    \
Symbol get_name() { return this->name; } \
Symbol get_parent() { return this->parent; }  \
Features get_features() { return this->features; } \
std::vector<Symbol> children; \
bool has_child(Symbol s) { for(Symbol c : children) { if (c == s) { return true; } } return false; } \

enum class FeatureType {
    attr,
    method
};


#define Feature_EXTRAS                                        \
virtual void dump_with_types(ostream&,int) = 0;  \
virtual FeatureType  feature_type() = 0;


#define Feature_SHARED_EXTRAS                                       \
void dump_with_types(ostream&,int);    

#define method_EXTRAS \
    FeatureType feature_type() override { return FeatureType::method; }  \
    Symbol get_name() { return name; } \
    Symbol get_return_type() { return return_type; } \
    Formals get_formals()  { return formals; } \
    Expression get_expression() { return expr; } 


#define attr_EXTRAS \
    FeatureType feature_type() override { return FeatureType::attr; } \
    Symbol get_name() { return name; } \
    Symbol get_type() { return type_decl; } \
    Expression get_init() { return init; } 


#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0;


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int); \
Symbol get_name() { return name; } \
Symbol get_type() { return type_decl; } 

#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0; 


#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int); \
Symbol get_name() { return name; } \
Symbol get_type() { return type_decl; }\
Expression get_expression() { return expr; }


#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; } \
virtual ExpressionType expression_type() { return ExpressionType::invalid; }

#define Expression_SHARED_EXTRAS           \
void dump_with_types(ostream&,int);

enum class ExpressionType {
         invalid,
        assign,
        dispatch,
        static_dispatch,
        loop,
        typcase,
        block,
        let,
        cond,
        plus,
        sub,
        mul,
        divide,
        neg,
        lt,
        eq,
        leq,
        comp,
        int_const,
        bool_const,
        string_const,
        new_,
        isvoid,
        no_expr,
        object,
};

#define assign_EXTRAS ExpressionType expression_type() { return ExpressionType::assign; } \
    Symbol get_name() { return name; } \
    Expression get_expression() { return expr; }
    
#define dispatch_EXTRAS ExpressionType expression_type() { return ExpressionType::dispatch; } \
    Expression get_object() { return expr; } \
    Symbol get_name() { return name; } \
    Expressions get_arguments() { return actual; }

#define static_dispatch_EXTRAS ExpressionType expression_type() { return ExpressionType::static_dispatch; } \
    Expression get_object() { return expr; } \
    Symbol get_type() { return type_name; } \
    Symbol get_name() { return name; } \
    Expressions get_arguments() { return actual; }

#define loop_EXTRAS ExpressionType expression_type() { return ExpressionType::loop; } \
    Expression get_predicate() { return pred; } \
    Expression get_body() { return body; }

#define block_EXTRAS ExpressionType expression_type() { return ExpressionType::block; } \
    Expressions get_body() { return body; }

#define let_EXTRAS ExpressionType expression_type() { return ExpressionType::let; } \
    Symbol get_identifier() { return identifier; } \
    Symbol get_type() { return type_decl; }\
    Expression get_init() { return init; } \
    Expression get_body() { return body; }

#define typcase_EXTRAS ExpressionType expression_type() { return ExpressionType::typcase; } \
    Expression get_expression() { return  expr; } \
    Cases get_cases() { return cases; } 

#define cond_EXTRAS ExpressionType expression_type() { return ExpressionType::cond; } \
    Expression get_predicate() { return pred; } \
    Expression get_then_expression() { return then_exp; }\
    Expression get_else_expression() { return else_exp; }

#define plus_EXTRAS ExpressionType expression_type() { return ExpressionType::plus; } \
    Expression get_left_operand() { return e1; }                        \
    Expression get_right_operand() { return e2; }

#define sub_EXTRAS ExpressionType expression_type() { return ExpressionType::sub; } \
    Expression get_left_operand() { return e1; }                        \
    Expression get_right_operand() { return e2; }

#define mul_EXTRAS ExpressionType expression_type() { return ExpressionType::mul; } \
    Expression get_left_operand() { return e1; }                        \
    Expression get_right_operand() { return e2; }

#define divide_EXTRAS ExpressionType expression_type() { return ExpressionType::divide; } \
    Expression get_left_operand() { return e1; } \
    Expression get_right_operand() { return e2; } 

#define neg_EXTRAS ExpressionType expression_type() { return ExpressionType::neg; } \
    Expression get_operand() { return e1; }

#define lt_EXTRAS ExpressionType expression_type() { return ExpressionType::lt; } \
    Expression get_left_operand() { return e1; } \
    Expression get_right_operand() { return e2; }

#define eq_EXTRAS ExpressionType expression_type() { return ExpressionType::eq; } \
    Expression get_left_operand() { return e1; }                                                    \
    Expression get_right_operand() { return e2; }

#define leq_EXTRAS ExpressionType expression_type() { return ExpressionType::leq; } \
    Expression get_left_operand() { return e1; } \
    Expression get_right_operand() { return e2; }

#define comp_EXTRAS ExpressionType expression_type() { return ExpressionType::comp; } \
    Expression get_operand() { return e1; }

#define int_const_EXTRAS ExpressionType expression_type() { return ExpressionType::int_const;}
#define bool_const_EXTRAS ExpressionType expression_type() { return ExpressionType::bool_const;}
#define string_const_EXTRAS ExpressionType expression_type() { return ExpressionType::string_const;}
#define new__EXTRAS ExpressionType expression_type() { return ExpressionType::new_; } \
    Symbol get_type() { return type_name; }

#define isvoid_EXTRAS ExpressionType expression_type() { return ExpressionType::isvoid; } \
    Expression get_operand() { return e1; }

#define no_expr_EXTRAS ExpressionType expression_type() { return ExpressionType::no_expr;}
#define object_EXTRAS ExpressionType expression_type() { return ExpressionType::object; } \
    Symbol get_name() { return name; }

#endif
