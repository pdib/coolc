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
bool has_child(Symbol s) { for(Symbol c : children) { if (c == s) { return true; } } return false; }

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
virtual void dump_with_types(ostream& ,int) = 0; \
ExpressionType expression_type = ExpressionType::typcase;

#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int); \
ExpressionType expression_type = ExpressionType::branch;


#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; } \
ExpressionType expression_type; 

#define Expression_SHARED_EXTRAS           \
void dump_with_types(ostream&,int);

enum class ExpressionType {
        assign,
        branch,
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

#define assign_EXTRAS ExpressionType expression_type = ExpressionType::assign;
#define dispatch_EXTRAS ExpressionType expression_type = ExpressionType::dispatch;
#define static_dispatch_EXTRAS ExpressionType expression_type = ExpressionType::static_dispatch;
#define loop_EXTRAS ExpressionType expression_type = ExpressionType::loop;
#define block_EXTRAS ExpressionType expression_type = ExpressionType::block;
#define let_EXTRAS ExpressionType expression_type = ExpressionType::let;
#define cond_EXTRAS ExpressionType expression_type = ExpressionType::cond;

#define plus_EXTRAS ExpressionType expression_type = ExpressionType::plus;\
    Expression get_left_operand() { return e1; }                        \
    Expression get_right_operand() { return e2; }

#define sub_EXTRAS ExpressionType expression_type = ExpressionType::sub; \
    Expression get_left_operand() { return e1; }                        \
    Expression get_right_operand() { return e2; }


#define mul_EXTRAS ExpressionType expression_type = ExpressionType::mul; \
    Expression get_left_operand() { return e1; }                        \
    Expression get_right_operand() { return e2; }

#define divide_EXTRAS ExpressionType expression_type = ExpressionType::divide; \
    Expression get_left_operand() { return e1; } \
    Expression get_right_operand() { return e2; }

#define neg_EXTRAS ExpressionType expression_type = ExpressionType::neg; 

#define lt_EXTRAS ExpressionType expression_type = ExpressionType::lt;
#define eq_EXTRAS ExpressionType expression_type = ExpressionType::eq;
#define leq_EXTRAS ExpressionType expression_type = ExpressionType::leq;
#define comp_EXTRAS ExpressionType expression_type = ExpressionType::comp;
#define int_const_EXTRAS ExpressionType expression_type = ExpressionType::int_const;
#define bool_const_EXTRAS ExpressionType expression_type = ExpressionType::bool_const;
#define string_const_EXTRAS ExpressionType expression_type = ExpressionType::string_const;
#define new__EXTRAS ExpressionType expression_type = ExpressionType::new_;
#define isvoid_EXTRAS ExpressionType expression_type = ExpressionType::isvoid;
#define no_expr_EXTRAS ExpressionType expression_type = ExpressionType::no_expr;
#define object_EXTRAS ExpressionType expression_type = ExpressionType::object;

#endif
