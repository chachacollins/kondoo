//TODO: make this capture free
import std::io;
import std::collections::list;

enum ExprKind
{
    VAR,
    FUN,
    APP,
}

alias ExprString = String;

struct ExprApp
{
    Expr* lhs;
    Expr* rhs;
}

alias FunBody = Expr*;

struct ExprFun
{
    Expr* head;
    FunBody body;
}

union As
{
    ExprString variable;
    ExprApp app;
    ExprFun fun;
}

struct Expr
{
    ExprKind kind;
    As as;
}

fn Expr* alloc_expr(ExprKind kind)
{
    Expr* expr = mem::alloc(Expr);
    expr.kind = kind;
    return expr;
}

fn Expr* create_variable_expr(String s)
{
    Expr* expr = alloc_expr(VAR);
    expr.as.variable = s;
    return expr;
}

<* @require head.kind == VAR : "function heads must be variables" *>
fn Expr* create_fun_expr(Expr* head, FunBody body)
{
    Expr* expr = alloc_expr(FUN);
    expr.as.fun.head = head;
    expr.as.fun.body = body;
    return expr;
}

fn Expr* create_app_expr(Expr* lhs, Expr* rhs)
{
    Expr* expr = alloc_expr(APP);
    expr.as.app.lhs = lhs;
    expr.as.app.rhs = rhs;
    return expr;
}

fn void expr_to_dstring_impl(Expr* expr, DString* out)
{
    switch(expr.kind)
    {
        case VAR: out.append_string(expr.as.variable);
        case FUN:
        {
            out.append_char('\\');
            expr_to_dstring_impl(expr.as.fun.head, out);
            out.append_string(" -> ");
            expr_to_dstring_impl(expr.as.fun.body, out);
        }
        case APP:
        {
            out.append_char('(');
            expr_to_dstring_impl(expr.as.app.lhs, out);
            out.append_char(')');
            expr_to_dstring_impl(expr.as.app.rhs, out);
        }
    }
}

fn DString expr_to_dstring(Expr* expr)
{
    DString d;
    expr_to_dstring_impl(expr, &d);
    return d;
}

fn void substitute_impl(Expr* body, Expr* head, Expr* with)
{
    switch(body.kind)
    {
        case VAR:
        {
            if (body.as.variable != head.as.variable) return;
            *body = *with;
        }
        case FUN: substitute_impl(body.as.fun.body, head, with);
        case APP: 
        {
            substitute_impl(body.as.app.lhs, head, with);
            substitute_impl(body.as.app.rhs, head, with);
        }
    }
}

<* @require target.kind == FUN : "only functions can have their body substituted" *>
fn Expr* substitute(Expr* target, Expr* with)
{
    Expr* head = target.as.fun.head;
    substitute_impl(target.as.fun.body, head, with);
    return target.as.fun.body;
}

fn Expr* beta_reduce(Expr* expr)
{
    switch(expr.kind)
    {
        case VAR: nextcase;
        case FUN:
            return expr;
        case APP:
        {
            expr.as.app.lhs = beta_reduce(expr.as.app.lhs);
            if (expr.as.app.lhs.kind != FUN) return expr;
            expr.as.app.rhs = beta_reduce(expr.as.app.rhs);
            Expr* temp = substitute(expr.as.app.lhs, expr.as.app.rhs);
            return beta_reduce(temp);
        }
    }
}

fn void main()
{
    Expr* x = create_variable_expr("x");
    Expr* f = create_fun_expr(x, create_fun_expr(create_variable_expr("z"), create_variable_expr("x")));
    Expr* a = create_app_expr(f, create_variable_expr("y"));
    io::printn(expr_to_dstring(a));
    Expr* z = beta_reduce(a);
    io::printn(expr_to_dstring(z));
    a = create_app_expr(z, create_variable_expr("a"));
    io::printn(expr_to_dstring(a));
    Expr* t =  beta_reduce(a);
    io::printn(expr_to_dstring(t));
}
