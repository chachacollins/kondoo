import std::io;
import std::collections::list;
import std::collections::map;

enum OptKind
{
    OPT_NONE,
    OPT_SOME,
}

struct Option<Type>
{
    Type val;
    OptKind kind;
}

excuse ACCESSING_OPT_NONE;

fn Type? Option.get_val(&self)
{
    if (self.kind == OPT_NONE) return ACCESSING_OPT_NONE~;
    return self.val;
}

fn bool Option.is_none(&self)
{
    return self.kind == OPT_NONE;
}

enum TokKind
{
    EOF,
    LPAREN,
    RPAREN,
    BSLASH,
    ARROW,
    ILLEGAL,
    IDENT,
}

struct Token
{
    TokKind kind;
    String lexeme;
}

struct Lexer
{
    String source;
    int pos;
    int r_pos;
}

fn void Lexer.skip_whitespace(&self)
{
    while(true)
    {
        char c = self.peek();
        switch (c)
        {
            case ' ': nextcase;
            case '\n': nextcase;
            case '\t': nextcase;
            case '\r': self.advance();
            default: return;
        }
    }
}

fn bool Lexer.is_at_end(&self)
{
    return (self.r_pos >= self.source.len);
}

fn char Lexer.peek(&self)
{
    if(self.is_at_end()) return '\0';
    return self.source[self.r_pos];
}

fn char Lexer.advance(&self)
{
    if (self.is_at_end()) return '\0';
    self.pos = self.r_pos;
    self.r_pos++;
    return self.source[self.pos];
}

fn Token Lexer.make_token(&self, TokKind kind)
{
    return (Token){ kind, string::format(mem, "%c",self.source[self.pos])};
}

fn bool Lexer.match(&self, char c)
{
    if (self.source[self.r_pos] == c)
    {
        self.advance();
        return true;
    }
    else { return false; }
}

fn Token Lexer.to_ident(&self)
{
    DString d = dstring::new(mem);
    while(ascii::is_alnum(self.peek())) d.append_char(self.advance());
    return (Token) {IDENT, d.str_view()};
}

fn Token Lexer.next_token(&self)
{
    self.skip_whitespace();
    if (self.is_at_end()) return self.make_token(TokKind.EOF);
    if (ascii::is_alpha(self.peek())) return self.to_ident();
    char c = self.advance();
    switch(c)
    {
        case '(': return self.make_token(TokKind.LPAREN);
        case ')': return self.make_token(TokKind.RPAREN);
        case '\\': return self.make_token(TokKind.BSLASH);
        case '-': if(self.match('>')) { return self.make_token(TokKind.ARROW);  }
                  else                { return self.make_token(TokKind.ILLEGAL);}
        default: return self.make_token(TokKind.ILLEGAL);
    }
}

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

struct Parser
{
    Lexer *lexer;
    Option{Token} peeked_token;
}

fn Token Parser.peek_token(&self)
{
    if (self.peeked_token.is_none())
    {
        self.peeked_token = { .val = self.lexer.next_token(), .kind = OPT_SOME};
    }
    return self.peeked_token.get_val()!!;
}

fn Token Parser.advance(&self)
{
    if (try tok = self.peeked_token.get_val())
    {
        self.peeked_token.kind = OPT_NONE;
        return tok;
    }
    return self.lexer.next_token();
}

fn Expr* Parser.parse_variable(&self)
{
    Token tok = self.advance();
    return create_variable_expr(tok.lexeme);
}

fn Expr* Parser.parse_fun(&self)
{
    self.advance();
    Token tok = self.peek_token();
    if (tok.kind != TokKind.IDENT)
    {
        io::printn("ERROR: expected identifier after slash");
        return null;
    }
    Token name = self.advance();
    tok = self.peek_token();
    if (tok.kind != TokKind.ARROW)
    {
        io::printn("ERROR: functions only accept one arguement. Expected `->`.");
        return null;
    }
    self.advance();
    Expr* body = self.parse();
    return create_fun_expr(create_variable_expr(name.lexeme), body);
}

fn Expr* Parser.parse_app(&self)
{
    self.advance();
    Expr* lhs = self.parse();
    Token tok = self.peek_token();
    if (tok.kind != TokKind.RPAREN)
    {
        io::printn("ERROR: expected closing paren on the lhs of the expr");
        return null;
    }
    self.advance();
    Expr* rhs = self.parse();
    if (rhs == null)
    {
        io::printn("ERROR: expected a rhs to the lhs of the expr");
        return null;
    }
    return create_app_expr(lhs, rhs);
}

fn Expr* Parser.parse(&self)
{
    Token tok = self.peek_token();
    switch(tok.kind)
    {
        case TokKind.IDENT: return self.parse_variable();
        case TokKind.BSLASH: return self.parse_fun();
        case TokKind.LPAREN: return self.parse_app();
        default: return null;
    }
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

uint counter = 0;

fn String getsym(String s)
{
    return string::format(mem, "%s%d", s, counter++);
}

HashMap{String, String} var_map;

fn void alpha_conversion(Expr* expr)
{
    switch(expr.kind)
    {
        case VAR:
        {
            if (var_map.has_key(expr.as.variable))
            {
                expr.as.variable = var_map.get(expr.as.variable)!!;
            }
            else
            {
                String ns = getsym(expr.as.variable);
                var_map.set(expr.as.variable, ns);
                expr.as.variable = ns;
            }
            return;
        }
        case FUN:
        {
            alpha_conversion(expr.as.fun.head);
            alpha_conversion(expr.as.fun.body);
        }
        case APP:
        {
            alpha_conversion(expr.as.app.lhs);
            alpha_conversion(expr.as.app.rhs);
        }
    }
}

fn Expr* beta_reduce(Expr* expr)
{
    switch(expr.kind)
    {
        case VAR: nextcase;
        case FUN: return expr;
        case APP:
        {
            expr.as.app.lhs = beta_reduce(expr.as.app.lhs);
            if (expr.as.app.lhs.kind != FUN) return expr;
            alpha_conversion(expr.as.app.lhs);
            expr.as.app.rhs = beta_reduce(expr.as.app.rhs);
            Expr* temp = substitute(expr.as.app.lhs, expr.as.app.rhs);
            return beta_reduce(temp);
        }
    }
}

fn Expr* eval(Expr* expr)
{
    return beta_reduce(expr);
}

fn void main()
{
    var_map.init(mem);
    while (true)
    {
        io::print("> ");
        String line = io::readline(mem)!!;
        Lexer lexer = {line, 0, 0};
        Parser parser = {.lexer = &lexer};
        Expr* expr = parser.parse();
        if (expr != null) io::printn(expr_to_dstring(eval(expr)));
        var_map.clear();
    }
}
