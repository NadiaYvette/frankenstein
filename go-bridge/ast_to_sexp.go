// Frankenstein Go bridge — AST → S-expression dumper.
//
// Usage: go run go-bridge/ast_to_sexp.go file.go
//
// Reads a Go source file, parses it via the standard library go/parser,
// and prints a single-line S-expression representing a small subset of
// the AST that the Haskell-side Go bridge knows how to translate to
// OrganIR.
//
// Supported subset (enough for arithmetic / recursive integer programs):
//
//   File          (Module name [decls])
//   FuncDecl      (FuncDecl name [params] body-block)
//   BlockStmt     (Block [stmts])
//   ReturnStmt    (Return expr | (Return (None)))
//   IfStmt        (If test then-block else-block)            -- else-block may be (Block ())
//   AssignStmt    (Assign name expr)                          -- := and = with single Ident lhs
//   ExprStmt      (ExprStmt expr)
//   BasicLit      (Constant integer)
//   Ident         (Name id)
//   BinaryExpr    (BinOp op-string lhs rhs)
//   UnaryExpr     (UnaryOp op-string operand)
//   CallExpr      (Call callee-name [args])                   -- callee must be a bare Ident
//   ParenExpr     transparent (passes through)
//
// Anything else causes an error.

package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"strings"
)

func quote(s string) string {
	var b strings.Builder
	b.WriteByte('"')
	for _, ch := range s {
		switch ch {
		case '\\':
			b.WriteString(`\\`)
		case '"':
			b.WriteString(`\"`)
		case '\n':
			b.WriteString(`\n`)
		case '\t':
			b.WriteString(`\t`)
		default:
			b.WriteRune(ch)
		}
	}
	b.WriteByte('"')
	return b.String()
}

func emit(node ast.Node) (string, error) {
	switch n := node.(type) {

	case *ast.File:
		var parts []string
		parts = append(parts, "Module", quote(n.Name.Name))
		var decls []string
		for _, d := range n.Decls {
			s, err := emit(d)
			if err != nil {
				return "", err
			}
			decls = append(decls, s)
		}
		parts = append(parts, "("+strings.Join(decls, " ")+")")
		return "(" + strings.Join(parts, " ") + ")", nil

	case *ast.FuncDecl:
		if n.Recv != nil {
			return "", fmt.Errorf("methods not supported")
		}
		var params []string
		if n.Type.Params != nil {
			for _, field := range n.Type.Params.List {
				for _, name := range field.Names {
					params = append(params, quote(name.Name))
				}
			}
		}
		body, err := emit(n.Body)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(FuncDecl %s (%s) %s)",
			quote(n.Name.Name), strings.Join(params, " "), body), nil

	case *ast.BlockStmt:
		var stmts []string
		for _, s := range n.List {
			es, err := emit(s)
			if err != nil {
				return "", err
			}
			stmts = append(stmts, es)
		}
		return "(Block (" + strings.Join(stmts, " ") + "))", nil

	case *ast.ReturnStmt:
		if len(n.Results) == 0 {
			return "(Return (None))", nil
		}
		if len(n.Results) > 1 {
			return "", fmt.Errorf("multi-value return not supported")
		}
		e, err := emit(n.Results[0])
		if err != nil {
			return "", err
		}
		return "(Return " + e + ")", nil

	case *ast.IfStmt:
		if n.Init != nil {
			return "", fmt.Errorf("if with init clause not supported")
		}
		test, err := emit(n.Cond)
		if err != nil {
			return "", err
		}
		thn, err := emit(n.Body)
		if err != nil {
			return "", err
		}
		var els string
		if n.Else == nil {
			els = "(Block ())"
		} else {
			// Else may be a *BlockStmt or another *IfStmt. Wrap an *IfStmt
			// in a synthetic block so the Haskell side always sees a block.
			if blk, ok := n.Else.(*ast.BlockStmt); ok {
				els, err = emit(blk)
			} else {
				inner, e := emit(n.Else)
				if e != nil {
					return "", e
				}
				els = "(Block (" + inner + "))"
			}
			if err != nil {
				return "", err
			}
		}
		return fmt.Sprintf("(If %s %s %s)", test, thn, els), nil

	case *ast.AssignStmt:
		if len(n.Lhs) != 1 || len(n.Rhs) != 1 {
			return "", fmt.Errorf("only single-target assignment supported")
		}
		ident, ok := n.Lhs[0].(*ast.Ident)
		if !ok {
			return "", fmt.Errorf("assignment LHS must be an identifier")
		}
		rhs, err := emit(n.Rhs[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(Assign %s %s)", quote(ident.Name), rhs), nil

	case *ast.ExprStmt:
		e, err := emit(n.X)
		if err != nil {
			return "", err
		}
		return "(ExprStmt " + e + ")", nil

	case *ast.BasicLit:
		if n.Kind != token.INT {
			return "", fmt.Errorf("only integer literals supported, got %s", n.Kind)
		}
		return "(Constant " + n.Value + ")", nil

	case *ast.Ident:
		// Built-in true/false → 1/0.
		if n.Name == "true" {
			return "(Constant 1)", nil
		}
		if n.Name == "false" {
			return "(Constant 0)", nil
		}
		return "(Name " + quote(n.Name) + ")", nil

	case *ast.BinaryExpr:
		l, err := emit(n.X)
		if err != nil {
			return "", err
		}
		r, err := emit(n.Y)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(BinOp %s %s %s)", quote(n.Op.String()), l, r), nil

	case *ast.UnaryExpr:
		x, err := emit(n.X)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(UnaryOp %s %s)", quote(n.Op.String()), x), nil

	case *ast.CallExpr:
		ident, ok := n.Fun.(*ast.Ident)
		if !ok {
			return "", fmt.Errorf("only direct (Ident) call targets supported")
		}
		var args []string
		for _, a := range n.Args {
			s, err := emit(a)
			if err != nil {
				return "", err
			}
			args = append(args, s)
		}
		return fmt.Sprintf("(Call %s (%s))", quote(ident.Name), strings.Join(args, " ")), nil

	case *ast.ParenExpr:
		return emit(n.X)

	case *ast.GenDecl:
		// Skip imports / non-function decls silently — return empty marker.
		return "(Skip)", nil

	default:
		return "", fmt.Errorf("unsupported AST node: %T", n)
	}
}

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintln(os.Stderr, "usage: ast_to_sexp <file.go|->")
		os.Exit(2)
	}
	path := os.Args[1]

	fset := token.NewFileSet()
	var src interface{}
	if path == "-" {
		buf := make([]byte, 0, 4096)
		tmp := make([]byte, 4096)
		for {
			n, err := os.Stdin.Read(tmp)
			if n > 0 {
				buf = append(buf, tmp[:n]...)
			}
			if err != nil {
				break
			}
		}
		src = buf
		path = "<stdin>"
	}

	file, err := parser.ParseFile(fset, path, src, parser.SkipObjectResolution)
	if err != nil {
		fmt.Fprintln(os.Stderr, "ParseError:", err)
		os.Exit(1)
	}

	out, err := emit(file)
	if err != nil {
		fmt.Fprintln(os.Stderr, "EmitError:", err)
		os.Exit(1)
	}
	fmt.Println(out)
}
