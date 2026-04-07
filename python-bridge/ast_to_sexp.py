#!/usr/bin/env python3
"""
Frankenstein Python bridge — AST → S-expression dumper.

Reads a Python source file from argv[1] (or stdin if "-") and prints a
single-line S-expression representing a small subset of the AST that the
Haskell-side Python bridge knows how to translate to OrganIR.

Supported subset (enough for arithmetic / recursive integer programs):

  Module(stmts)
  FunctionDef(name, params, body)
  Return(expr|None)
  If(test, then-stmts, else-stmts)
  Assign(target_name, expr)        -- single Name target only
  ExprStmt(expr)
  BinOp(op_name, lhs, rhs)         -- Add Sub Mult FloorDiv Mod
  UnaryOp(op_name, operand)        -- USub
  Compare(lhs, op_name, rhs)       -- Eq NotEq Lt LtE Gt GtE  (single op)
  Constant(int)                    -- ints only (for now)
  Name(id)
  Call(callee_name, args)          -- callee must be a bare Name

Anything else raises NotImplementedError.
"""

from __future__ import annotations

import ast
import sys


def quote(s: str) -> str:
    out = ['"']
    for ch in s:
        if ch == '\\':
            out.append('\\\\')
        elif ch == '"':
            out.append('\\"')
        elif ch == '\n':
            out.append('\\n')
        elif ch == '\t':
            out.append('\\t')
        else:
            out.append(ch)
    out.append('"')
    return ''.join(out)


def emit(node: ast.AST) -> str:
    if isinstance(node, ast.Module):
        return "(Module " + " ".join(emit(s) for s in node.body) + ")"

    if isinstance(node, ast.FunctionDef):
        params = "(" + " ".join(quote(a.arg) for a in node.args.args) + ")"
        body = "(" + " ".join(emit(s) for s in node.body) + ")"
        return f"(FunctionDef {quote(node.name)} {params} {body})"

    if isinstance(node, ast.Return):
        if node.value is None:
            return "(Return (None))"
        return f"(Return {emit(node.value)})"

    if isinstance(node, ast.If):
        thn = "(" + " ".join(emit(s) for s in node.body) + ")"
        els = "(" + " ".join(emit(s) for s in node.orelse) + ")"
        return f"(If {emit(node.test)} {thn} {els})"

    if isinstance(node, ast.Assign):
        if len(node.targets) != 1 or not isinstance(node.targets[0], ast.Name):
            raise NotImplementedError("only single-Name assignment targets supported")
        return f"(Assign {quote(node.targets[0].id)} {emit(node.value)})"

    if isinstance(node, ast.Expr):
        return f"(ExprStmt {emit(node.value)})"

    if isinstance(node, ast.BinOp):
        return f"(BinOp {quote(type(node.op).__name__)} {emit(node.left)} {emit(node.right)})"

    if isinstance(node, ast.UnaryOp):
        return f"(UnaryOp {quote(type(node.op).__name__)} {emit(node.operand)})"

    if isinstance(node, ast.Compare):
        if len(node.ops) != 1 or len(node.comparators) != 1:
            raise NotImplementedError("only single-operator comparisons supported")
        return (f"(Compare {emit(node.left)} "
                f"{quote(type(node.ops[0]).__name__)} {emit(node.comparators[0])})")

    if isinstance(node, ast.Constant):
        if isinstance(node.value, bool):
            # bool is subclass of int — handle first
            return f"(ConstantBool {'1' if node.value else '0'})"
        if isinstance(node.value, int):
            return f"(Constant {node.value})"
        if isinstance(node.value, str):
            return f"(ConstantStr {quote(node.value)})"
        raise NotImplementedError(f"unsupported constant: {node.value!r}")

    if isinstance(node, ast.Name):
        return f"(Name {quote(node.id)})"

    if isinstance(node, ast.Call):
        if not isinstance(node.func, ast.Name):
            raise NotImplementedError("only direct (Name) call targets supported")
        if node.keywords:
            raise NotImplementedError("keyword arguments not supported")
        args = "(" + " ".join(emit(a) for a in node.args) + ")"
        return f"(Call {quote(node.func.id)} {args})"

    raise NotImplementedError(f"unsupported AST node: {type(node).__name__}")


def main() -> int:
    if len(sys.argv) != 2:
        print("usage: ast_to_sexp.py <file.py|->", file=sys.stderr)
        return 2
    path = sys.argv[1]
    src = sys.stdin.read() if path == "-" else open(path, "r").read()
    try:
        tree = ast.parse(src, filename=path)
        print(emit(tree))
    except SyntaxError as e:
        print(f"SyntaxError: {e}", file=sys.stderr)
        return 1
    except NotImplementedError as e:
        print(f"NotImplementedError: {e}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
