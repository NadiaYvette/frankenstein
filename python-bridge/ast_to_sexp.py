#!/usr/bin/env python3
"""
Frankenstein Python bridge — AST → S-expression dumper.

Reads a Python source file from argv[1] (or stdin if "-") and prints a
single-line S-expression representing a small subset of the AST that the
Haskell-side Python bridge knows how to translate to OrganIR.

Supported subset (enough for arithmetic / recursive integer programs
plus simple records):

  Module(stmts)
  FunctionDef(name, params, body)
  ClassDef(name, fields)           -- record-like: __init__ with self.X = X
  Return(expr|None)
  If(test, then-stmts, else-stmts)
  Assign(target_name, expr)        -- single Name target only
  ExprStmt(expr)
  BinOp(op_name, lhs, rhs)         -- Add Sub Mult FloorDiv Mod
  UnaryOp(op_name, operand)        -- USub
  Compare(lhs, op_name, rhs)       -- Eq NotEq Lt LtE Gt GtE  (single op)
  Constant(int|str|bool)
  Name(id)
  Attr(value, attr)                -- value.attr
  Call(callee_name, args)          -- callee must be a bare Name
  Raise(expr)                      -- raise expr (expr is required)
  Try(body-stmts, except-stmts)    -- single bare `except:` only;
                                   --   no type, no `as`, no else/finally

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

    if isinstance(node, ast.ClassDef):
        # Record-like class: a single __init__ that does `self.X = X` for
        # each parameter. We extract the field names in declaration order.
        if node.bases or node.keywords or node.decorator_list:
            raise NotImplementedError(
                f"class {node.name}: bases/keywords/decorators not supported")
        init = None
        for stmt in node.body:
            if isinstance(stmt, ast.FunctionDef) and stmt.name == "__init__":
                init = stmt
                break
        if init is None:
            raise NotImplementedError(
                f"class {node.name}: must define __init__")
        # Skip 'self' (first arg). Field order = positional arg order.
        fields = [a.arg for a in init.args.args[1:]]
        # Sanity check the body shape: every stmt must be `self.X = X`
        # for some X in fields, in declaration order.
        seen = []
        for stmt in init.body:
            if not isinstance(stmt, ast.Assign): continue
            if len(stmt.targets) != 1: continue
            tgt = stmt.targets[0]
            if (isinstance(tgt, ast.Attribute)
                and isinstance(tgt.value, ast.Name)
                and tgt.value.id == "self"
                and isinstance(stmt.value, ast.Name)
                and stmt.value.id == tgt.attr):
                seen.append(tgt.attr)
        if seen != fields:
            raise NotImplementedError(
                f"class {node.name}: __init__ body must be only "
                f"self.X = X assignments matching the parameters")
        flds = "(" + " ".join(quote(f) for f in fields) + ")"
        return f"(ClassDef {quote(node.name)} {flds})"

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

    if isinstance(node, ast.Attribute):
        return f"(Attr {emit(node.value)} {quote(node.attr)})"

    if isinstance(node, ast.Raise):
        if node.exc is None:
            raise NotImplementedError("bare `raise` (no expression) not supported")
        if node.cause is not None:
            raise NotImplementedError("`raise X from Y` not supported")
        return f"(Raise {emit(node.exc)})"

    if isinstance(node, ast.Try):
        # MVP: exactly one bare `except:` clause, no `as`, no else/finally.
        if node.orelse:
            raise NotImplementedError("try/except `else:` clause not supported")
        if node.finalbody:
            raise NotImplementedError("try/except `finally:` clause not supported")
        if len(node.handlers) != 1:
            raise NotImplementedError("only a single `except:` handler supported")
        h = node.handlers[0]
        if h.type is not None:
            raise NotImplementedError(
                "typed `except SomeError:` not supported — use bare `except:`")
        if h.name is not None:
            raise NotImplementedError("`except ... as e:` binding not supported")
        body = "(" + " ".join(emit(s) for s in node.body) + ")"
        handler = "(" + " ".join(emit(s) for s in h.body) + ")"
        return f"(Try {body} {handler})"

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
