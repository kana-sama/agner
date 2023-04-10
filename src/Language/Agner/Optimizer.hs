module Language.Agner.Optimizer where

import Language.Agner.Syntax

optimize :: Module -> Module
optimize = 
  resolveTailCalls

resolveTailCalls :: Module -> Module
resolveTailCalls MkModule{decls} = MkModule{decls = [go_decl d | d <- decls]}
  where
    go_decl :: FunDecl -> FunDecl
    go_decl decl = decl{clauses = [go_clause c | c <- decl.clauses]}

    go_clause :: FunClause -> FunClause
    go_clause clause = let ?funid = clause.funid in clause{body = go_body clause.body}

    go_body :: (?funid :: FunId) => [Expr] -> [Expr]
    go_body [expr] = [go_expr expr]
    go_body [] = []
    go_body (e:es) = e : go_body es

    go_expr :: (?funid :: FunId) => Expr -> Expr
    go_expr = \case
      Apply _ funid args | ?funid == funid -> Apply TailCall funid args
      Receive cases -> Receive [(p, go_body es) | (p, es) <- cases]
      AndAlso e1 e2 -> AndAlso e1 (go_expr e2)
      Begin es -> Begin (go_body es)
      expr -> expr
