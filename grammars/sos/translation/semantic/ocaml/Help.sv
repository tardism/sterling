-- -- type bool_expr =
-- --   | True
-- --   | False  
-- --   | Greater of arith_expr * arith_expr
-- --   | Eq of arith_expr * arith_expr
-- --   | And of bool_expr * bool_expr
-- --   | Or of bool_expr * bool_expr

-- -- like Greater of arith_expr * arith_expr

-- function OcamlConstructorTerm 
-- String ::= type::String types::TypeList
-- {
--   return if types == "" then " | " ++ type else 
--           " | " ++ type ++ " of " ++ types.pp_comma;
-- }


-- -- let OcamlAbsSyntaxDecl has pp
-- -- we will use Decls eventually

-- function ocamlConstructorDecls
-- String ::= type::String constructors::AbsConstructorDecls
-- {
--   local decls::String = 
--   return "type " ++ type ++ " =\n" ++
--     implode("\n", 
--             map(\ c::AbsConstructorDecl ->
--                    OcamlConstructorTerm(c.type.pp, c.types), constructors.constructorDecls)) ++
--     "\n\n";
-- }


