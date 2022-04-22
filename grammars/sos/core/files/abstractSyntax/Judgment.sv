grammar sos:core:files:abstractSyntax;


nonterminal Judgment with
   pp,
   moduleName,
   tyEnv, constructorEnv, judgmentEnv, translationEnv,
   upSubst, downSubst, finalSubst,
   errors,
   location;
propagate errors on Judgment;

abstract production relation
top::Judgment ::= rel::QName args::TermList
{
  top.pp = rel.pp ++ " " ++ args.pp_space;

  args.moduleName = top.moduleName;

  args.tyEnv = top.tyEnv;
  args.constructorEnv = top.constructorEnv;

  rel.judgmentEnv = top.judgmentEnv;
  top.errors <- rel.judgmentErrors;

  local unifyArgs::TypeUnify =
        typeListUnify(args.types, freshenTypeList(rel.judgmentType));
  unifyArgs.downSubst = args.upSubst;
  args.downSubst = top.downSubst;
  top.upSubst = if rel.judgmentFound
                then unifyArgs.upSubst
                else args.upSubst;
  args.finalSubst = top.finalSubst;
}


abstract production eqJudgment
top::Judgment ::= t1::Term t2::Term
{
  top.pp = t1.pp ++ " = " ++ t2.pp;

  t1.moduleName = top.moduleName;
  t2.moduleName = top.moduleName;

  t1.tyEnv = top.tyEnv;
  t1.constructorEnv = top.constructorEnv;
  t2.tyEnv = top.tyEnv;
  t2.constructorEnv = top.constructorEnv;

  local unifyTys::TypeUnify = typeUnify(t1.type, t2.type);
  t1.downSubst = top.downSubst;
  t2.downSubst = t1.upSubst;
  unifyTys.downSubst = t2.upSubst;
  top.upSubst = unifyTys.upSubst;
}


abstract production neqJudgment
top::Judgment ::= t1::Term t2::Term
{
  top.pp = t1.pp ++ " != " ++ t2.pp;

  t1.moduleName = top.moduleName;
  t2.moduleName = top.moduleName;

  t1.tyEnv = top.tyEnv;
  t1.constructorEnv = top.constructorEnv;
  t2.tyEnv = top.tyEnv;
  t2.constructorEnv = top.constructorEnv;

  local unifyTys::TypeUnify = typeUnify(t1.type, t2.type);
  t1.downSubst = top.downSubst;
  t2.downSubst = t1.upSubst;
  unifyTys.downSubst = t2.upSubst;
  top.upSubst = unifyTys.upSubst;
}


abstract production greaterJudgment
top::Judgment ::= t1::Term t2::Term
{
  top.pp = t1.pp ++ " > " ++ t2.pp;

  t1.moduleName = top.moduleName;
  t2.moduleName = top.moduleName;

  t1.tyEnv = top.tyEnv;
  t1.constructorEnv = top.constructorEnv;
  t2.tyEnv = top.tyEnv;
  t2.constructorEnv = top.constructorEnv;

  local unifyTys1::TypeUnify =
        typeUnify(t1.type, intType(location=top.location));
  local unifyTys2::TypeUnify =
        typeUnify(t2.type, intType(location=top.location));
  t1.downSubst = top.downSubst;
  t2.downSubst = t1.upSubst;
  unifyTys1.downSubst = t2.upSubst;
  unifyTys2.downSubst = unifyTys1.upSubst;
  top.upSubst = unifyTys2.upSubst;
}


abstract production lessJudgment
top::Judgment ::= t1::Term t2::Term
{
  top.pp = t1.pp ++ " < " ++ t2.pp;

  t1.moduleName = top.moduleName;
  t2.moduleName = top.moduleName;

  t1.tyEnv = top.tyEnv;
  t1.constructorEnv = top.constructorEnv;
  t2.tyEnv = top.tyEnv;
  t2.constructorEnv = top.constructorEnv;

  local unifyTys1::TypeUnify =
        typeUnify(t1.type, intType(location=top.location));
  local unifyTys2::TypeUnify =
        typeUnify(t2.type, intType(location=top.location));
  t1.downSubst = top.downSubst;
  t2.downSubst = t1.upSubst;
  unifyTys1.downSubst = t2.upSubst;
  unifyTys2.downSubst = unifyTys1.upSubst;
  top.upSubst = unifyTys2.upSubst;
}


abstract production geqJudgment
top::Judgment ::= t1::Term t2::Term
{
  top.pp = t1.pp ++ " >= " ++ t2.pp;

  t1.moduleName = top.moduleName;
  t2.moduleName = top.moduleName;

  t1.tyEnv = top.tyEnv;
  t1.constructorEnv = top.constructorEnv;
  t2.tyEnv = top.tyEnv;
  t2.constructorEnv = top.constructorEnv;

  local unifyTys1::TypeUnify =
        typeUnify(t1.type, intType(location=top.location));
  local unifyTys2::TypeUnify =
        typeUnify(t2.type, intType(location=top.location));
  t1.downSubst = top.downSubst;
  t2.downSubst = t1.upSubst;
  unifyTys1.downSubst = t2.upSubst;
  unifyTys2.downSubst = unifyTys1.upSubst;
  top.upSubst = unifyTys2.upSubst;
}


abstract production leqJudgment
top::Judgment ::= t1::Term t2::Term
{
  top.pp = t1.pp ++ " <= " ++ t2.pp;

  t1.moduleName = top.moduleName;
  t2.moduleName = top.moduleName;

  t1.tyEnv = top.tyEnv;
  t1.constructorEnv = top.constructorEnv;
  t2.tyEnv = top.tyEnv;
  t2.constructorEnv = top.constructorEnv;

  local unifyTys1::TypeUnify =
        typeUnify(t1.type, intType(location=top.location));
  local unifyTys2::TypeUnify =
        typeUnify(t2.type, intType(location=top.location));
  t1.downSubst = top.downSubst;
  t2.downSubst = t1.upSubst;
  unifyTys1.downSubst = t2.upSubst;
  unifyTys2.downSubst = unifyTys1.upSubst;
  top.upSubst = unifyTys2.upSubst;
}


abstract production transJudgment
top::Judgment ::= args::TermList t::Term translation::Term
{
  top.pp =
      args.pp_comma ++ " |- " ++ t.pp ++ " ~~> " ++ translation.pp;

  args.moduleName = top.moduleName;
  t.moduleName = top.moduleName;
  translation.moduleName = top.moduleName;

  args.tyEnv = top.tyEnv;
  args.constructorEnv = top.constructorEnv;
  t.tyEnv = top.tyEnv;
  t.constructorEnv = top.constructorEnv;
  translation.tyEnv = top.tyEnv;
  translation.constructorEnv = top.constructorEnv;

  --type being translated
  local finalTransType::Type =
        performSubstitutionType(t.type, top.finalSubst);
  top.errors <-
      case finalTransType of
      | nameType(name) -> []
      | errorType() -> []
      | _ ->
        [errorMessage("Cannot translate type " ++ finalTransType.pp,
                      location=top.location)]
      end;

  args.downSubst = top.downSubst;
  t.downSubst = args.upSubst;
  translation.downSubst = t.upSubst;
  local unifyTerms::TypeUnify = typeUnify(t.type, translation.type);
  unifyTerms.downSubst = translation.upSubst;
  local unifyTypes::TypeUnify =
        case performSubstitutionType(t.type, unifyTerms.upSubst) of
        | nameType(name) when
          lookupEnv(name, top.translationEnv) matches [tenvi] ->
          typeListUnify(args.types, tenvi.types)
          --if not extensible and known, just unify something useless
        | _ -> typeUnify(intType(location=top.location),
                         intType(location=top.location))
        end;
  unifyTypes.downSubst = unifyTerms.upSubst;
  top.upSubst = unifyTypes.upSubst;
}


abstract production binOpJudgment
top::Judgment ::= t1::Term op::BinOp t2::Term result::Term
{
  top.pp = t1.pp ++ op.pp ++ t2.pp ++ " = " ++ result.pp;

  t1.moduleName = top.moduleName;
  t2.moduleName = top.moduleName;
  result.moduleName = top.moduleName;

  t1.tyEnv = top.tyEnv;
  t1.constructorEnv = top.constructorEnv;
  t2.tyEnv = top.tyEnv;
  t2.constructorEnv = top.constructorEnv;
  result.tyEnv = top.tyEnv;
  result.constructorEnv = top.constructorEnv;

  t1.downSubst = top.downSubst;
  t2.downSubst = t1.upSubst;
  result.downSubst = t2.upSubst;
  op.downSubst = result.upSubst;
  top.upSubst = op.upSubst;

  op.leftTy = t1.type;
  op.rightTy = t2.type;
  op.resultTy = result.type;
}





nonterminal BinOp with
   pp,
   leftTy, rightTy, resultTy, downSubst, upSubst, finalSubst,
   location;

inherited attribute leftTy::Type;
inherited attribute rightTy::Type;
inherited attribute resultTy::Type;

abstract production plusOp
top::BinOp ::=
{
  top.pp = " + ";

  --everything should be int
  local unifyLeft::TypeUnify =
        typeUnify(intType(location=top.location), top.leftTy);
  local unifyRight::TypeUnify =
        typeUnify(intType(location=top.location), top.rightTy);
  local unifyResult::TypeUnify =
        typeUnify(intType(location=top.location), top.resultTy);
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  unifyResult.downSubst = unifyRight.upSubst;
  top.upSubst = unifyResult.upSubst;
}


abstract production minusOp
top::BinOp ::=
{
  top.pp = " - ";

  --everything should be int
  local unifyLeft::TypeUnify =
        typeUnify(intType(location=top.location), top.leftTy);
  local unifyRight::TypeUnify =
        typeUnify(intType(location=top.location), top.rightTy);
  local unifyResult::TypeUnify =
        typeUnify(intType(location=top.location), top.resultTy);
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  unifyResult.downSubst = unifyRight.upSubst;
  top.upSubst = unifyResult.upSubst;
}


abstract production multOp
top::BinOp ::=
{
  top.pp = " * ";

  --everything should be int
  local unifyLeft::TypeUnify =
        typeUnify(intType(location=top.location), top.leftTy);
  local unifyRight::TypeUnify =
        typeUnify(intType(location=top.location), top.rightTy);
  local unifyResult::TypeUnify =
        typeUnify(intType(location=top.location), top.resultTy);
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  unifyResult.downSubst = unifyRight.upSubst;
  top.upSubst = unifyResult.upSubst;
}


abstract production divOp
top::BinOp ::=
{
  top.pp = " / ";

  --everything should be int
  local unifyLeft::TypeUnify =
        typeUnify(intType(location=top.location), top.leftTy);
  local unifyRight::TypeUnify =
        typeUnify(intType(location=top.location), top.rightTy);
  local unifyResult::TypeUnify =
        typeUnify(intType(location=top.location), top.resultTy);
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  unifyResult.downSubst = unifyRight.upSubst;
  top.upSubst = unifyResult.upSubst;
}


abstract production modOp
top::BinOp ::=
{
  top.pp = " % ";

  --everything should be int
  local unifyLeft::TypeUnify =
        typeUnify(intType(location=top.location), top.leftTy);
  local unifyRight::TypeUnify =
        typeUnify(intType(location=top.location), top.rightTy);
  local unifyResult::TypeUnify =
        typeUnify(intType(location=top.location), top.resultTy);
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  unifyResult.downSubst = unifyRight.upSubst;
  top.upSubst = unifyResult.upSubst;
}


abstract production appendOp
top::BinOp ::=
{
  top.pp = " ++ ";

  --everything should be string
  local unifyLeft::TypeUnify =
        typeUnify(stringType(location=top.location), top.leftTy);
  local unifyRight::TypeUnify =
        typeUnify(stringType(location=top.location), top.rightTy);
  local unifyResult::TypeUnify =
        typeUnify(stringType(location=top.location), top.resultTy);
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  unifyResult.downSubst = unifyRight.upSubst;
  top.upSubst = unifyResult.upSubst;
}

