grammar sos:core:files:abstractSyntax;


nonterminal Judgment with
   pp,
   moduleName,
   tyEnv, constructorEnv, judgmentEnv, translationEnv,
   upSubst, downSubst, finalSubst,
   downVarTypes, upVarTypes,
   headRel, isRelJudgment, isTransJudgment,
   errors,
   location;
propagate errors on Judgment;

--relation being applied to form a judgment
synthesized attribute headRel::JudgmentEnvItem;
synthesized attribute isRelJudgment::Boolean;
synthesized attribute isTransJudgment::Boolean;

abstract production relation
top::Judgment ::= rel::QName args::TermList
{
  top.pp = rel.pp ++ " " ++ args.pp_space;

  args.moduleName = top.moduleName;

  args.tyEnv = top.tyEnv;
  args.constructorEnv = top.constructorEnv;

  rel.judgmentEnv = top.judgmentEnv;
  top.errors <- rel.judgmentErrors;

  local freshened::TypeList = freshenTypeList(rel.judgmentType);
  args.expectedTypes =
       if rel.judgmentFound
       then just(rel.judgmentType)
       else nothing();
  args.lastConstructor = rel;
  args.downSubst = top.downSubst;
  top.upSubst = args.upSubst;
  args.finalSubst = top.finalSubst;

  args.downVarTypes = top.downVarTypes;
  top.upVarTypes = args.upVarTypes;

  top.headRel = rel.fullJudgment;
  top.isRelJudgment = true;
  top.isTransJudgment = false;
}


abstract production transJudgment
top::Judgment ::= args::TermList ty::QName t::Term translation::Term
{
  top.pp =
      args.pp_comma ++ " |{" ++ ty.pp ++ "}- " ++ t.pp ++ " ~~> " ++
                                                  translation.pp;

  args.moduleName = top.moduleName;
  t.moduleName = top.moduleName;
  translation.moduleName = top.moduleName;

  args.tyEnv = top.tyEnv;
  args.constructorEnv = top.constructorEnv;
  t.tyEnv = top.tyEnv;
  t.constructorEnv = top.constructorEnv;
  translation.tyEnv = top.tyEnv;
  translation.constructorEnv = top.constructorEnv;

  ty.tyEnv = top.tyEnv;
  top.errors <- ty.tyErrors;

  args.downSubst = top.downSubst;
  t.downSubst = args.upSubst;
  translation.downSubst = t.upSubst;
  local unifyT::TypeUnify =
        typeUnify(t.type, ty.fullTy, location=t.location);
  local unifyTranslation::TypeUnify =
        typeUnify(translation.type, ty.fullTy,
                  location=translation.location);
  unifyT.downSubst = translation.upSubst;
  unifyTranslation.downSubst = unifyT.upSubst;
  args.lastConstructor =
       toQName("<translation>", top.location);
  args.expectedTypes =
       if ty.tyFound
       then case ty.fullTy of
            | nameType(name)
              when lookupEnv(name, top.translationEnv)
                   matches [tenvi] ->
              just(tenvi.types)
            | _ -> nothing()
            end
       else nothing();
  top.upSubst = unifyTranslation.upSubst;

  args.finalSubst = top.finalSubst;
  t.finalSubst = top.finalSubst;
  translation.finalSubst = top.finalSubst;

  args.downVarTypes = top.downVarTypes;
  t.downVarTypes = args.upVarTypes;
  translation.downVarTypes = t.upVarTypes;
  top.upVarTypes = translation.upVarTypes;

  top.headRel =
      error("Should not access headRel on non-relation");
  top.isRelJudgment = false;
  top.isTransJudgment = true;
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

  t1.downVarTypes = top.downVarTypes;
  t2.downVarTypes = t1.upVarTypes;
  result.downVarTypes = t2.upVarTypes;
  top.upVarTypes = result.upVarTypes;

  top.headRel =
      error("Should not access headRel on non-relation");
  top.isRelJudgment = false;
  top.isTransJudgment = false;
}


abstract production topBinOpJudgment
top::Judgment ::= t1::Term op::TopBinOp t2::Term
{
  top.pp = t1.pp ++ op.pp ++ t2.pp;

  t1.moduleName = top.moduleName;
  t2.moduleName = top.moduleName;

  t1.tyEnv = top.tyEnv;
  t1.constructorEnv = top.constructorEnv;
  t2.tyEnv = top.tyEnv;
  t2.constructorEnv = top.constructorEnv;

  t1.downSubst = top.downSubst;
  t2.downSubst = t1.upSubst;
  op.leftTy = t1.type;
  op.rightTy = t2.type;
  op.downSubst = t2.upSubst;
  top.upSubst = op.upSubst;

  t1.downVarTypes = top.downVarTypes;
  t2.downVarTypes = t1.upVarTypes;
  top.upVarTypes = t2.upVarTypes;

  top.headRel =
      error("Should not access headRel on non-relation");
  top.isRelJudgment = false;
  top.isTransJudgment = false;
}





--A binary operator for the judgment form  T1 <op> T2 = T3
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
        typeUnify(intType(location=top.location), top.leftTy,
                  location=top.location);
  local unifyRight::TypeUnify =
        typeUnify(intType(location=top.location), top.rightTy,
                  location=top.location);
  local unifyResult::TypeUnify =
        typeUnify(intType(location=top.location), top.resultTy,
                  location=top.location);
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
        typeUnify(intType(location=top.location), top.leftTy,
                  location=top.location);
  local unifyRight::TypeUnify =
        typeUnify(intType(location=top.location), top.rightTy,
                  location=top.location);
  local unifyResult::TypeUnify =
        typeUnify(intType(location=top.location), top.resultTy,
                  location=top.location);
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
        typeUnify(intType(location=top.location), top.leftTy,
                  location=top.location);
  local unifyRight::TypeUnify =
        typeUnify(intType(location=top.location), top.rightTy,
                  location=top.location);
  local unifyResult::TypeUnify =
        typeUnify(intType(location=top.location), top.resultTy,
                  location=top.location);
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
        typeUnify(intType(location=top.location), top.leftTy,
                  location=top.location);
  local unifyRight::TypeUnify =
        typeUnify(intType(location=top.location), top.rightTy,
                  location=top.location);
  local unifyResult::TypeUnify =
        typeUnify(intType(location=top.location), top.resultTy,
                  location=top.location);
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
        typeUnify(intType(location=top.location), top.leftTy,
                  location=top.location);
  local unifyRight::TypeUnify =
        typeUnify(intType(location=top.location), top.rightTy,
                  location=top.location);
  local unifyResult::TypeUnify =
        typeUnify(intType(location=top.location), top.resultTy,
                  location=top.location);
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
        typeUnify(stringType(location=top.location), top.leftTy,
                  location=top.location);
  local unifyRight::TypeUnify =
        typeUnify(stringType(location=top.location), top.rightTy,
                  location=top.location);
  local unifyResult::TypeUnify =
        typeUnify(stringType(location=top.location), top.resultTy,
                  location=top.location);
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  unifyResult.downSubst = unifyRight.upSubst;
  top.upSubst = unifyResult.upSubst;
}





--A binary operator for the judgment form  T1 <op> T2
nonterminal TopBinOp with
   pp,
   leftTy, rightTy, downSubst, upSubst, finalSubst,
   location;

abstract production eqOp
top::TopBinOp ::=
{
  top.pp = " = ";

  local unifyTys::TypeUnify =
        typeUnify(top.leftTy, top.rightTy, location=top.location);
  unifyTys.downSubst = top.downSubst;
  top.upSubst = unifyTys.upSubst;
}


abstract production neqOp
top::TopBinOp ::=
{
  top.pp = " != ";

  local unifyTys::TypeUnify =
        typeUnify(top.leftTy, top.rightTy, location=top.location);
  unifyTys.downSubst = top.downSubst;
  top.upSubst = unifyTys.upSubst;
}


abstract production lessOp
top::TopBinOp ::=
{
  top.pp = " < ";

  local unifyTys1::TypeUnify =
        typeUnify(top.leftTy, intType(location=top.location),
                  location=top.location);
  local unifyTys2::TypeUnify =
        typeUnify(top.rightTy, intType(location=top.location),
                  location=top.location);
  unifyTys1.downSubst = top.downSubst;
  unifyTys2.downSubst = unifyTys1.upSubst;
  top.upSubst = unifyTys2.upSubst;
}


abstract production greaterOp
top::TopBinOp ::=
{
  top.pp = " > ";

  local unifyTys1::TypeUnify =
        typeUnify(top.leftTy, intType(location=top.location),
                  location=top.location);
  local unifyTys2::TypeUnify =
        typeUnify(top.rightTy, intType(location=top.location),
                  location=top.location);
  unifyTys1.downSubst = top.downSubst;
  unifyTys2.downSubst = unifyTys1.upSubst;
  top.upSubst = unifyTys2.upSubst;
}


abstract production leqOp
top::TopBinOp ::=
{
  top.pp = " <= ";

  local unifyTys1::TypeUnify =
        typeUnify(top.leftTy, intType(location=top.location),
                  location=top.location);
  local unifyTys2::TypeUnify =
        typeUnify(top.rightTy, intType(location=top.location),
                  location=top.location);
  unifyTys1.downSubst = top.downSubst;
  unifyTys2.downSubst = unifyTys1.upSubst;
  top.upSubst = unifyTys2.upSubst;
}


abstract production geqOp
top::TopBinOp ::=
{
  top.pp = " >= ";

  local unifyTys1::TypeUnify =
        typeUnify(top.leftTy, intType(location=top.location),
                  location=top.location);
  local unifyTys2::TypeUnify =
        typeUnify(top.rightTy, intType(location=top.location),
                  location=top.location);
  unifyTys1.downSubst = top.downSubst;
  unifyTys2.downSubst = unifyTys1.upSubst;
  top.upSubst = unifyTys2.upSubst;
}

