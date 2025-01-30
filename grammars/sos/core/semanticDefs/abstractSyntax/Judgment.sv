grammar sos:core:semanticDefs:abstractSyntax;


nonterminal Judgment with
   pp,
   moduleName,
   tyEnv, constructorEnv, judgmentEnv, projectionEnv,
   upSubst, downSubst, finalSubst,
   downVarTypes, upVarTypes,
   headRel, isRelJudgment, isProjJudgment, projType,
   isConclusion, isExtensibleRule, isProjectionRule,
   projRuleConstructors,
   errors,
   location;
propagate errors on Judgment;

--relation being applied to form a judgment
synthesized attribute headRel::JudgmentEnvItem;
synthesized attribute isRelJudgment::Boolean;
--whether this is a projection, and of what type
synthesized attribute isProjJudgment::Boolean;
synthesized attribute projType::QName;

inherited attribute isConclusion::Boolean;
inherited attribute isExtensibleRule::Boolean;
inherited attribute isProjectionRule::Boolean;

abstract production relation
top::Judgment ::= rel::QName args::TermList
{
  top.pp = rel.pp ++ " " ++ args.pp_space;

  args.moduleName = top.moduleName;

  args.tyEnv = top.tyEnv;
  args.constructorEnv = top.constructorEnv;

  rel.judgmentEnv = top.judgmentEnv;
  top.errors <- rel.judgmentErrors;

  --if it is the conclusion, we want to check it works even if the
  --relation is polymorphic, so rigidize rather than freshen
  local freshened::TypeList =
      if top.isConclusion
      then rigidizeTypeList(rel.judgmentType)
      else freshenTypeList(rel.judgmentType);
  args.expectedTypes =
       if rel.judgmentFound
       then just(^freshened)
       else nothing();
  args.lastConstructor =
       if rel.judgmentFound
       then rel.fullJudgment.name
       else ^rel;
  args.downSubst = top.downSubst;
  top.upSubst = args.upSubst;
  args.finalSubst = top.finalSubst;

  args.expectedPC =
       if top.isConclusion && top.isExtensibleRule &&
          rel.judgmentFound && rel.fullJudgment.isExtensible
       then just(rel.fullJudgment.pcIndex)
       else nothing();
  args.isConclusion = top.isConclusion;
  args.isExtensibleRule = top.isExtensibleRule;
  args.isProjectionRule = top.isProjectionRule;

  args.downVarTypes = top.downVarTypes;
  top.upVarTypes = args.upVarTypes;

  top.headRel = rel.fullJudgment;
  top.isRelJudgment = true;
  top.isProjJudgment = false;
  top.projType = error("Should not access");

  top.projRuleConstructors = [];

  top.errors <-
      if top.isConclusion && !top.isExtensibleRule
        --fixed relations can't add new rules in other modules
      then if rel.judgmentFound &&
              !sameModule(top.moduleName, rel.fullJudgment.name)
           then [errorMessage("Cannot add new rules to imported " ++
                    "fixed judgment " ++ rel.fullJudgment.name.pp,
                    location=top.location)]
           else []
      else [];
}


abstract production negationRelation
top::Judgment ::= rel::QName args::TermList
{
  top.pp = "! " ++ rel.pp ++ " " ++ args.pp_space;

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
  args.lastConstructor =
       if rel.judgmentFound
       then rel.fullJudgment.name
       else ^rel;
  args.downSubst = top.downSubst;
  top.upSubst = args.upSubst;
  args.finalSubst = top.finalSubst;

  args.expectedPC =
       if top.isConclusion && top.isExtensibleRule &&
          rel.judgmentFound && rel.fullJudgment.isExtensible
       then just(rel.fullJudgment.pcIndex)
       else nothing();
  args.isConclusion = top.isConclusion;
  args.isExtensibleRule = top.isExtensibleRule;
  args.isProjectionRule = top.isProjectionRule;

  args.downVarTypes = top.downVarTypes;
  top.upVarTypes = args.upVarTypes;

  top.headRel = rel.fullJudgment;
  top.isRelJudgment = true;
  top.isProjJudgment = false;
  top.projType = error("Should not access");

  top.projRuleConstructors = [];

  top.errors <-
      if top.isConclusion
      then [errorMessage("Cannot have a negation as a conclusion",
                         location=top.location)]
      else [];
}


abstract production projJudgment
top::Judgment ::= args::TermList ty::QName t::Term projection::Term
{
  top.pp =
      args.pp_comma ++ " |{" ++ ty.pp ++ "}- " ++ t.pp ++ " ~~> " ++
                                                  projection.pp;

  args.moduleName = top.moduleName;
  t.moduleName = top.moduleName;
  projection.moduleName = top.moduleName;

  args.tyEnv = top.tyEnv;
  args.constructorEnv = top.constructorEnv;
  t.tyEnv = top.tyEnv;
  t.constructorEnv = top.constructorEnv;
  projection.tyEnv = top.tyEnv;
  projection.constructorEnv = top.constructorEnv;

  ty.tyEnv = top.tyEnv;
  top.errors <- ty.tyErrors;

  args.downSubst = top.downSubst;
  t.downSubst = args.upSubst;
  projection.downSubst = t.upSubst;
  local unifyT::TypeUnify =
        typeUnify(t.type, ty.fullTy, location=t.location);
  local unifyProjection::TypeUnify =
        typeUnify(projection.type, ty.fullTy,
                  location=projection.location);
  unifyT.downSubst = projection.upSubst;
  unifyProjection.downSubst = unifyT.upSubst;
  args.lastConstructor =
       toQName("<projection>", top.location);
  args.expectedTypes =
       if ty.tyFound
       then case ty.fullTy of
            | nameType(name)
              when lookupEnv(^name, top.projectionEnv)
                   matches [tenvi] ->
              just(tenvi.types)
            | _ -> nothing()
            end
       else nothing();
  top.upSubst = unifyProjection.upSubst;

  args.finalSubst = top.finalSubst;
  t.finalSubst = top.finalSubst;
  projection.finalSubst = top.finalSubst;

  args.expectedPC = nothing();
  args.isConclusion = false;
  args.isExtensibleRule = false;
  args.isProjectionRule = false;

  args.downVarTypes = top.downVarTypes;
  t.downVarTypes = args.upVarTypes;
  projection.downVarTypes = t.upVarTypes;
  top.upVarTypes = projection.upVarTypes;

  top.headRel =
      error("Should not access headRel on non-relation");
  top.isRelJudgment = false;
  top.isProjJudgment = true;
  top.projType = if ty.tyFound
                 then case ty.fullTy of
                      | nameType(name) -> ^name
                      | _ -> ^ty
                      end
                 else ^ty;

  top.projRuleConstructors = [t.headConstructor];

  top.errors <-
      if top.isConclusion && top.isProjectionRule
      then [errorMessage("Cannot write a projection rule for " ++
               "projection judgments", location=top.location)]
      else [];

  top.errors <-
      if !top.isConclusion
      then []
      else if t.isVariable
      then [errorMessage("Cannot write a rule for projection for " ++
               "an unspecified constructor", location=t.location)]
      else if ty.tyFound &&
              sameModule(top.moduleName, ty.fullTy.name)
      then [errorMessage("Cannot write a rule for projection for " ++
               "a constructor from the module declaring the type " ++
               "it builds", location=top.location)]
      else if t.headIsConstructor && !t.headConstructorCurrentModule
      then [errorMessage("Cannot write a rule for projection for " ++
               "a constructor from another module",
               location=top.location)]
      else [];
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
  op.finalSubst = top.finalSubst;

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
  top.isProjJudgment = false;
  top.projType =
      error("Should not access projType on non-projection");

  top.projRuleConstructors = [];

  top.errors <-
      if top.isConclusion
      then [errorMessage("Conclusion of rule must be a relation or" ++
               " projection; found binary operation",
               location=top.location)]
      else [];
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
  top.isProjJudgment = false;
  top.projType =
      error("Should not access projType on non-projection");

  top.projRuleConstructors = [];

  top.errors <-
      if top.isConclusion
      then [errorMessage("Conclusion of rule must be a relation or" ++
               " projection; found binary operation",
               location=top.location)]
      else [];
}





--A binary operator for the judgment form  T1 <op> T2 = T3
nonterminal BinOp with
   pp,
   leftTy, rightTy, resultTy, downSubst, upSubst, finalSubst, errors,
   location;
propagate errors on BinOp;

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

  --unify the three types with each other
  local unifyLeftRight::TypeUnify =
      typeUnify(top.leftTy, top.rightTy, location=top.location);
  local unifyRightResult::TypeUnify =
      typeUnify(top.rightTy, top.resultTy, location=top.location);
  unifyLeftRight.downSubst = top.downSubst;
  unifyRightResult.downSubst = unifyLeftRight.upSubst;
  top.upSubst = unifyRightResult.upSubst;

  --everything should be string or list in the end
  --only need to check one because they are all unified
  top.errors <-
      case performSubstitutionType(top.leftTy, top.finalSubst) of
      | stringType() -> []
      | listType(_) -> []
      | varType(_) -> [] --not filled in, so could be string or list
      | ty -> [errorMessage("Cannot append type " ++ ty.pp,
                            location=top.location)]
      end;
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

