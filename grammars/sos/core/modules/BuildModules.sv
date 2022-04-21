grammar sos:core:modules;


@{-
  - Given the module name we want to compile, build a list of modules based on imports
  - @param initialModule  name of the module we want to compile
  - @param rootLoc  root location in the filesystem in which to find the modules
  - @param fileParse  parser to use for parsing each file
  - @param ioIn  initial IO state
  - @return  list of module objects, where B comes later in the list than A if A builds on B, or an error message
-}
function buildModuleList
IOVal<Either<String ModuleList>> ::=
   initialModule::String rootLoc::String
   fileParse::(ParseResult<File_c> ::= String String)
   ioIn::IOToken
{
  local firstModule::IOVal<Either<String Module>> =
        buildModule(initialModule, rootLoc, fileParse, ioIn);
  local built::IOVal<Either<String ModuleList>> =
        buildModuleList_helper(
           firstModule.iovalue.fromRight.buildsOnDecls,
           rootLoc, fileParse, nilModuleList(), firstModule.io);

  return case firstModule.iovalue of
         | left(err) -> ioval(firstModule.io, left(err))
         | right(mod) ->
           case built.iovalue of
           | left(err) -> ioval(built.io, left(err))
           | right(modList) ->
             ioval(built.io, right(consModuleList(mod, modList)))
           end
         end;
}
function buildModuleList_helper
IOVal<Either<String ModuleList>> ::=
   buildsOn::[QName] rootLoc::String
   fileParse::(ParseResult<File_c> ::= String String)
   thusFar::ModuleList ioIn::IOToken
{
  --Build the first module from the list of buildsOn
  local buildNextModule::IOVal<Either<String Module>> =
        buildModule(head(buildsOn).pp, rootLoc, fileParse, ioIn);
  local nextModule::Module = buildNextModule.iovalue.fromRight;
  --Build its list of modules it builds on
  local subcallNextModule::IOVal<Either<String ModuleList>> =
        buildModuleList_helper(nextModule.buildsOnDecls, rootLoc,
           fileParse, thusFar, buildNextModule.io);
  local nextModuleList::ModuleList =
        consModuleList(nextModule,
                       subcallNextModule.iovalue.fromRight);
  --Build the rest of the modules from buildsOn
  local subcallRestCurrent::IOVal<Either<String ModuleList>> =
        buildModuleList_helper(tail(buildsOn), rootLoc, fileParse,
           nextModuleList, subcallNextModule.io);

  return
     case buildsOn of
     | [] -> ioval(ioIn, right(thusFar))
     | mod::r ->
       --If anything else built on mod, it would already be in thusFar
       --along with all the modules it builds on
       if contains(mod.pp, thusFar.nameList)
       then buildModuleList_helper(r, rootLoc, fileParse, thusFar,
                                   ioIn)
       else case buildNextModule.iovalue of
            | left(err) -> ioval(buildNextModule.io, left(err))
            | right(_) ->
              case subcallNextModule.iovalue of
              | left(err) -> ioval(buildNextModule.io, left(err))
              | right(_) -> subcallRestCurrent
              end
            end
     end;
}


@{-
  - Build a Module object by reading and parsing the files in the module
  - @param moduleName  module to read and build (e.g. stlc:host)
  - @param rootLoc  root location in the filesystem in which to find the module
  - @param fileParse  parser to use for parsing each file
  - @param ioIn  initial IO state
  - @return  produces the Module object for the named module or an error message
-}
function buildModule
IOVal<Either<String Module>> ::=
   moduleName::String rootLoc::String
   fileParse::(ParseResult<File_c> ::= String String)
   ioIn::IOToken
{
  local moduleParts::[String] = explode(":", moduleName);
  local dirLoc::String =
        if endsWith(rootLoc, "/")
        then rootLoc ++ implode("/", moduleParts)
        else implode("/", rootLoc::moduleParts);
  local isDir::IOVal<Boolean> = isDirectoryT(dirLoc, ioIn);
  local dirContents::IOVal<[String]> =
        listContentsT(dirLoc, isDir.io);
  local sosFiles::[String] =
        filter(\ s::String -> splitFileNameAndExtension(s).2 == "sos",
               dirContents.iovalue);
  local files::IOVal<Either<String Files>> =
        buildFiles(dirLoc, sosFiles, fileParse, dirContents.io);
  return
     if !isDir.iovalue
     then ioval(isDir.io, left("Could not find module " ++ moduleName))
     else case files.iovalue of
          | left(err) -> ioval(files.io, left(err))
          | right(f) -> ioval(files.io, right(module(moduleName, f)))
          end;
}


@{-
  - Build a Files object of all the files in the given list
  - @param files  list of filenames
  - @param fileParse  parser to use for parsing each file
  - @param ioIn  initial IO state
  - @return  produces the Files object for all the given files or an error message
-}
function buildFiles
IOVal<Either<String Files>> ::=
   directory::String files::[String]
   fileParse::(ParseResult<File_c> ::= String String)
   ioIn::IOToken
{
  local filename::String = directory ++ "/" ++ head(files);
  local fileContents::IOVal<String> = readFileT(filename, ioIn);
  local parsed::ParseResult<File_c> =
        fileParse(fileContents.iovalue, head(files));
  local rest::IOVal<Either<String Files>> =
        buildFiles(directory, tail(files),
                   fileParse, fileContents.io);
  return
     case files of
     | [] -> ioval(ioIn, right(nilFiles()))
     | f::r ->
       if !parsed.parseSuccess
       then ioval(fileContents.io,
                  left("File " ++ filename ++ " did not parse:\n" ++
                       parsed.parseErrors))
       else case rest.iovalue of
            | left(err) -> ioval(rest.io, left(err))
            | right(fs) ->
              ioval(rest.io,
                    right(consFiles(head(files), parsed.parseTree.ast,
                                    fs)))
            end
     end;
}

