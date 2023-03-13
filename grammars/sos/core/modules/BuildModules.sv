grammar sos:core:modules;


@{-
  - Given the module name we want to compile, build a list of modules based on imports
  - @param initialModule  name of the module we want to compile
  - @param rootLocs  directories in the filesystem in which to find the modules
  - @param abstractFileParse  parser to use for parsing each .sos file
  - @param concreteFileParse  parser to use for parsing each .conc file
  - @param mainFileParse  parser to use for parsing each .main file
  - @param ioIn  initial IO state
  - @return  list of module objects, where B comes later in the list than A if A builds on B, or an error message
-}
function buildModuleList
IOVal<Either<String ModuleList>> ::=
   initialModule::String rootLocs::[String]
   abstractFileParse::(ParseResult<File_c> ::= String String)
   concreteFileParse::(ParseResult<ConcreteFile_c> ::= String String)
   mainFileParse::(ParseResult<MainFile_c> ::= String String)
   ioIn::IOToken
{
  local stdLib::IOVal<ModuleList> =
        buildStdLib(abstractFileParse, concreteFileParse,
                    mainFileParse, ioIn);
  local firstModule::IOVal<Either<String Module>> =
        buildModule(initialModule, rootLocs, abstractFileParse,
                    concreteFileParse, mainFileParse, stdLib.io);
  local built::IOVal<Either<String ModuleList>> =
        buildModuleList_helper(
           firstModule.iovalue.fromRight.buildsOnDecls,
           rootLocs, abstractFileParse, concreteFileParse,
           mainFileParse, stdLib.iovalue, firstModule.io);

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
   buildsOn::[QName] rootLocs::[String]
   abstractFileParse::(ParseResult<File_c> ::= String String)
   concreteFileParse::(ParseResult<ConcreteFile_c> ::= String String)
   mainFileParse::(ParseResult<MainFile_c> ::= String String)
   thusFar::ModuleList ioIn::IOToken
{
  --Build the first module from the list of buildsOn
  local buildNextModule::IOVal<Either<String Module>> =
        buildModule(head(buildsOn).pp, rootLocs, abstractFileParse,
                    concreteFileParse, mainFileParse, ioIn);
  local nextModule::Module = buildNextModule.iovalue.fromRight;
  --Build its list of modules it builds on
  local subcallNextModule::IOVal<Either<String ModuleList>> =
        buildModuleList_helper(nextModule.buildsOnDecls, rootLocs,
           abstractFileParse, concreteFileParse, mainFileParse,
           thusFar, buildNextModule.io);
  local nextModuleList::ModuleList =
        consModuleList(nextModule,
                       subcallNextModule.iovalue.fromRight);
  --Build the rest of the modules from buildsOn
  local subcallRestCurrent::IOVal<Either<String ModuleList>> =
        buildModuleList_helper(tail(buildsOn), rootLocs, abstractFileParse,
           concreteFileParse, mainFileParse, nextModuleList,
           subcallNextModule.io);

  return
     case buildsOn of
     | [] -> ioval(ioIn, right(thusFar))
     | mod::r ->
       --If anything else built on mod, it would already be in thusFar
       --along with all the modules it builds on
       if contains(mod.pp, thusFar.nameList)
       then buildModuleList_helper(r, rootLocs, abstractFileParse,
               concreteFileParse, mainFileParse, thusFar, ioIn)
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
  - Build the basic ModuleList for the standard library alone
  - @param abstractFileParse  parser to use for parsing each .sos file
  - @param concreteFileParse  parser to use for parsing each .conc file
  - @param mainFileParse  parser to use for parsing each .main file
  - @param ioIn  initial IO state
  - @return  produces the ModuleList object for the standard library
-}
function buildStdLib
IOVal<ModuleList> ::=
   abstractFileParse::(ParseResult<File_c> ::= String String)
   concreteFileParse::(ParseResult<ConcreteFile_c> ::= String String)
   mainFileParse::(ParseResult<MainFile_c> ::= String String)
   ioIn::IOToken
{
  local readSOS_HOME::IOVal<String> = envVarT("SOS_HOME", ioIn);
  local dir::String = readSOS_HOME.iovalue ++ "/stdLib";
  local files::IOVal<Either<String Files>> =
      buildAllFiles(dir, abstractFileParse, concreteFileParse,
                    mainFileParse, readSOS_HOME.io);
  return if readSOS_HOME.iovalue == ""
         then error("Must run using provided script")
         else case files.iovalue of
              | right(f) -> ioval(files.io, stdLibModuleList(f))
              | left(err) ->
                error("Error reading standard library:  " ++ err)
              end;
}


@{-
  - Build a Module object by reading and parsing the files in the module
  - @param moduleName  module to read and build (e.g. stlc:host)
  - @param rootLocs  directories in the filesystem in which to find the module
  - @param abstractFileParse  parser to use for parsing each .sos file
  - @param concreteFileParse  parser to use for parsing each .conc file
  - @param mainFileParse  parser to use for parsing each .main file
  - @param ioIn  initial IO state
  - @return  produces the Module object for the named module or an error message
-}
function buildModule
IOVal<Either<String Module>> ::=
   moduleName::String rootLocs::[String]
   abstractFileParse::(ParseResult<File_c> ::= String String)
   concreteFileParse::(ParseResult<ConcreteFile_c> ::= String String)
   mainFileParse::(ParseResult<MainFile_c> ::= String String)
   ioIn::IOToken
{
  local moduleParts::[String] = explode(":", moduleName);
  local dirLoc::IOVal<Maybe<String>> =
        findDir(moduleParts, rootLocs, ioIn);
  local dir::String = dirLoc.iovalue.fromJust;
  local finalFiles::IOVal<Either<String Files>> =
        buildAllFiles(dir, abstractFileParse, concreteFileParse,
                      mainFileParse, dirLoc.io);
  --Combine
  return
     case dirLoc.iovalue of
     | nothing() ->
       ioval(dirLoc.io, left("Could not find module " ++ moduleName))
     | _ -> case finalFiles.iovalue of
            | left(err) -> ioval(finalFiles.io, left(err))
            | right(f) ->
              ioval(finalFiles.io, right(module(moduleName, f)))
            end
     end;
}


@{-
  - Build a Files object by reading and parsing the files in the directory
  - @param dir  directory to read files
  - @param abstractFileParse  parser to use for parsing each .sos file
  - @param concreteFileParse  parser to use for parsing each .conc file
  - @param mainFileParse  parser to use for parsing each .main file
  - @param ioIn  initial IO state
  - @return  produces the Files object for the directory or an error message
-}
function buildAllFiles
IOVal<Either<String Files>> ::=
   dir::String
   abstractFileParse::(ParseResult<File_c> ::= String String)
   concreteFileParse::(ParseResult<ConcreteFile_c> ::= String String)
   mainFileParse::(ParseResult<MainFile_c> ::= String String)
   ioIn::IOToken
{
  local dirContents::IOVal<[String]> = listContentsT(dir, ioIn);
  --Abstract files
  local sosFiles::[String] =
        filter(\ s::String -> splitFileNameAndExtension(s).2 == "sos",
               dirContents.iovalue);
  local files::IOVal<Either<String Files>> =
        buildFiles(dir, sosFiles, abstractFileParse,
                   consAbstractFiles, nilFiles(), dirContents.io);
  --Concrete files
  local concFiles::[String] =
        filter(\ s::String -> splitFileNameAndExtension(s).2 == "conc",
               dirContents.iovalue);
  local stepFiles::IOVal<Either<String Files>> =
        case files.iovalue of
        | left(_) -> files
        | right(fs) ->
          buildFiles(dir, concFiles, concreteFileParse,
                     consConcreteFiles, fs, files.io)
        end;
  --Main files
  local mainFiles::[String] =
        filter(\ s::String -> splitFileNameAndExtension(s).2 == "main",
               dirContents.iovalue);
  local finalFiles::IOVal<Either<String Files>> =
        case stepFiles.iovalue of
        | left(_) -> stepFiles
        | right(fs) ->
          buildFiles(dir, mainFiles, mainFileParse,
                     consMainFiles, fs, stepFiles.io)
        end;
  --Combine
  return finalFiles;
}


@{-
  - Find the location of the module in the given root locations
  - @param moduleParts  name of the module, split (e.g. [a, b, c] for a:b:c)
  - @param rootLocs  directories in which to look for the module
  - @param ioIn  initial IO state
  - @return  produces the directory location of the module or nothing()
-}
function findDir
IOVal<Maybe<String>> ::= moduleParts::[String] rootLocs::[String]
                         ioIn::IOToken
{
  local dirLoc::String = implode("/", head(rootLocs)::moduleParts);
  local isDir::IOVal<Boolean> = isDirectoryT(dirLoc, ioIn);

  local rest::IOVal<Maybe<String>> =
      findDir(moduleParts, tail(rootLocs), isDir.io);

  return case rootLocs of
         | [] -> ioval(ioIn, nothing())
         | _::_ when isDir.iovalue -> ioval(isDir.io, just(dirLoc))
         | _::_ -> rest
         end;
}


@{-
  - Build a Files object of all the files in the given list
  - @param files  list of filenames
  - @param fileParse  parser to use for parsing each file
  - @param consFiles  constructor for building Files from the parsed file
  - @param base   initial Files on which to build
  - @param ioIn  initial IO state
  - @return  produces the Files object for all the given files or an error message
-}
function buildFiles
attribute ast<fileType> {} occurs on fileType_c =>
IOVal<Either<String Files>> ::=
   directory::String files::[String]
   fileParse::(ParseResult<fileType_c> ::= String String)
   consFiles::(Files ::= String fileType Files)
   base::Files ioIn::IOToken
{
  local filename::String = directory ++ "/" ++ head(files);
  local fileContents::IOVal<String> = readFileT(filename, ioIn);
  local parsed::ParseResult<fileType_c> =
        fileParse(fileContents.iovalue, head(files));
  local rest::IOVal<Either<String Files>> =
        buildFiles(directory, tail(files), fileParse, consFiles,
                   base, fileContents.io);
  return
     case files of
     | [] -> ioval(ioIn, right(base))
     | f::r ->
       if !parsed.parseSuccess
       then ioval(fileContents.io,
                  left("File " ++ filename ++ " did not parse:\n" ++
                       parsed.parseErrors))
       else case rest.iovalue of
            | left(err) -> ioval(rest.io, left(err))
            | right(fs) ->
              ioval(rest.io,
                    right(consFiles(head(files), parsed.parseTree.ast, fs)))
            end
     end;
}

