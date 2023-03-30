# Running SOS-Ext
As described in the [discussion of modules](modules.md), SOS-Ext is
based on a module system where each module is the contents of a
directory.  Given a module name, SOS-Ext takes the following steps in
order:
* Identify the directory wherein that module resides
* Reads all the files in the directory, building up the imported
  modules as well
* Checks for errors in the definitions in the specified module and the
  imported modules
* Runs any actions specified by extensions to the core language of
  SOS-Ext


## Finding Modules
By default, SOS-Ext looks for modules in the current directory.  For
example, if the current directory is `dir` and we have a directory
`dir/mo/du/le/`, running
```
sos-ext mo:du:le
```
will find and check the contents of the `dir/mo/du/le/` directory.
However, we often want to run SOS-Ext from a different directory.  If
we are in the `dir/mo/du/` directory, we can set the root directory to
find grammars by running SOS-Ext as
```
sos-ext -I ../.. mo:du:le
```
This will look for the module starting two directories up, finding
`../../mo/du/le` for the location of the module.  If SOS-Ext finds
multiple instances of the `-I` flag, it will check in each location in
order when trying to find modules, taking the first one it finds as
the intended module.


## Actions Taken
SOS-Ext is written as an extensible language itself.  The core
language takes only one action when running, which is to check the
module for errors.  If it finds any errors, it prints them out by
module and file, otherwise outputting a message that no errors were
found.

The behavior of SOS-Ext can be extended by extensions to add more
actions.  These are activated by various flags, which can be found by
running
```
sos-ext --help
```
to list the available flags.  In particular, SOS-Ext does not produce
an executable by default.  The right combination of flags must be
passed to tell it to produce some sort of executable.  More can be
read about this in [the extensions
documentation](extensions.md#translations).
