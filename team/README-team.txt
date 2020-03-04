# Sample README

NB - replace this file by your overall team README.

This directory is used for:

* Team phase code submission
* Team phase individual team contribution statements (named `team-login.md`)
* overall team readme

# Documentation

This documentation will contain:
- how to run the code
- features of the language with some code examples (TODO)
- implementative details of our system (TODO)

## Run the code

Enter into the `src` directory and run with `dotnet run`.

## Features

## Implementation details
The code is divided in a series of modules.

- `TestLib`: provides a basic testing library, built on top of Expecto.
- `SharedTypes`: contains the types which are reused across different modules,
like `Token`, `Ast` and error types.
- `Tokeniser`, `Parser`, `TypeChecker`, `SKIEngine`, `BetaEngine`: these are the
core modules of our language. Their top level functions all return a
`Result<someType, ErrorT>` where `someType` is the type returning for a correct
execution of the program.
