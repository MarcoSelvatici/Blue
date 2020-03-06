module PreprocessorTest

open SharedTypes

let testCasesPreprocessor = 
    [
        "No import", "x", Ok "x\n";
        "No import multiline", "a\nb\nc\nd", Ok "a\nb\nc\nd\n";
        "No import emptyline", "l1\n\nl3", Ok "l1\n\nl3\n";
        
        "Import test", "import Test", Ok "let test = 0 in\nni ";
        "Import test multiline", "import Test\na+3", Ok "let test = 0 in\na+3\nni ";
        "Import test emptyline", "\n\nimport Test\nNull\n", Ok "\n\nlet test = 0 in\nNull\n\nni ";

        "Import list", "import List", 
        Ok "let listMap f lst =
    if size lst == 0
    then []
    else append (f (head lst)) (listMap f (tail lst))
    fi in\nni ";

        "Import error", "import test", "\'import test\' error\nno such library\ndid you mean [\"List\"; \"Test\"]" |> PreprocessorError |> Error;
    ]