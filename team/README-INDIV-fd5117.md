## Individual Contribution

### **Lexer**

The Lexer for Blue was somewhat completed after the individual part of the project. However I updated it as follows in order to make it compiant with the other components of the language:
-  Add lexing for `print` and `test` builtins.
-  Removing chars and floats, as well as other unused features.
-  Change inline comments to `#`.
-  Make it tail recursive by adding an extra paramenter to the recursive function to pass around the partial result.

### **End-To-End Testing**

I Added some End-to-End tests files to the unit test files already developed by each one of us during the individual part. <br>
They test  both engines' functionalities and can be found in the __Tests__ folder. <br>
The reason for end-to-end testing was to make sure that the components were linked properly and to spot any mistakes that were missed during unit testing. <br>
Another important purpose for End-to-End was to enable regression testing, i.e. to make sure that any new feature added to Blue was not corrupting any of the previously existing ones. <br>

### Blue Demo

During the group part I mainly focused on writing code in Blue for Demo purposes. <br>
I wrote two libraries that can be easily imported thanks to a preprocessor by using the keyword `import` and allow you to use all the main helper functions on `Lists` and `Strings`. <br>
The functions show how recursion works smoothly in Beta-Engine Blue without the need of a 'rec' keyword. This is a summary of the libraries' content:<br>

1. **Libraries**

| function | Args | What it does |
| --- | --- | --- |
| listMap | f (fun) lst (list) | Applies `f` to each element of `lst` |
| listReduce | f (function) lst (list) | Accumulates the list `lst` using function `f` |
| listFold | f (fun) acc (int) lst (list) | Accumulates the list `lst` using function `f` into `acc` that can be initialised to anything|
| listFindInt | key (int) lst (list) | Returns true if the element `key` is present in the list, false otherwise|
| listFind | f (fun) key ('a) lst (list) | Returns true if the element `key` and any of the elements in the list `lst` passed to the function `f` return true, false otherwise|
| listConcat | lhs (list) rhs (list) | Returns a single list composed by the concatenation of the two input lists |
| listReverse | lst (list) | Returns `lst` inverted |
| listSplitAt | idx (int) lst (list) | Returns a list of lists composed by the `lst[0-idx]` as one element and the rest as another element |
listSort | f (fun) l (list) | Returns the list `l` sorted using the passed function `f` |
| listItem | idx (int) lst (list) default ('a) | Allows you to randomly access the list at any index `idx`. If the index is out of bound, return value will be set to `default` |
| listContains | f (fun) keys ('a) lst (list) | Returns true if any of the `keys` is present in the list `lst` |
| stringItem | idx (int) str (String) | Random access for strings |
| stringLength | str (String) | Returns the size of the given string |


1. **LexNGram**
   
Having added all of these nice recursive functions that make programming in Blue with strings and lists much easier, I thought of adding a nice more complex program to show in the Demo. <br>
Therefore I wrote a Lexer for a subset of Blue (or more precisely a very similar language), that can be found in the `demo` folder. <br>
The Lexer correctly tokenises the following features:
- LITERALS:
    - INTEGERS [0-9]+
    - STRINGS: ['][^']*[']
    - BOOLS: [true,false]
 
- BUILTINS:
   | +  | -  | /  |  *  | !  |  > |  >= | <  |  <= |  == |
   | ---|-----|---|----|----|----|----|----|-------|-----|
   |Plus| Min |Div| Mul| Not|  Gr | GEq|  Le|   LEq | EqTo|

- LISTS-STRINGS
  - size 
  - head
  - tail
  - append
  - implode
  - explode 
  - strEq

- IDENTIFIER: 
    - any sequence of chars 

### **README-team**

Finally I added a quick readme that explains the language Blue in its main components and features. This file has been later updated by each member, providing details and insights on their specific part of the project.

## **Group Dynamic and Contribution**

I feel that the team has done an amazing job in balancing the workload, communicate and parallelising the load. <br>
Some of us took a stronger leading position within the team, showing others how to deal with some parts that were unclear. In my specific case I think that Marco helped me most by providing explanations as well as debugging support. <br>
I am very pleased by how well everybody shared their doubts, thoughts and suggestions on the groupchat and how precisely everyone met their deadline and delivered a quality result. <br>


