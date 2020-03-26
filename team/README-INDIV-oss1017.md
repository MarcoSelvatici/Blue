### Individual Contributions

The majority of my contribution to the group stage of the project was in the Visual2 based GUI.
I also extended my runtime and contributed to other parts of the project when needed.

#### SKI runtime and tests
**Overview**  
The runtime created in the individual phase of the project needed a few changes
in order to properly fit into the group deliverable:
- Error reporting was slightly adapted to stay consistent with other modules 
- Code was altered to work with the final revision of our shared types  

The code was then extended to support some new built-in functions such as 
print and append. 

Sequences were changed to be fully evaluated as that wasn't the case before (e.g. Now: [1+1,1-1] -> [2,0], Before: [1+1,1-1] -> [1+1,1-1])

**Recursion**  
Recursion was never properly implemented during the individual phase. 
Recursion with one argument was supported but I had aimed to implement it completely
and properly using the Y combinator in the group phase. 

This proved to be more difficult than anticipated due to the structure of
my code from the individual stage. I extended my original code to support two arguments
but decided not to include it as it was not using combinator reduction and led to messy code.

Seeing as the Beta reduction engine was implemented and functional I decided to focus on 
different sections of the project, mainly the GUI.

**Testing**  
Testing was done in the form of unit tests for individual functions as well as broader tests 
which cover the full runtime.

Some tests also evaluate the full end to end performance of our project:  
Lexer > Parser > Runtime


#### Visual GUI
I worked on altering and extending the Visual2 codebase to properly function with our language and
display its features. 
The functionality to easily switch between the two runtimes and enable/disable type checking was added. 
The output of programs run inside the Blue language editor are also displayed on the side tab.

I also implemented pretty-printing to be used in combination with the GUI to 
return more human-readble results or expressions.

#### Windows compatibility check
Being the only group member with a windows device, I was given the responsibility of checking if all was functional
and able to be built on windows.  
During the group stage, a few issues related to DOS line endings were identified. 
I had to slightly alter both the preprocessor and lexer to support Unix (\n) and DOS (\r\n) line endings.

### Group Dynamic and contributions
All in all, I feel like responsibilities were divided fairly with all members completing their tasks on time.
There was good communication and collaboration on different parts throughout the project.  
All team member's contributions were strong and relevant to the group effort with Marco
contributing more overall as far as both code and management/overview are concerned.

The fact that I was unfamiliar with HTML/CSS means that I spent more time than should be necessary implementing the
GUI features. My contribution as far as delivered code is therefore probably lower than the rest of the group's.



