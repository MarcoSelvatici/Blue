### My contributions

<b>Setup / restructure / cleanup the team repository at various stages</b>

Since I extensively worked with git and other versioning systems (e.g. 
Google's g4), I handled all of the critical transactions where there were
chances of messing up the repository, which included:

- resolve big merge conflicts among branches,
- rebasing branches that went out of sync,
- cherry picking useful commits from "broken" branches.

I also setup the repository at the beginning and restructured it several
times, like when we started the team phase and when we introduced the 
Visual2-like editor.

<b>Parser + unit tests</b>

During the individual code phase, I implemented the parser and wrote many 
unit tests for it.

Since I designed the parser, I also designed the main shared types in our
code (Token, Ast).

The parser required no changes during the group phase, aside from when we 
decided to how to represent the empty list.

<b>Type checker + unit tests</b>

During the individual code phase, I implemented the type checker and 
wrote many unit tests for it.

During group phase I changed the type checker by:

- adding a few builtin function types,
- introducing type checking for recursive functions,
- creating a prettyprint function for the types,
- allowing to keep track of the types of all let functions (to implement 
tooltips).

<b>TestLib</b>

I created the simple test lib to have a consistent testing framework 
across all the modules of our code.

<b>Stripped down Visual2 to create a working editor for Blue</b>

I cretaed the intial version of our Editor, by taking Visual2 code and 
removing all of the code and files that we did not need. The resulting
editor could run code and display its result in a popup window.

The UI has then been further improved by Oliver (with the tabs on the 
right section of the screen).

<b>Show types when hovering let expressions</b>

I edited the type checker to return the types of all let expressions,
and added code to create tooltips displaying these types upon hovering.

Note: you need to click the run button to generate/refresh them.

<b>Church numerals demo code</b>

I implemented the demo code for Church numerals.

### General notes

We often helped each other spotting and fixing bugs and we 
always communicated in the group chat about the issues.

I personally felt that the group was well balanced, and everybody did
their work well and reliably, respecting team's deadline.

I think I had a slighly more relevant role in organising team's work and 
in team meetings, when we took decisions about what features we should 
be including and who was going to work on them.
