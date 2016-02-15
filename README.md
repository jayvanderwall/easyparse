# easyparse
A simple python parsing library.

The easyparse module is designed to make parsing [context-free grammars](https://en.wikipedia.org/wiki/Context-free_grammar) as easy as possible. In a nutshell, this means we can recognize text that has recursive properties (like HTML) that regular expressions cannot. There are things that we cannot parse, however, but most parsers out there parse context free grammars, so this isn't a big deal in practice.
Grammars are defined in a reasonably natural syntax by using overloaded operator.
New functions can be easily defined to allow succinct and readable parsing of complicated grammars.

This module is still in alpha and may change substantially in the near future.

## Installing

Simply put the file easyparse.py somewhere in your python path. To find out where that is, run the following python code:

```python
import sys
print '\n'.join(sys.path)
```

## Hello, World

The following program will recognize sentences of the form "Hello, \<something\>" where \<something\> can be replaced by any letters with no spaces between them.

```python
import sys
import easyparse as ep

hello_grammar = ep.Grammar()
hello_grammar.start = hello_grammar.hello ^ hello_grammar.separator + hello_grammar.object
hello_grammar.hello = ep.parse('Hello')
hello_grammar.separator = ep.ignore(',')
hello_grammar.object= ep.parse(re='[A-Za-z]+')

ok, result = hello_grammar.start.parse(sys.stdin.read())
if ok:
  print 'You said {} to {}!'.format(result[0], result[1])
else:
  print 'Your sentence was unrecognized by the grammar, here's what went wrong:'
  ep.print_backtrace(result)
```

This is a pretty simple grammar but contains many of the basic concepts.

Although not strictly necessary, it is convenient to place all rules in a grammar object as we did.
This allows us to refer to rules that we have not yet defined. By doing so we can create grammars in a natural top-down fashion.
The grammar object also gives names to all the rules which will show up in a backtrace if the parsing fails.

The start rule comprises three rules with symbols connecting them. Between hello and separator there is a '^' and between separator and object there is a '+'. 
For many grammars, we want to ignore whitespace between rules. The plus sign does this. Sometimes we do not. It would look weird to have a space between "Hello" and ",", so we disallow the whitespace using the '^' operator.

The hello rule consists entirely of a rule to parse the string "Hello".

The separator rule parses a single comma, then ignores it. This affects only the output of the parser, not how the input is parsed. We'll talk about the output later.

The object rule uses a regular expression by using the "re" keyword argument. The regular expressions are parsed with python's "re" module.

Finally we have some code to read everything from the user, run it through the parser, and get the result. 
Every parse rule has a parse method. There is nothing special about the start rule.

The parse method returns two values: True or False indicating success or failure, and either the parse results on success, or a backtrace on failiure.
We'll talk about the formatting of the results later, but unless you do some extra processing, they will simply be a list of all matches that you do not ignore.
The backtrace returned in case of failure can be inspected directly or printed using the 'print_backtrace' function provided.

Play around with this grammar and various inputs to the program then read the rest of the sections of the readme for a fuller explanation of easyparse's capabilities.

## Parse Rules

The basic unit of easyparse is the parse rule. These are all instances derived from the base class `RuleBase`, but for you likely won't need to directly use any rule classes. easyparse contains functions which create these rules and combine them in useful ways for you.

Rules may be part of a grammar, as in the Hello, World example, or may stand alone. For brevity, the examples will use standalone rules, however all information applies to rules used as part of a grammar.

### Simple Patterns

The simplest rule is one that matches some text exactly: no more and no less. An example is shown below:

```python
rule = ep.parse('abc')
ok, result = rule.parse('abc')
# ok == True
# result == 'abc'
```

### Ignored Patterns

Sometimes we insist that a grammar have some characters that we don't need to process later. A good example is a comma in a list of function arguments. We express this by saying `ignore` instead of `parse` in a rule.

```python
rule = ep.ignore('abc')
ok, result = rule.parse('abc')
# ok == True
# ep.is_ignored(result) == True
```

Note: the result of an ignored rule is the special value `ep.Ignored`. Do not check against this value directly, rather use `ep.is_ignored()` as shown. Likely you won't need to check for an ignored value anyway unless you are writing your own parse rule functions.
