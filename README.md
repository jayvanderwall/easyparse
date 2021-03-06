# easyparse
A simple python parsing library.

The easyparse module is designed to make parsing [context-free grammars](https://en.wikipedia.org/wiki/Context-free_grammar) as easy as possible. In a nutshell, this means we can recognize text that has recursive properties (like HTML) that regular expressions cannot. There are things that we cannot parse, however, but most parsers out there parse context free grammars, so this isn't a big deal in practice.
Grammars are defined in a reasonably natural syntax by using overloaded operators.
New functions can be easily defined to allow succinct and readable parsing of complicated grammars.

This module is still in alpha and may change substantially in the near future. It has been testing in both Python 2.7.6 and Python 3.4.3.

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
  print "Your sentence was unrecognized by the grammar, here's what went wrong:"
  ep.print_backtrace(result)
```

This is a pretty simple grammar but contains many of the basic concepts.

Although not strictly necessary, it is convenient to place all rules in a grammar object as we did.
This allows us to refer to rules that we have not yet defined. By doing so we can create grammars in a natural top-down fashion.
The grammar object also gives names to all the rules which will show up in a backtrace if the parsing fails.

The start rule comprises three rules with symbols connecting them. Between hello and separator there is a `^` and between separator and object there is a `+`. 
For many grammars, we want to ignore whitespace between rules. The plus sign does this. Sometimes we do not. It would look weird to have a space between "Hello" and ",", so we disallow the whitespace using the `^` operator.

The hello rule consists entirely of a rule to parse the string "Hello".

The separator rule parses a single comma, then ignores it. This affects only the output of the parser, not how the input is parsed. We'll talk about the output later.

The object rule uses a regular expression by using the `re` keyword argument. The regular expressions are parsed with python's `re` module.

Finally we have some code to read everything from the user, run it through the parser, and get the result. 
Every parse rule has a parse method. There is nothing special about the start rule.

The parse method returns two values: `True` or `False` indicating success or failure, and either the parse results on success, or a backtrace on failiure.
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

### Regular Expressions

Although not strictly necessary, a parse rule can be expressed using a regular expression for convenience. Either the `parse()` or `ignore()` function may be used with the `re` keyword to use a regular expression. All regular expressions are processed by reading all text until the next newline and sending it through the Python regular expression library.

```python
rule = ep.parse(re=r'-?[0-9]+\.[0-9]+')  # Match floating point not in scientific notation
ok, result = rule.parse('3.14')
# ok == True
# result == '3.14'
```

It is a good idea to use Python's raw strings (prefix with `r`) when using regular expressions.

### EOF

A special rule is necessary to match the end of input, since it is technically not a character. This is called `eof` in the easyparse module.

### Conjunctions

A fancy word for including one thing and another and another. In easyparse there are two kinds of conjunctions: one that allows space (`+`) and one that disallows it (`^`). If a rule contains multiple rules separated by either operator, then all its subrules must match for that rule to match. The return value of the rule will be a list of all return values from the subrules.

```python
rule = ep.parse(re='[a-z]+') + ep.ignore('<-') + ep.parse(re='[a-z]+')
ok, result = rule.parse('foo = value')
# ok == True
# result == ['foo', 'value']
```

### Disjunctions

Another fancy word meaning one thing or another or another. Different possibilities are separated by the `|` (pipe) operator. Unlike some parsers, and like other parser combinators, easyparse always checks rules in a disjunction from left to right and stops after any of them match. This is called short-circuit evaluation. This keeps the parser simple and makes reasoning about it simple in most cases, but there are cases where a different algorithm may accept what easyparse rejects using the same grammar. In practice this restriction is usually not a big deal.

The return value of a disjunction is the return value of the chosen subrule.

```python
rule = ep.parse(re='[a-z]+') | ep.parse(re='[0-9]+')
ok, result = rule.parse('abc')
# ok == True
# result == 'abc'
ok, result = rule.parse('123')
# ok == True
# result == '123'
```

### Transformations

Until now we have only touched a little on what we do after we match a rule. In some cases, the grammar can directly solve our problem. An example often given in parsers is a calculator. Each rule has an associated transformation (called actions in some parsers) that is performed when the rule matches. This is denoted by a `>` sign from the rule to a function taking in the rule's return value. In other cases, we simply wish to have a data structure that we can post-process. ASTs are a good example of this. And in other cases we may simply be scanning a text file and want to pick out certain fields.

In easyparse, each rule can be thought of as a function. It takes in text from its input stream, and if it matches, sends its match or matches to its transformation function. If you provide no transformation, you get either the matched text, or a list of all matches (in the case of conjunctions).

```python
regular_rule = ep.parse(re='-?[0-9]+')
transformed_rule = ep.parse(re='-?[0-9]+') > int
ok, result = regular_rule.parse('-15')
# ok == True
# result == '-15'
# type(result) == str
ok, result = transformed_rule.parse('-15')
# ok == True
# result == -15
# type(result) == int
```

In the above example, we used the built-in function `int` to transform a string containing an integer to an integer. The next example shows how to use multiple transformations to process text. Because the example is more complex, we use a grammar object, but that does not change the concepts involved.

```python
g = ep.Grammar()
g.term = g.num + ep.ignore('+') + g.term > sum
g.num = ep.parse(re='-?[0-9]+') > int
ok, result = g.term.parse('5 + 7 + 3')
# ok == True
# result == 15
```

Until now, the return values of rules have been either strings or lists of strings. But here the return value for the `num` rule is transformed into an integer. As the `term` rule is parsed, it builds up a list of integers. Although perhaps not obvious from this example, it is the case that if a conjunction calls another conjunction recursively, the results of the matched rules are all placed in one list. As an example, without the transformations the result of `g.term` above would have been `['5', '7', '3']`, **not** `['5', ['7', '3']]`. If you want the latter behavior, use a transformation! The `list` function in python will convert anything list-like into an actual list. easyparse knows the difference between a true Python list like `list` returns and its own internal lists of parse results and will not flatten out any lists you create.

The above result uses recursion, meaning a rule appears both on the left and right of the equals sign. Notice that the rule was not written as follows.

```python
g.term = g.term + ep.ignore('+') + g.num > sum  # BAD!
```

This would have caused an infinite loop as easyparse works from left to right.

We can also make our own functions. Each function must take a single argument: the return value of the rule. This is either a string from a normal pattern, a list of strings, or the result of a previous transformation (or list of previous results and so on). You will always know if you are receiving a list or a single result based on whether your rule is a conjunction (i.e. has a `+` or `^` operator). **Note:** If you have a rule that is a disjunction (or) with one possibility being a conjunction (and) and another not, then you will receive a list if the disjunction matches and a single element if the other matches, much as you would expect. You may wish to break up your grammar to separate out these different transformations.

In this example we take indices from Lua, which are 1-based, and transform them into Python, which are 0-based.
```python
def string_minus_one(s):
  int_value = int(s)
  return str(int_value - 1)

g = ep.Grammar()
g.indexed_identifier = g.identifier + ep.ignore('[') + g.index + ep.ignore(']') > ''.join
g.identifier = ep.parse(re=r'[a-zA-Z_]+')
g.index = ep.parse(re=r'[0-9]+') > string_minus_one

ok, result = g.indexed_identifier.parse('foo[1]')
# ok == True
# result == 'foo[0]'
```

Note that we had to define the function first, otherwise the Python interpreter doesn't know about it. We also used `''.join` to concatenate a list of strings. This is a standard Python idiom.

## Convenience Rules

With a simple pattern rule, conjunction, and disjunction, any grammar can be built. However, to make life easier, several convenience functions are defined. Some of these are rules, while others are better thought of as functions operating on rules. These might be known as higher-order parse rules if you're into that. They're easier than that sounds.

### null

This parse rule always succeeds and consumes no input. The most common
use is as a fallback alternative when you want to match one of several
parse rules or none of them.

```python
rule = ep.parse('a') | ep.parse('b') | ep.null
ok, result = rule.parse('q')
# ok == True
# ep.is_ignored(result) == True
```

The `optional` rule often can replace use of the `null` rule.

### optional

This function takes a single argument and returns a rule that either
matches the rule or doesn't. `optional(r)` is equivalent to `r |
null`.

```python
rule = ep.optional(ep.parse('a')) + ep.parse('b')
ok, result = rule.parse('ab')
# ok == True
# result == ['a', 'b']
ok, result = rule.parse('b')
# ok == True
# result == ['a', 'b']
```

### many

Sometimes called the Kleene star operator, this creates a parse rule
that matches another parse rule repeated zero or more times. Using the
keyword ignore_whitespace you can decide whether whitespace is allowed
between the repetitions of the rule. The default is to disallow
whitespace. The rule `many(r)` is mostly equivalent to `optional(r ^
many(r))`, but the function is not recursive, allowing it to match
arbitrarily long strings. The return value is a list of the outputs of
the rule that matched.

```python
rule = ep.many(ep.parse(re='[abc]+'), ignore_whitespace=True)
ok, result = rule.parse('abcabc abc')
# ok == True
# result == ['abcabc', 'abc']
```

### repeat

Where the many operator matches anywhere from zero to infinity of a
patten, sometimes we want tighter bounds. The repeat function allows
us to use a rule, a minimum bound, and an optional maximum bound and
get a rule that matches between those bounds. A minimum bound of 0
with no maximum given is the same as `many`.

## Defining your own rules

There are many ways to define your own parse rules. We've already seen
how to make rules that are part of a grammar or stand by
themselves. But we can also define higher-order rules like
`optional`. There are three ways to do this:

1. Create a function that takes in a rule and spits out another
   rule. This may not use recursion.
2. Create a function with the `parse_rule` decorator. This may use
   recursion.
3. Create a class that derives from RuleBase and overrides the
   `_parse` and `get_rule_type_name` methods.

This simplest is option one. The `optional` rule could be implemented
using this method as follows:

```python
def optional(rule):
  return rule | null
```

If, however, a rule function must call itself, this may not work. The entire rule will be fully expanded when it is created, and this may create an infinite loop. The following naive implementation of `many` will not work, for instance.

```python
# BAD
def many(rule):
  return optional(rule) + many(rule)
```

The reason is a little subtle, and it's ok if you quite see why. If
you always use the `parse_rule` decorator, you will never get into
trouble. It also helps mark your intention to make this a function
usable in parse rules.

```python
# GOOD
@parse_rule
def many(rule):
  return optional(rule + many(rule))
```

You'll notice the only difference between the bad and good examples is
the use of the decorator. The decorator in effect delays running the
function until you actually need to parse something with it. So even
though the many rule is recursive, once a rule fails to parse it will
stop recursing.

If you look at the code for easyparse, you will see that `many` is not
in fact implemented with this decorator. The reason is that each
recursive call causes Python to make another function call. Eventually
Python reaches a limit of function calls that have not returned, and
quits. To avoid this, we have to create a custom class. This is
generally not necessary except when using the above options is too
slow or consumes too many resources.

When creating a custom class, you must derive it from the
`RuleBase`. Then you must override the `_parse` (note the underscore)
method taking a buffered iterator and the keyword
allow_whitespace. This method must return two arguments: a boolean
indicating success and the parsed data on success or a traceback on
failure. Finally, it is recommended to override the
`get_rule_type_name` method and return a string uniquely representing
this rule (likely the name of the class). This is used in creating
backtraces.

See the code in easyparse for an example. Creating a custom class is a
bit more involved than this README should cover.
