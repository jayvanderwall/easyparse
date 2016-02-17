# Copyright under the ISC license. See LICENSE for full text.
import re
import sys

VERBOSE = False

class Grammar(object):

    def __init__(self):
        object.__setattr__(self, '_real_rules', {})

    def __getattr__(self, name):
        return VirtualRule(self, name)

    def __setattr__(self, name, value):
        self._real_rules[name] = value

class RuleBase(object):

    def __add__(self, other_rule):
        return ConjunctionRule(self, other_rule, ignore_whitespace=True)

    def __xor__(self, other_rule):
        return ConjunctionRule(self, other_rule, ignore_whitespace=False)

    def __or__(self, other_rule):
        return DisjunctionRule(self, other_rule)

    def __gt__(self, transformation_function):
        return TransformationRule(self, transformation_function)

    def parse(self, iterator, *args, **kwargs):
        return self._parse(self._to_buffered_iterator(iterator),
                           *args, **kwargs)

    def _to_buffered_iterator(self, iterator):
        if isinstance(iterator, BufferedIterator):
            return iterator
        return BufferedIterator(iterator)

    def get_rule_type_name(self):
        return 'Rule'

    def get_rule_name(self):
        return ''

    def display(self, string):
        if VERBOSE:
            print(string)

class VirtualRule(RuleBase):
    """A rule that may not exist yet but will be looked up when needed."""

    def __init__(self, grammar, name):
        super(VirtualRule, self).__init__()
        self.grammar = grammar
        self.name = name

    def get_rule_name(self):
        return self.name

    def get_rule_type_name(self):
        return self._lookup_rule().get_rule_type_name()

    def _parse(self, *args, **kwargs):
        ok, result = self._lookup_rule()._parse(*args, **kwargs)
            
        if not ok and result is not None:
            result = self._name_last_backtrace(result)
        return ok, result

    def _lookup_rule(self):
        return self.grammar._real_rules[self.name]

    def _name_last_backtrace(self, backtrace):
        backtrace = (self.get_rule_name(),) + backtrace[1:]
        return backtrace

class PatternRule(RuleBase):

    def __init__(self, pattern, ignore_output=False):
        super(PatternRule, self).__init__()
        self.ignore_output = ignore_output
        self.pattern = pattern

    def _parse(self, buffered_iterator,
               ignore_whitespace=False):
        self.display('Testing pattern {}'.format(self.pattern))

        if self._check_pattern(buffered_iterator, ignore_whitespace):
            if not self.ignore_output:
                output = self.pattern
            else:
                output = Ignored
            ok = True
            self.display('Succeeded')
        else:
            self.display('Failed')
            ok = False
            output = ('', self.get_rule_type_name(), self.pattern)
        return ok, output

    def get_rule_type_name(self):
        return 'Pattern'

    def _check_pattern(self, buffered_iterator, ignore_whitespace):
        can_ignore_whitespace = ignore_whitespace
        for c in self.pattern:
            try:
                next_char = next(buffered_iterator)
                if not next_char.isspace():
                    can_ignore_whitespace = False
                while c != next_char:
                    we_can_ignore_this_space = \
                        next_char.isspace() and can_ignore_whitespace
                    if not we_can_ignore_this_space:
                        return False
                    else:
                        next_char = next(buffered_iterator)
            except StopIteration:
                return False
        return True

class RegularExpressionRule(RuleBase):

    def __init__(self, regular_expression_pattern, ignore_output=False):
        self.regular_expression_text = regular_expression_pattern
        self.pattern = re.compile(regular_expression_pattern)
        self.pattern_ignoring_whitespace = \
            re.compile(r'(\s*)' + regular_expression_pattern)
        self.ignore_output = ignore_output

    def _parse(self, buffered_iterator,
               ignore_whitespace=False):
        self.display('Testing regex {}'.format(
            self.regular_expression_text))
        buffered_iterator.checkpoint()
        line = self._read_until_end_of_line(buffered_iterator)
        match = self.pattern.match(line)
        ok = False
        output = ('', self.get_rule_type_name(),
                  self.regular_expression_text)
        if match:
            match_text = line[match.start():match.end()]
            buffered_iterator.rewind()
            buffered_iterator.discard(len(match_text))
            ok = True
            output = match_text
        elif ignore_whitespace:
            match = self.pattern_ignoring_whitespace.match(line)
            if match:
                all_match_text = line[match.start():match.end()]
                buffered_iterator.rewind()
                buffered_iterator.discard(len(all_match_text))
                ok = True
                output = all_match_text[match.end(1):match.end(0)]
            else:
                buffered_iterator.commit()
        if ok and self.ignore_output:
            output = Ignored
        if not ok:
            self.display('Failed')
        else:
            self.display('Succeeded')
        return ok, output

    def get_rule_type_name(self):
        return 'RegularExpression'

    def _read_until_end_of_line(self, buffered_iterator):
        chars = []
        try:
            while True:
                c = next(buffered_iterator)
                if c == '\n':
                    break
                chars.append(c)
        except StopIteration:
            pass
        return ''.join(chars)

class ConjunctionRule(RuleBase):

    def __init__(self, left_rule, right_rule, ignore_whitespace=False):
        """Create a new conjunction.

        left_rule -- The first rule which must match.

        right_rule -- The second rule which must match after the
        left_rule has.

        ignore_whitespace -- Ignore whitespace between the left and
        right rule. Preceding whitespace must be handled external to
        this rule.

        """
        whitespace_ignore_list = [True, ignore_whitespace]
        self.rules, self.whitespace_ignore_list = \
            flatten_conjunction_rules((left_rule, right_rule),
                                      whitespace_ignore_list)

    def _parse(self, buffered_iterator, ignore_whitespace=False):
        """Parse a conjunction.

        On success, return a ConjunctionList of all results.

        On failure, set the rule-specific field to a list of all
        successful parse results, whether or not they were ignored.

        """
        self.display('Testing conjunction')
        results = ConjunctionList()
        successes = []
        whitespace_ignore_list = \
            [ignore_whitespace] + self.whitespace_ignore_list[1:]
        for idx, (rule, ignore_whitespace) in enumerate(
                zip(self.rules, whitespace_ignore_list)):
            self.display('Trying rule {} (ignore_ws={})'.format(
                idx, ignore_whitespace))
            ok, result = rule._parse(buffered_iterator,
                                     ignore_whitespace=ignore_whitespace)
            if not ok:
                self.display('Failed on rule {}'.format(idx))
                return ok, ('', self.get_rule_type_name(),
                            (successes, result))
            results.extend_or_append(result)
            successes.append((rule.get_rule_name(),
                              rule.get_rule_type_name(),
                              result))
        self.display('Succeded: {}'.format(results))
        return True, results

    def get_rule_type_name(self):
        return 'Conjunction'

def flatten_conjunction_rules(rule_list, whitespace_ignore_list):
    i = 0
    while i < len(rule_list):
        while isinstance(rule_list[i], ConjunctionRule):
            whitespace_ignore_list = whitespace_ignore_list[:i+1] + \
                rule_list[i].whitespace_ignore_list[1:] + \
                whitespace_ignore_list[i+1:]
            rule_list = rule_list[:i] + rule_list[i].rules + \
                rule_list[i+1:]
        i += 1
    return rule_list, whitespace_ignore_list

class ConjunctionList(list):
    """A list class with a unique type to allow us to flatten the results
    of conjunctions without any nasty surprises if the user wanted to
    use lists or tuples or another grouping mechanism.
    """
    def extend_or_append(self, other):
        if isinstance(other, ConjunctionList):
            self.extend(other)
        else:
            self.append(other)

class DisjunctionRule(RuleBase):

    def __init__(self, *rules):
        super(DisjunctionRule, self).__init__()
        self.rules = flatten_disjunction_rules(rules)

    def _parse(self, buffered_iterator, ignore_whitespace=False):
        self.display('Testing disjunction')
        backtraces = []
        for idx, rule in enumerate(self.rules):
            buffered_iterator.checkpoint()
            ok, result = rule._parse(buffered_iterator,
                                     ignore_whitespace=ignore_whitespace)
            if ok:
                self.display('Succeded on rule {}: {}'.format(
                    idx, result))
                buffered_iterator.commit()
                return True, result
            else:
                if result is None:
                    backtraces.append((rule.get_rule_name(),
                                       rule.get_rule_type_name(),
                                       None))
                else:
                    backtraces.append(result)
                buffered_iterator.rewind()
        self.display('Failed')
        assert len(backtraces) == len(self.rules)
        return False, ('', self.get_rule_type_name(), backtraces)

    def get_rule_type_name(self):
        return 'Disjunction'


def flatten_disjunction_rules(rule_list):
    i = 0
    while i < len(rule_list):
        while isinstance(rule_list[i], DisjunctionRule):
            rule_list = rule_list[:i] + rule_list[i].rules + \
                rule_list[i+1:]
        i += 1
    return rule_list


class TransformationRule(RuleBase):

    def __init__(self, left_rule, transformation_function):
        self.left_rule = left_rule
        self.transformation_function = transformation_function

    def _parse(self, buffered_iterator, ignore_whitespace=False):
        ok, result = self.left_rule._parse(buffered_iterator,
            ignore_whitespace=ignore_whitespace)
        if ok:
            self.display('Transforming result')
            try:
                transformed_result = self.transformation_function(result)
            except Exception as ex:
                return False, ('', 'Transformation', result)
            return ok, transformed_result
        else:
            return ok, result

    def get_rule_type_name(self):
        # Note: Like the virtual rule, if someone is querying us from
        # outside, they will really want information on the left rule,
        # not this transformation function itself.
        return self.left_rule.get_rule_type_name()

    def get_rule_name(self):
        return self.left_rule.get_rule_name()

class EofRule(RuleBase):
    """A rule that matches the end of a buffered iterator."""
    def _parse(self, buffered_iterator, ignore_whitespace=False):
        buffered_iterator.checkpoint()
        try:
            while True:
                next_char = next(buffered_iterator)
                if not (ignore_whitespace and next_char.isspace()):
                    break
            buffered_iterator.commit()
            return False, None
        except StopIteration:
            return True, Ignored

    def get_rule_type_name(self):
        return 'Eof'

class BufferedIterator(object):
    def __init__(self, iterator):
        self.iterator = iter(iterator)
        self.history_buffers = []
        self.future_buffer = []

    def __iter__(self):
        return self

    def next(self):
        # Python 2 iterator function
        return self.__next__()

    def __next__(self):
        # Python 3 iterator function
        item = self._get_next_item()
        self._store_item_in_history(item)
        return item

    def _get_next_item(self):
        if self.future_buffer:
            return self.future_buffer.pop(0)
        else:
            return next(self.iterator)

    def _store_item_in_history(self, item):
        if self.history_buffers:
            self.history_buffers[-1].append(item)

    def checkpoint(self):
        self.history_buffers.append([])

    def rewind(self):
        self.future_buffer = \
            self.history_buffers.pop() + self.future_buffer

    def commit(self):
        try:
            top = self.history_buffers.pop()
            self.history_buffers[-1].extend(top)
        except IndexError:
            pass

    def discard(self, count):
        for _ in range(count):
            next(self)

##
# Functions and classes for human-readable backtrace printing.
#

def print_backtrace(backtrace, fout=sys.stdout):
    printer = BacktracePrinter(fout)
    printer.print_backtrace(backtrace)

class BacktracePrinter(object):

    def __init__(self, fout, indent=0):
        self.fout = fout
        self.indent=indent

    def print_backtrace(self, backtrace):
        name, type_, data = backtrace
        self.fout.write(' ' * self.indent)
        self.fout.write("{}: ".format(self._format_rule_name(name)))
        self.fout.write("{}: ".format(type_))
        self.dispatch_print(type_)(data)

    def _format_rule_name(self, name):
        if name != '':
            return name
        else:
            return "<Unnamed>"

    def dispatch_print(self, type_):
        method_name = 'print_{}'.format(type_)
        if hasattr(self, method_name):
            return getattr(self, method_name)
        return self.print_default

    def print_Pattern(self, data):
        self._print_simple_match(data)

    def _print_simple_match(self, data):
        self.fout.write("Failed to match '{}'\n".format(data))

    def print_RegularExpression(self, data):
        self._print_simple_match(data)

    def print_Conjunction(self, data):
        successes, failure = data
        self.fout.write("Failed\n")
        for rule_name, rule_type_name, match in successes:
            self.fout.write(" " * (self.indent + 1))
            self.fout.write("{}: {}: Matched '{}'\n".format(
                self._format_rule_name(rule_name), rule_type_name, match))
        BacktracePrinter(self.fout, indent=self.indent + 1).print_backtrace(
            failure)

    def print_Disjunction(self, data):
        self.fout.write("Failed\n")
        printer = BacktracePrinter(self.fout, indent=self.indent + 1)
        for path in data:
            printer.print_backtrace(path)

    def print_Transformation(self, data):
        self.fout.write("Failed to transform '{}'\n".format(data))

    def print_default(self, data):
        self.fout.write("Failed\n")

##
# Convenience functions and classes derived from the foundational
# classes above.
#

def parse(pattern=None, re=None, **kwargs):
    if pattern is not None:
        return PatternRule(pattern, **kwargs)
    elif re is not None:
        return RegularExpressionRule(re, **kwargs)
    else:
        raise ValueError('Must provide a pattern or regular expression')

def ignore(pattern=None, re=None, **kwargs):
    return parse(pattern=pattern, re=re, ignore_output=True, **kwargs)

def parse_rule(decorated_function):
    """Decorate a function to make it a rule.

    The function may take in any arguments it wants, and must return a
    class derived from RuleBase.

    Note that use of this decorator allows rules to be recursive and
    is optional otherwise (but may be nice for documentation or
    consistency).

    """
    class InnerClass(RuleBase):
        def __init__(self, *args, **kwargs):
            self._expr_func = decorated_function
            self.args = args
            self.kwargs = kwargs
        def _parse(self, buf, **kwargs):
            return self._expr_func(*self.args, **self.kwargs)._parse(buf,
                                                                     kwargs)
        def get_rule_type_name(self):
            if hasattr(decorated_function, '__name__'):
                return decorated_function.__name__
            else:
                return 'UserRule'
    return InnerClass

null = ignore('')

eof = EofRule()

class optional(RuleBase):
    def __init__(self, rule):
        """Parse zero or one occurrences of a rule.

        Equivalent to rule | null, but much faster. The price we pay
        is manualy managing the buffered_iterator.
        """
        self.rule = rule
    def _parse(self, buffered_iterator, ignore_whitespace=False):
        buffered_iterator.checkpoint()
        ok, result = self.rule._parse(buffered_iterator,
                                      ignore_whitespace=ignore_whitespace)
        if ok:
            buffered_iterator.commit()
        else:
            buffered_iterator.rewind()
            result = Ignored
        return True, result
    def get_rule_type_name(self):
        return 'optional'

class many(RuleBase):
    """A rule that matches many occurrences of a pattern. Equivalent to
    optional(rule + many(rule)), but this version uses iteration
    instead of recursion. Whitespace between rules is determined by a
    keyword when creating the rule (default not ignored). Whitespace
    before the rule operates as normal.
    """
    def __init__(self, rule, ignore_whitespace=False):
        self.rule = rule
        self.ignore_whitespace = ignore_whitespace
    def _parse(self, buffered_iterator, ignore_whitespace=False):
        results = ConjunctionList()
        ignore_this_round = ignore_whitespace
        while True:
            _, result = optional(self.rule)._parse(buffered_iterator,
                ignore_whitespace=ignore_this_round)
            if is_ignored(result):
                return True, results
            results.extend_or_append(result)
            ignore_this_round = self.ignore_whitespace
    def get_rule_type_name(self):
        return 'many'

@parse_rule
def join(rule, joiner, ignore_whitespace=False):
    """Act like string.join by combining a rule with a joiner rule in
    between any instances of that rule."""
    if ignore_whitespace:
        return optional(rule + many(joiner + rule))
    else:
        return optional(rule ^ many(joiner ^ rule))

@parse_rule
def at_most_n(rule, count, ignore_whitespace=False):
    """Parse at most some number of instances of a rule."""
    if count == 0:
        return null
    if ignore_whitespace:
        return optional(rule) + at_most_n(rule, count - 1)
    else:
        return optional(rule) ^ at_most_n(rule, count - 1)

@parse_rule
def at_least_n(rule, count, ignore_whitespace=False):
    """Parse at least some number of instances of a rule."""
    if count == 0:
        return many(rule)
    if ignore_whitespace:
        return rule + at_least_n(rule, count - 1)
    else:
        return rule ^ at_least_n(rule, count - 1)

# Make the ignored symbol actually be an empty conjunction list. This
# allows us to deal with it in conjunctions without special code. Note
# that this means [] == Ignored will return True, so don't create a
# case in which it is useful to distinguish between Ignored and an
# empty list.
Ignored = ConjunctionList()

def is_ignored(value):
    return isinstance(value, ConjunctionList) and len(value) == 0
