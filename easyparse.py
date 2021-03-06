# Copyright under the ISC license. See LICENSE for full text.
import re
import sys
import collections
import operator

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
        return ConjunctionRule(self, other_rule, allow_whitespace=True)

    def __xor__(self, other_rule):
        return ConjunctionRule(self, other_rule, allow_whitespace=False)

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
        backtrace = Backtrace(self.get_rule_name(),
                              backtrace.rule_type_name,
                              backtrace.data)
        return backtrace

class PatternRule(RuleBase):

    def __init__(self, pattern, ignore_output=False):
        super(PatternRule, self).__init__()
        self.ignore_output = ignore_output
        self.pattern = pattern

    def _parse(self, buffered_iterator,
               allow_whitespace=False):
        self.display('Testing pattern {}'.format(self.pattern))

        if self._check_pattern(buffered_iterator, allow_whitespace):
            if not self.ignore_output:
                output = self.pattern
            else:
                output = Ignored
            ok = True
            self.display('Succeeded')
        else:
            self.display('Failed')
            ok = False
            output = Backtrace('', self.get_rule_type_name(), self.pattern)
        return ok, output

    def get_rule_type_name(self):
        return 'Pattern'

    def _check_pattern(self, buffered_iterator, allow_whitespace):
        can_allow_whitespace = allow_whitespace
        for c in self.pattern:
            try:
                next_char = next(buffered_iterator)
                if not next_char.isspace():
                    can_allow_whitespace = False
                while c != next_char:
                    we_can_ignore_this_space = \
                        next_char.isspace() and can_allow_whitespace
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
               allow_whitespace=False):
        self.display('Testing regex {}'.format(
            self.regular_expression_text))
        match_text = self._match_pattern(buffered_iterator, allow_whitespace)
        if match_text is not None:
            ok = True
            if self.ignore_output:
                output = Ignored
            else:
                output = match_text
        else:
            ok = False
            output = Backtrace('', self.get_rule_type_name(),
                               self.regular_expression_text)
        self.display('Succeeded' if ok else 'Failed')
        return ok, output

    def _match_pattern(self, buffered_iterator, allow_whitespace):
        buffered_iterator.checkpoint()
        consumed_count = None
        try:
            line = self._read_until_end_of_line(buffered_iterator)
            match_text, consumed_count = \
                self._get_matching_text_and_length_or_none(
                    line, allow_whitespace)
        finally:
            buffered_iterator.commit(consumed_count)
        return match_text

    def _get_matching_text_and_length_or_none(self, line, allow_whitespace):
        match = self.pattern.match(line)
        match_text = None
        consumed_count = None
        if match:
            match_text = line[match.start():match.end()]
            consumed_count = len(match_text)
        elif allow_whitespace:
            match = self.pattern_ignoring_whitespace.match(line)
            if match:
                match_text_with_ws = line[match.start():match.end()]
                consumed_count = len(match_text_with_ws)
                match_text = match_text_with_ws[match.end(1):match.end(0)]
        return match_text, consumed_count

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

    def get_rule_type_name(self):
        return 'RegularExpression'

class ConjunctionRule(RuleBase):

    def __init__(self, left_rule, right_rule, allow_whitespace=False):
        """Create a new conjunction.

        left_rule -- The first rule which must match.

        right_rule -- The second rule which must match after the
        left_rule has.

        allow_whitespace -- Ignore whitespace between the left and
        right rule. Preceding whitespace must be handled external to
        this rule.

        """
        whitespace_ignore_list = [True, allow_whitespace]
        self.rules, self.whitespace_ignore_list = \
            flatten_conjunction_rules((left_rule, right_rule),
                                      whitespace_ignore_list)

    def _parse(self, buffered_iterator, allow_whitespace=False):
        """Parse a conjunction.

        On success, return a ConjunctionList of all results.

        On failure, set the rule-specific field to a list of all
        successful parse results, whether or not they were ignored.

        """
        self.display('Testing conjunction')
        results = ConjunctionList()
        successes = []
        whitespace_ignore_list = \
            [allow_whitespace] + self.whitespace_ignore_list[1:]
        for idx, (rule, allow_whitespace) in enumerate(
                zip(self.rules, whitespace_ignore_list)):
            self.display('Trying rule {} (ignore_ws={})'.format(
                idx, allow_whitespace))
            ok, result = rule._parse(buffered_iterator,
                                     allow_whitespace=allow_whitespace)
            if not ok:
                self.display('Failed on rule {}'.format(idx))
                return ok, Backtrace('', self.get_rule_type_name(),
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

    def _parse(self, buffered_iterator, allow_whitespace=False):
        self.display('Testing disjunction')
        backtraces = []
        for idx, rule in enumerate(self.rules):
            buffered_iterator.checkpoint()
            ok, result = rule._parse(buffered_iterator,
                                     allow_whitespace=allow_whitespace)
            if ok:
                self.display('Succeded on rule {}: {}'.format(
                    idx, result))
                buffered_iterator.commit()
                return True, result
            else:
                if result is None:
                    backtraces.append(Backtrace(rule.get_rule_name(),
                                                rule.get_rule_type_name(),
                                                None))
                else:
                    backtraces.append(result)
                buffered_iterator.rewind()
        self.display('Failed')
        assert len(backtraces) == len(self.rules)
        return False, Backtrace('', self.get_rule_type_name(), backtraces)

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

    def _parse(self, buffered_iterator, allow_whitespace=False):
        ok, result = self.left_rule._parse(buffered_iterator,
            allow_whitespace=allow_whitespace)
        if ok:
            self.display('Transforming result')
            try:
                transformed_result = self.transformation_function(result)
            except Exception as ex:
                return False, Backtrace('', 'Transformation', result)
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
    def _parse(self, buffered_iterator, allow_whitespace=False):
        buffered_iterator.checkpoint()
        try:
            while True:
                next_char = next(buffered_iterator)
                if not (allow_whitespace and next_char.isspace()):
                    break
            buffered_iterator.commit()
            return False, Backtrace('', self.get_rule_type_name(), None)
        except StopIteration:
            return True, Ignored

    def get_rule_type_name(self):
        return 'Eof'

Backtrace = collections.namedtuple('Backtrace',
                                   ['rule_name', 'rule_type_name', 'data'])

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

    def commit(self, length=None):
        top = self.history_buffers.pop()
        top_history = top[:length]
        top_future = top[len(top_history):]
        self.future_buffer = top_future + self.future_buffer
        try:
            self.history_buffers[-1].extend(top_history)
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
    def _parse(self, buffered_iterator, allow_whitespace=False):
        buffered_iterator.checkpoint()
        ok, result = self.rule._parse(buffered_iterator,
                                      allow_whitespace=allow_whitespace)
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
    optional(rule ^ many(rule)), but this version uses iteration
    instead of recursion. Whitespace between rules is determined by a
    keyword when creating the rule (default not allowed). Whitespace
    before the rule operates as normal.
    """
    def __init__(self, rule, allow_whitespace=False):
        self.rule = rule
        self.allow_whitespace = allow_whitespace
    def _parse(self, buffered_iterator, allow_whitespace=False):
        results = ConjunctionList()
        allow_this_round = allow_whitespace
        while True:
            _, result = optional(self.rule)._parse(buffered_iterator,
                allow_whitespace=allow_this_round)
            if is_ignored(result):
                return True, results
            results.extend_or_append(result)
            allow_this_round = self.allow_whitespace
    def get_rule_type_name(self):
        return 'many'

def many_ws(rule):
    """The same as the many rule with the allow_whitespace keyword set to
    True.
    """
    return many(rule, allow_whitespace=True)

@parse_rule
def join(rule, joiner):
    """Act like string.join by combining a rule with a joiner rule in
    between any instances of that rule. Does not allow whitespace."""
    return optional(rule ^ many(joiner ^ rule))

@parse_rule
def join_ws(rule, joiner):
    """Same as the join rule but allows whitespace."""
    return optional(rule + many(joiner + rule))

@parse_rule
def repeat(rule, from_count, to_count=None, allow_whitespace=False):
    """Allow between from_count and to_count repetitions of a rule.

    If to_count is not given, then allow as many repetitions as can be
    parsed.
    """

    if to_count == 0:
        return null

    if from_count == 0 and to_count is None:
        return many(rule, allow_whitespace=allow_whitespace)

    op = operator.__add__ if allow_whitespace else operator.__xor__
    next_to_count = to_count - 1 if to_count is not None else None
    next_from_count = from_count - 1 if from_count > 0 else 0

    first_part = optional(rule) if from_count == 0 else rule

    return op(first_part, repeat(rule, next_from_count, next_to_count,
                                 allow_whitespace))

def repeat_ws(rule, from_count, to_count=None):
    """Same as the repeat rule with allow_whitespace set to True."""
    return repeat(rule, from_count, to_count, allow_whitespace=True)

# Make the ignored symbol actually be an empty conjunction list. This
# allows us to deal with it in conjunctions without special code. Note
# that this means [] == Ignored will return True, so use the
# is_ignored function for this check.
Ignored = ConjunctionList()

def is_ignored(value):
    return isinstance(value, ConjunctionList) and len(value) == 0
