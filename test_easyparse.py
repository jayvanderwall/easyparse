#!/usr/bin/env python
import unittest
import re
import pdb
import operator
import cProfile
try:
    # Python 2
    from StringIO import StringIO
except ImportError:
    # Python 3
    from io import StringIO

import easyparse

class FunctionalTest(unittest.TestCase):

    def test_parse_digit(self):
        parser = easyparse.Grammar()
        parser.digit = easyparse.parse('0') > int
        ok, result = parser.digit.parse('0')
        self.assertTrue(ok)
        self.assertEqual(result, 0)

    def test_parse_recursive(self):
        parser = easyparse.Grammar()
        parser.digits = (easyparse.parse('0') | easyparse.parse('1')) +\
                        parser.digits | easyparse.ignore('')
        ok, result = parser.digits.parse('101')
        self.assertTrue(ok)
        self.assertEqual(['1', '0', '1'], result)

    def test_parse_regex_string(self):
        parser = easyparse.Grammar()
        parser.string = easyparse.parse(re='"[^"]*"')
        ok, result = parser.string.parse('"abc"')
        self.assertTrue(ok)
        self.assertEqual(result, '"abc"')

    def test_parse_list(self):
        parser = self._make_list_grammar()
        ok, result = parser.list_rule.parse('[1, 2]')
        self.assertTrue(ok)
        self.assertEqual([1,2], result)

    def _make_list_grammar(self):
        parser = easyparse.Grammar()
        parser.list_rule = easyparse.ignore('[') + (parser.list_items | easyparse.ignore('')) + easyparse.ignore(']') > list
        parser.list_items = parser.item + (easyparse.ignore(',') + parser.list_items | easyparse.ignore(''))
        parser.item = easyparse.parse(re='[0-9]') > int
        return parser

    def test_parse_list_with_trailing_comma(self):
        """Test that the grammar does not allow a list with a trailing
        comma"""
        parser = self._make_list_grammar()
        ok, result = parser.list_rule.parse('[1,]')
        self.assertFalse(ok)

    def test_parse_dictionary(self):
        parser = self._make_dict_grammar()
        ok, result = parser.dict_rule.parse('{7: 5, 2:3}')
        self.assertTrue(ok)
        self.assertEqual(result, {7:5, 2:3})

    def _make_dict_grammar(self):
        parser = easyparse.Grammar()
        parser.dict_rule = easyparse.ignore('{') + (parser.item_tuple_list | easyparse.ignore('')) + easyparse.ignore('}') > dict
        parser.item_tuple_list = parser.item_tuple + (easyparse.ignore(',') + parser.item_tuple_list | easyparse.ignore(''))
        parser.item_tuple = parser.item + easyparse.ignore(':') + parser.item > tuple
        parser.item = easyparse.parse(re='[0-9]') > int
        return parser

    def test_parse_dictionary_trailing_comma(self):
        parser = self._make_dict_grammar()
        ok, result = parser.dict_rule.parse('{7:2,}')
        self.assertFalse(ok)

    def test_parse_large_stack_with_many(self):
        rule = easyparse.many(easyparse.parse('a'))
        ok, result = rule.parse('a' * 1000)
        self.assertTrue(ok)
        self.assertEqual(['a'] * 1000, result)

    def test_parse_large_stack_with_join(self):
        rule = easyparse.join(easyparse.parse('a'),
                              easyparse.ignore(';'))
        ok, result = rule.parse('a;' * 999 + 'a')
        self.assertTrue(ok)
        self.assertEqual(['a'] * 1000, result)

    def test_method_call_syntax(self):
        grammar = self._make_method_call_grammar()
        ok, result = grammar.start.parse('foo.bar(a, b)')
        if not ok:
            easyparse.print_backtrace(result)
        self.assertTrue(ok)
        self.assertEqual(['foo', 'bar', ('a', 'b')], result)
        self.assertFalse(grammar.start.parse('foo. bar(a)')[0])
        self.assertFalse(grammar.start.parse('foo .bar(a)')[0])
        self.assertTrue(grammar.start.parse('foo.bar( a)')[0])

    def _make_method_call_grammar(self):
        grammar = easyparse.Grammar()
        grammar.start = easyparse.null + grammar.method_call
        grammar.method_call = grammar.method_object + grammar.arglist
        grammar.method_object = grammar.instance_name ^\
                                easyparse.ignore('.') ^\
                                grammar.method_name
        grammar.instance_name = grammar.identifier
        grammar.method_name = grammar.identifier
        grammar.arglist = easyparse.ignore('(') + \
                          easyparse.join(grammar.arg,
                                         easyparse.ignore(','),
                                         allow_whitespace=True) + \
                          easyparse.ignore(')') > tuple
        grammar.arg = grammar.identifier
        grammar.identifier = easyparse.parse(re=r'[a-zA-Z_]+')
        return grammar

class TestBacktrace(unittest.TestCase):

    def test_parse_rule_failure(self):
        rule = easyparse.PatternRule('a')
        ok, backtrace = rule.parse('b')
        self.assertFalse(ok)
        self.assertEqual(('', 'Pattern', 'a'), backtrace)

    def test_re_rule_failure(self):
        rule = easyparse.RegularExpressionRule('a')
        ok, backtrace = rule.parse('b')
        self.assertFalse(ok)
        self.assertEqual(('', 'RegularExpression', 'a'), backtrace)

    def test_conjunction_rule_failure(self):
        rule = easyparse.ConjunctionRule(
            easyparse.PatternRule('a'),
            easyparse.PatternRule('b'))
        ok, backtrace = rule.parse('aa')
        self.assertFalse(ok)
        self.assertEqual(('', 'Conjunction',
                          ([('', 'Pattern', 'a')],
                           ('', 'Pattern', 'b'))),
                         backtrace)
        ok, backtrace = rule.parse('ba')
        self.assertFalse(ok)
        self.assertEqual(('', 'Conjunction',
                          ([],
                           ('', 'Pattern', 'a'))),
                         backtrace)

    def test_disjunction_rule_failure(self):
        rule = easyparse.DisjunctionRule(
            easyparse.PatternRule('a'),
            easyparse.PatternRule('b'))
        ok, backtrace = rule.parse('c')
        a_backtrace = ('', 'Pattern', 'a')
        b_backtrace = ('', 'Pattern', 'b')
        inner_backtraces = [a_backtrace, b_backtrace]
        self.assertFalse(ok)
        self.assertEqual(('', 'Disjunction', inner_backtraces), backtrace)

    def test_named_rule_backtrace(self):
        """Test that the generic rule name is replaced by a specific one
        when the rule is part of a Grammar object."""
        g = easyparse.Grammar()
        g.ident = easyparse.PatternRule('a')
        ok, backtrace = g.ident.parse('b')
        self.assertFalse(ok)
        self.assertEqual(('ident', 'Pattern', 'a'), backtrace)

    def test_exception_in_transformation(self):
        rule = easyparse.TransformationRule(easyparse.parse('a'), int)
        ok, backtrace = rule.parse('a')
        self.assertFalse(ok)
        self.assertEqual(('', 'Transformation', 'a'), backtrace)

    def test_exception_in_named_transformation(self):
        g = easyparse.Grammar()
        g.integer = easyparse.TransformationRule(easyparse.parse('a'), int)
        ok, backtrace = g.integer.parse('a')
        self.assertFalse(ok)
        self.assertEqual(('integer', 'Transformation', 'a'), backtrace)

class TestPrintBacktrace(unittest.TestCase):

    def test_print_backtrace_pattern(self):
        rule = easyparse.PatternRule('a')
        ok, backtrace = rule.parse('b')
        self.assertFalse(ok)
        backtrace_string = self._print_backtrace_to_string(backtrace)
        exp_backtrace_string = \
            """<Unnamed>: Pattern: Failed to match 'a'\n"""
        self.assertEqual(exp_backtrace_string, backtrace_string)

    def _print_backtrace_to_string(self, backtrace):
        s = StringIO()
        easyparse.print_backtrace(backtrace, fout=s)
        ret = s.getvalue()
        s.close()
        return ret

    def test_print_backtrace_named_pattern(self):
        g = easyparse.Grammar()
        g.ident = easyparse.PatternRule('a')
        ok, backtrace = g.ident.parse('b')
        self.assertFalse(ok)
        backtrace_string = self._print_backtrace_to_string(backtrace)
        exp_backtrace_string = \
            """ident: Pattern: Failed to match 'a'\n"""
        self.assertEqual(exp_backtrace_string, backtrace_string)

    def test_print_backtrace_regex(self):
        rule = easyparse.RegularExpressionRule('[0-9]')
        ok, backtrace = rule.parse('q')
        self.assertFalse(ok)
        backtrace_string = self._print_backtrace_to_string(backtrace)
        exp_backtrace_string = \
            """<Unnamed>: RegularExpression: Failed to match '[0-9]'\n"""
        self.assertEqual(exp_backtrace_string, backtrace_string)

    def test_print_backtrace_named_regex(self):
        g = easyparse.Grammar()
        g.num = easyparse.RegularExpressionRule('[0-9]')
        ok, backtrace = g.num.parse('q')
        self.assertFalse(ok)
        backtrace_string = self._print_backtrace_to_string(backtrace)
        exp_backtrace_string = \
            """num: RegularExpression: Failed to match '[0-9]'\n"""
        self.assertEqual(exp_backtrace_string, backtrace_string)

    def test_print_backtrace_conjunction(self):
        rule = easyparse.ConjunctionRule(
            easyparse.PatternRule('a'),
            easyparse.PatternRule('0'))
        ok, backtrace = rule.parse('a1')
        self.assertFalse(ok)
        backtrace_string = self._print_backtrace_to_string(backtrace)
        exp_backtrace_string = '\n'.join([
            "<Unnamed>: Conjunction: Failed",
            " <Unnamed>: Pattern: Matched 'a'",
            " <Unnamed>: Pattern: Failed to match '0'\n"])
        self.assertEqual(exp_backtrace_string, backtrace_string)

    def test_print_backtrace_named_conjunction(self):
        g = easyparse.Grammar()
        g.start = easyparse.ConjunctionRule(
            g.a_rule, g.num)
        g.a_rule = easyparse.parse('a')
        g.num = easyparse.parse('0')
        ok, backtrace = g.start.parse('a1')
        self.assertFalse(ok)
        backtrace_string = self._print_backtrace_to_string(backtrace)
        exp_backtrace_string = '\n'.join([
            "start: Conjunction: Failed",
            " a_rule: Pattern: Matched 'a'",
            " num: Pattern: Failed to match '0'\n"])
        self.assertEqual(exp_backtrace_string, backtrace_string)

    def test_print_backtrace_disjunction(self):
        rule = easyparse.DisjunctionRule(
            easyparse.parse('a'),
            easyparse.parse('b'))
        ok, backtrace = rule.parse('c')
        self.assertFalse(ok)
        backtrace_string = self._print_backtrace_to_string(backtrace)
        exp_backtrace_string = '\n'.join([
            "<Unnamed>: Disjunction: Failed",
            " <Unnamed>: Pattern: Failed to match 'a'",
            " <Unnamed>: Pattern: Failed to match 'b'\n"])
        self.assertEqual(exp_backtrace_string, backtrace_string)

    def test_print_backtrace_named_disjunction(self):
        g = easyparse.Grammar()
        g.choice = easyparse.DisjunctionRule(
            easyparse.parse('a'),
            easyparse.parse('b'))
        ok, backtrace = g.choice.parse('c')
        self.assertFalse(ok)
        backtrace_string = self._print_backtrace_to_string(backtrace)
        exp_backtrace_string = '\n'.join([
            "choice: Disjunction: Failed",
            " <Unnamed>: Pattern: Failed to match 'a'",
            " <Unnamed>: Pattern: Failed to match 'b'\n"])
        self.assertEqual(exp_backtrace_string, backtrace_string)

    def test_print_multiple_disjunction_levels(self):
        g = easyparse.Grammar()

        g.top = easyparse.parse('a') | g.inner
        g.inner = easyparse.parse('b') + g.inner_inner
        g.inner_inner = easyparse.parse('c') | easyparse.parse('d')

        ok, backtrace = g.top.parse('bq')

        self.assertFalse(ok)
        backtrace_string = self._print_backtrace_to_string(backtrace)
        exp_backtrace_string = '\n'.join([
            "top: Disjunction: Failed",
            " <Unnamed>: Pattern: Failed to match 'a'",
            " inner: Conjunction: Failed",
            "  <Unnamed>: Pattern: Matched 'b'",
            "  inner_inner: Disjunction: Failed",
            "   <Unnamed>: Pattern: Failed to match 'c'",
            "   <Unnamed>: Pattern: Failed to match 'd'\n"])
        self.assertEqual(exp_backtrace_string, backtrace_string)

    def test_print_backtrace_transformation(self):
        """Test the backtrace for when the transformation itself fails."""
        rule = easyparse.TransformationRule(
            easyparse.PatternRule('a'),
            int)
        ok, backtrace = rule.parse('a')
        self.assertFalse(ok)
        backtrace_string = self._print_backtrace_to_string(backtrace)
        exp_backtrace_string = \
            "<Unnamed>: Transformation: Failed to transform 'a'\n"
        self.assertEqual(exp_backtrace_string, backtrace_string)

    def test_print_backtrace_named_transformation(self):
        g = easyparse.Grammar()
        g.num = easyparse.PatternRule('a') > int
        ok, backtrace = g.num.parse('a')
        self.assertFalse(ok)
        backtrace_string = self._print_backtrace_to_string(backtrace)
        exp_backtrace_string = \
            "num: Transformation: Failed to transform 'a'\n"
        self.assertEqual(exp_backtrace_string, backtrace_string)

    def test_print_backtrace_named_rule_with_transformation(self):
        """Test the failure of a rule with a transformation."""
        g = easyparse.Grammar()
        g.term = g.num + easyparse.ignore('+') + g.term | easyparse.eof > \
                 sum
        g.num = easyparse.parse(re='[0-9]+') > int
        ok, backtrace = g.term.parse('1+q')
        self.assertFalse(ok)
        backtrace_string = self._print_backtrace_to_string(backtrace)
        exp_backtrace_string = '\n'.join([
            "term: Disjunction: Failed",
            " <Unnamed>: Conjunction: Failed",
            "  num: RegularExpression: Matched '1'",
            "  <Unnamed>: Pattern: Matched '[]'",  # [] means Ignored
            "  term: Disjunction: Failed",
            "   <Unnamed>: Conjunction: Failed",
            "    num: RegularExpression: Failed to match '[0-9]+'",
            "   <Unnamed>: Eof: Failed",
            " <Unnamed>: Eof: Failed\n"])
        self.assertEqual(exp_backtrace_string, backtrace_string)

class TestParser(unittest.TestCase):

    def test_create_virtual_rule_in_parser(self):
        parser = easyparse.Grammar()
        parser.number = parser.digit + (parser.number | easyparse.ignore(''))
        parser.digit = easyparse.parse('0') | easyparse.parse('1')
        exp_value = (True, ['0', '1', '0', '1'])
        act_value = parser.number.parse('0101')
        self.assertEqual(exp_value, act_value)

class TestVirtualRule(unittest.TestCase):

    def test_create_virtual_rule(self):
        exp_result = (True, 'q')
        class Mock(easyparse.Grammar):
            class Rule:
                def _parse(self, iterator, *args, **kwargs):
                    return exp_result
            def __init__(self):
                object.__setattr__(self, '_real_rules',
                                   {'rule': Mock.Rule()})
        mock_parent = Mock()
        rule = easyparse.VirtualRule(mock_parent, 'rule')
        act_result = rule.parse('')
        self.assertEqual(exp_result, act_result)

class TestParseRule(unittest.TestCase):

    def test_match_string(self):
        rule = easyparse.PatternRule('0')
        self.assertEqual((True, '0'), rule.parse('0'))
        self.assertFalse(rule.parse('1')[0])

    def test_match_shorter_than_pattern(self):
        rule = easyparse.PatternRule('000')
        self.assertFalse(rule.parse('0')[0])

    def test_match_ignores_preceding_whitespace(self):
        rule = easyparse.PatternRule('0')
        self.assertEqual((True, '0'), rule.parse(' 0', allow_whitespace=True))

    def test_match_ignores_preceding_whitespace_but_matches_pattern_space(self):
        rule = easyparse.PatternRule(' ')
        self.assertEqual((True, ' '), rule.parse('\t ', allow_whitespace=True))

    def test_whitespace_matches_itself(self):
        rule = easyparse.PatternRule('\t')
        itr = '\t00'
        self.assertEqual((True, '\t'), rule.parse(itr))
    
    def test_match_with_significant_whitespace(self):
        rule = easyparse.PatternRule('0')
        self.assertEqual(False,
                         rule.parse(' 0')[0])

    def test_match_with_injected_whitespace(self):
        rule = easyparse.PatternRule('01')
        self.assertEqual(False, rule.parse('0 1')[0])

    def test_match_with_same_whitespace_preceding(self):
        rule = easyparse.PatternRule(' 0')
        self.assertEqual((True, ' 0'), rule.parse('  0',
                                                  allow_whitespace=True))

    def test_match_wrong_rule_with_whitespace(self):
        rule = easyparse.PatternRule('0')
        self.assertEqual(False, rule.parse(' 1')[0])

    def test_match_conjunction_rule(self):
        rule = easyparse.PatternRule('0') + easyparse.PatternRule('1')
        self.assertEqual((True, ['0', '1']),
                         rule.parse('01'))
        self.assertFalse(rule.parse('00')[0])

    def test_match_multiple_conjunction_rules(self):
        rule = easyparse.PatternRule('0') + easyparse.PatternRule('1') +\
               easyparse.PatternRule('2')
        self.assertEqual((True, ['0', '1', '2']),
                         rule.parse('012'))
        self.assertFalse(rule.parse('0012')[0])

    def test_match_disjunction_rule(self):
        rule = easyparse.PatternRule('0') | easyparse.PatternRule('1')
        self.assertEqual((True, '0'),
                         rule.parse('0'))
        self.assertEqual((True, '1'),
                         rule.parse('1'))
        self.assertFalse(rule.parse('2')[0])

    def test_match_conjunctions_and_disjunctions(self):
        rule = easyparse.PatternRule('0') + easyparse.PatternRule('1') | \
               easyparse.PatternRule('1')
        self.assertEqual((True, ['0', '1']),
                         rule.parse('01'))
        self.assertEqual((True, '1'),
                         rule.parse('1'))
        self.assertFalse(rule.parse('00')[0])

    def test_transformation_function(self):
        rule = easyparse.PatternRule('0') > int
        self.assertEqual((True, 0), rule.parse('0'))
        self.assertNotEqual((True, '0'), rule.parse('0'))

    def test_many_disjunctions_form_one_object(self):
        rule = easyparse.PatternRule('0') | \
               easyparse.PatternRule('1') | \
               easyparse.PatternRule('2') | \
               easyparse.PatternRule('3')
        self.assertEqual(4, len(rule.rules))
        self.assertEqual((True, '2'), rule.parse('2'))

    def test_many_disjunctions_are_right_branching(self):
        rule = easyparse.PatternRule('0') | \
               (easyparse.PatternRule('1') |
                easyparse.PatternRule('2'))
        self.assertEqual(3, len(rule.rules))
        self.assertEqual((True, '1'), rule.parse('1'))

    def test_many_conjunctions_form_one_object(self):
        rule = easyparse.PatternRule('0') + \
               easyparse.PatternRule('1') + \
               easyparse.PatternRule('2') + \
               easyparse.PatternRule('3')
        self.assertEqual(4, len(rule.rules))
        self.assertEqual((True, ['0','1','2','3']), rule.parse('0123'))

    def test_eof_rule(self):
        rule = easyparse.EofRule()
        self.assertEqual((True, easyparse.Ignored), rule.parse(''))

    def test_fail_eof_rule_with_more_data(self):
        rule = easyparse.EofRule()
        self.assertEqual(False, rule.parse('abc')[0])

    def test_eof_with_preceding_whitespace(self):
        rule = easyparse.parse('a') + easyparse.EofRule()
        self.assertEqual((True, ['a']), rule.parse('a '))

    def test_eof_ignoring_preceding_whitespace(self):
        rule = easyparse.parse('a') ^ easyparse.EofRule()
        self.assertEqual(False, rule.parse('a ')[0])

    def test_conjunction_not_ignoring_whitespace(self):
        rule = easyparse.PatternRule('a') ^ \
               easyparse.PatternRule('b')
        self.assertEqual((True, ['a','b']),
                         rule.parse('ab'))
        self.assertEqual(False, rule.parse('a b')[0])

    def test_two_conjunctions_not_ignoring_whitespace(self):
        # Use parens to test different groupings
        rule0 = easyparse.PatternRule('a') ^ (
            easyparse.PatternRule('b') ^ easyparse.PatternRule('c'))
        rule1 = (easyparse.PatternRule('a') ^ 
            easyparse.PatternRule('b')) ^ easyparse.PatternRule('c')
        self.assertEqual((True, ['a', 'b', 'c']), rule0.parse('abc'))
        self.assertEqual((True, ['a', 'b', 'c']), rule1.parse('abc'))
        for bad_pattern in ['a bc', 'ab c', 'a b c']:
            self.assertEqual(False, rule0.parse(bad_pattern)[0])
            self.assertEqual(False, rule1.parse(bad_pattern)[0])

    def test_two_conjunctions_second_ignoring_whitespace(self):
        rule0 = easyparse.PatternRule('a') + (
            easyparse.PatternRule('b') ^ easyparse.PatternRule('c'))
        rule1 = (easyparse.PatternRule('a') + 
            easyparse.PatternRule('b')) ^ easyparse.PatternRule('c')
        for good_pattern in ['abc', 'a bc']:
            self.assertEqual((True, ['a', 'b', 'c']),
                             rule0.parse(good_pattern), msg=good_pattern)
            self.assertEqual((True, ['a', 'b', 'c']),
                             rule1.parse(good_pattern), msg=good_pattern)
        for bad_pattern in ['ab c', 'a b c']:
            self.assertEqual(False, rule0.parse(bad_pattern)[0])
            self.assertEqual(False, rule1.parse(bad_pattern)[0])

    def test_two_conjunctions_first_ignoring_whitespace(self):
        rule0 = easyparse.PatternRule('a') ^ (
            easyparse.PatternRule('b') + easyparse.PatternRule('c'))
        rule1 = (easyparse.PatternRule('a') ^ 
            easyparse.PatternRule('b')) + easyparse.PatternRule('c')
        for good_pattern in ['abc', 'ab c']:
            self.assertEqual((True, ['a', 'b', 'c']),
                             rule0.parse(good_pattern), msg=good_pattern)
            self.assertEqual((True, ['a', 'b', 'c']),
                             rule1.parse(good_pattern), msg=good_pattern)
        for bad_pattern in ['a bc', 'a b c']:
            self.assertEqual(False, rule0.parse(bad_pattern)[0])
            self.assertEqual(False, rule1.parse(bad_pattern)[0])

class TestHelperFunctions(unittest.TestCase):

    def test_parse_delegates_to_pattern_rule(self):
        rule = easyparse.parse('010')
        self.assertEqual((True, '010'),
                         rule.parse('010'))
        self.assertFalse(rule.parse('01')[0])

    def test_ignore_returns_none(self):
        rule = easyparse.ignore('010')
        self.assertEqual((True, easyparse.Ignored),
                         rule.parse('010'))
        self.assertFalse(rule.parse('01')[0])

    def test_ignore_fails_on_preceding_whitespace(self):
        rule = easyparse.ignore('1')
        self.assertEqual(False, rule.parse(' 1')[0])

    def test_ignore_re_fails_on_preceding_whitespace(self):
        rule = easyparse.ignore(re='[0-9]')
        self.assertEqual(False,
                         rule.parse(' 5')[0])

    def test_ignore_in_conjunction_is_ignored(self):
        rule = easyparse.ignore('0') + easyparse.parse('1')
        self.assertEqual((True, ['1']), rule.parse('01'))
        self.assertFalse(rule.parse('10')[0])

    def test_parse_re(self):
        rule = easyparse.parse(re=r'[0-9]')
        self.assertEqual((True, '8'), rule.parse('8'))
        self.assertFalse(rule.parse('a')[0])

    def test_ignore_re(self):
        rule = easyparse.ignore(re=r'[0-9]')
        self.assertEqual((True, easyparse.Ignored), rule.parse('8'))
        self.assertFalse(rule.parse('a')[0])

    def test_rule_decorator(self):
        @easyparse.parse_rule
        def mock_rule(r):
            return r + r
        rule0 = easyparse.PatternRule('9')
        rule1 = mock_rule(rule0)
        self.assertEqual((True, ['9','9']), rule1.parse('99'))

    def test_null(self):
        rule = easyparse.null
        self.assertEqual((True, easyparse.Ignored), rule.parse(''))
        self.assertEqual((True, easyparse.Ignored), rule.parse('abc'))

    def test_optional(self):
        rule = easyparse.optional(easyparse.PatternRule('0'))
        self.assertEqual((True, '0'), rule.parse('0'))
        self.assertEqual((True, easyparse.Ignored), rule.parse(''))

    def test_many(self):
        rule = easyparse.many(easyparse.PatternRule('a'))
        self.assertEqual((True, easyparse.Ignored), rule.parse(''))
        for i in range(1,5):
            self.assertEqual((True, ['a'] * i), rule.parse('a' * i))

    def test_join(self):
        rule = easyparse.join(easyparse.PatternRule('a'),
                              easyparse.PatternRule('q',
                                                    ignore_output=True))
        self.assertEqual((True, easyparse.Ignored), rule.parse(''))
        self.assertEqual((True, ['a']), rule.parse('a'))
        self.assertEqual((True, ['a', 'a']), rule.parse('aqa'))
        self.assertEqual((True, ['a']), rule.parse('aa'))

    def test_at_most_n(self):
        rule = easyparse.at_most_n(easyparse.PatternRule('ab'), 2) + easyparse.PatternRule('c')
        self.assertEqual((True, ['c']), rule.parse('c'))
        self.assertEqual((True, ['ab','c']), rule.parse('abc'))
        self.assertEqual((True, ['ab','ab','c']), rule.parse('ababc'))
        self.assertEqual(False, rule.parse('abababc')[0])

    def test_at_least_n(self):
        rule = easyparse.at_least_n(easyparse.PatternRule('ab'), 2) + easyparse.PatternRule('c')
        self.assertEqual((True, ['ab', 'ab', 'c']), rule.parse('ababc'))
        self.assertEqual((True, ['ab', 'ab', 'ab', 'c']),
                         rule.parse('abababc'))
        self.assertEqual(False, rule.parse('c')[0])
        self.assertEqual(False, rule.parse('abc')[0])

class TestRegularExpression(unittest.TestCase):

    def test_match_regular_expression_rule(self):
        rule = easyparse.RegularExpressionRule(r'[0-9]')
        self.assertEqual((True, '7'), rule.parse('7'))

    def test_ignore_regular_expression_rule(self):
        rule = easyparse.RegularExpressionRule(r'[0-9]', ignore_output=True)
        self.assertEqual((True, easyparse.Ignored), rule.parse('7'))

    def test_match_two_re_rules(self):
        rule0 = easyparse.RegularExpressionRule(r'[0-9]')
        rule1 = easyparse.RegularExpressionRule(r'[a-z]')
        buf = easyparse.BufferedIterator('a7')
        self.assertEqual((True, 'a'), rule1.parse(buf))
        self.assertEqual((True, '7'), rule0.parse(buf))

    def test_match_re_rules_with_failures(self):
        rule0 = easyparse.RegularExpressionRule(r'[a-z]')
        rule1 = easyparse.RegularExpressionRule(r'[0-9]')
        buf = easyparse.BufferedIterator('7a')
        buf.checkpoint()
        self.assertEqual(False, rule0.parse(buf)[0])
        buf.rewind()
        self.assertEqual((True, '7'), rule1.parse(buf))

    def test_re_match_with_same_whitespace_preceding(self):
        rule = easyparse.RegularExpressionRule(r' 0')
        self.assertEqual((True, ' 0'), rule.parse('  0',
                                                  allow_whitespace=True))

class TestBufferedIterator(unittest.TestCase):

    def test_play_buffer(self):
        itr = easyparse.BufferedIterator([1,2,3])
        act = [x for x in itr]
        exp = [1,2,3]
        self.assertEqual(act, exp)

    def test_checkpoint_rewind_buffer(self):
        values = [1,2,3,4,5]
        itr = easyparse.BufferedIterator(values)
        itr.checkpoint()
        self.assertEqual(values[0], next(itr))
        itr.rewind()
        self.assertEqual([x for x in itr], values)

    def test_multiple_checkpoint_buffer(self):
        values = [1,2,3,4,5,6]
        itr = easyparse.BufferedIterator(values)
        itr.checkpoint()
        for i in range(2):
            self.assertEqual(values[i], next(itr))
        itr.checkpoint()
        for i in range(2):
            self.assertEqual(values[2 + i], next(itr))
        itr.rewind()
        self.assertEqual(values[2], next(itr))
        itr.rewind()
        self.assertEqual([x for x in itr], values)

    def test_stop_iteration_then_rewind(self):
        values = [1,2,3]
        itr = easyparse.BufferedIterator(values)
        itr.checkpoint()
        for i in range(3):
            self.assertEqual(values[i], next(itr))
        self.assertRaises(StopIteration, lambda : next(itr))
        itr.rewind()
        self.assertEqual([x for x in itr], values)

    def test_commit(self):
        values = [1,2,3]
        itr = easyparse.BufferedIterator(values)
        itr.checkpoint()
        next(itr)
        itr.checkpoint()
        next(itr)
        itr.commit()
        self.assertEqual(3, next(itr))
        itr.rewind()
        self.assertEqual([1, 2, 3], [x for x in itr])

if __name__ == '__main__':
    unittest.main()
