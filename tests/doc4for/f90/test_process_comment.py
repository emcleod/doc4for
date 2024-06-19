import unittest
from doc4for.f90.generate_module_tree import process_comment

class TestProcessComment(unittest.TestCase):
    def test_with_code(self):
        input_comment = 'This is a comment with some {code} in it.'
        expected_output = 'This is a comment with some <code>code</code> in it.'
        assert process_comment(input_comment) == expected_output

    def test_with_multiple_codes(self):
        input_comment = 'This is a {code1} and {code2} comment.'
        expected_output = 'This is a <code>code1</code> and <code>code2</code> comment.'
        assert process_comment(input_comment) == expected_output

    def test_with_no_codes(self):
        input_comment = 'This comment has no code blocks.'
        expected_output = 'This comment has no code blocks.'
        assert process_comment(input_comment) == expected_output

    def test_with_html_entities(self):
        input_comment = 'This is a comment with {<code>} in it.'
        expected_output = 'This is a comment with <code>&lt;code&gt;</code> in it.'
        assert process_comment(input_comment) == expected_output

if __name__ == '__main__':
    unittest.main()