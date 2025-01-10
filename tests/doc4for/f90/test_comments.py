import unittest
from fparser.one.block_statements import Comment
from unittest import TestCase
from unittest.mock import Mock
from doc4for.f90.generate_file_tree import is_doc4for_comment, format_comments
from doc4for.utils.comment_utils import format_comment_for_html

class TestComments(TestCase):

    def test_comment_wrong_start(self):
        item1 = Mock(comment='! Start of comment\n')
        item2 = Mock(comment='! End of comment *!\n')
        comments = [
            Comment(None, item1),
            Comment(None, item2)
        ]
        self.assertFalse(is_doc4for_comment(comments))

    def test_comment_wrong_end(self):
        item1 = Mock(comment='!!* Start of comment\n')
        item2 = Mock(comment='! End of comment *\n')
        comments = [
            Comment(None, item1),
            Comment(None, item2)
        ]
        self.assertFalse(is_doc4for_comment(comments))

    def test_single_line_comment(self):
        item1 = Mock(comment='!!* One line comment *!\n')
        comments = [
            Comment(None, item1),
        ]
        self.assertTrue(is_doc4for_comment(comments))

    def test_multi_line_comment(self):
        item1 = Mock(comment='!!*\n')
        item2 = Mock(comment='!First comment\n')
        item3 = Mock(comment='!Second comment\n')
        item4 = Mock(comment='!Third comment\n')
        item5 = Mock(comment='!*!\n')
        comments = [
            Comment(None, item1),
            Comment(None, item2),
            Comment(None, item3),
            Comment(None, item4),
            Comment(None, item5),
        ]
        self.assertTrue(is_doc4for_comment(comments))

    def test_empty_comment_stack(self):
        self.assertEqual(format_comments([]), '\n')

    def test_single_comment(self):
        item1 = Mock(comment='!!*\n')
        item2 = Mock(comment='!Content\n')
        item3 = Mock(comment='!*!\n')
        comments = [
            Comment(None, item1),
            Comment(None, item2),
            Comment(None, item3)
        ]
        self.assertEqual(format_comments(comments), '\nContent\n\n')

    def test_multiple_comments(self):
        item1 = Mock(comment='!!* Start\n')
        item2 = Mock(comment='! Line 1\n')
        item3 = Mock(comment='! Line 2\n')
        item4 = Mock(comment='! Line 3\n')
        item5 = Mock(comment='! End *!\n')
        comments = [
            Comment(None, item1),
            Comment(None, item2),
            Comment(None, item3),
            Comment(None, item4),
            Comment(None, item5)
        ]
        expected = 'Start\nLine 1\nLine 2\nLine 3\nEnd\n'
        self.assertEqual(format_comments(comments), expected)

    def test_comments_with_empty_lines(self):
        item1 = Mock(comment='!!* Start\n')
        item2 = Mock(comment='! Line 1\n')
        item3 = Mock(comment='! \n')
        item4 = Mock(comment='! \n')
        item5 = Mock(comment='! End *!\n')
        comments = [
            Comment(None, item1),
            Comment(None, item2),
            Comment(None, item3),
            Comment(None, item4),
            Comment(None, item5)
        ]
        expected = 'Start\nLine 1\n\n\nEnd\n'
        self.assertEqual(format_comments(comments), expected)

    def test_all_empty_comments(self):
        item1 = Mock(comment='!!*\n')
        item2 = Mock(comment='!\n')
        item3 = Mock(comment='!\n')
        item4 = Mock(comment='!\n')
        item5 = Mock(comment='!*!\n')
        comments = [
            Comment(None, item1),
            Comment(None, item2),
            Comment(None, item3),
            Comment(None, item4),
            Comment(None, item5)
        ]
        self.assertEqual(format_comments(comments), '\n\n\n\n\n')

    def test_comments_with_html_formatting(self):
        def mock_format_comment_for_html(content):
            return f'<p>{content}</p>' if content else ''

        global format_comment_for_html
        original_format = format_comment_for_html
        format_comment_for_html = mock_format_comment_for_html

        try:
            item1 = Mock(comment='!!*\n')
            item2 = Mock(comment='! <pre>\n')
            item3 = Mock(comment='! Formatted\n')
            item4 = Mock(comment='! </pre>\n')
            item5 = Mock(comment='!*!\n')
            comments = [
                Comment(None, item1),
                Comment(None, item2),
                Comment(None, item3),
                Comment(None, item4),
                Comment(None, item5)
            ]
            expected = '\n&lt;pre&gt;\nFormatted\n&lt;/pre&gt;\n\n'
            self.assertEqual(format_comments(comments), expected)
        finally:
            format_comment_for_html = original_format

if __name__ == '__main__':
    unittest.main()