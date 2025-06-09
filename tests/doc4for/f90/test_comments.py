import unittest
from fparser.two.Fortran2003 import Comment
from unittest import TestCase
from unittest.mock import Mock
from doc4for.utils.comment_utils import format_comment_for_html, is_doc4for_comment, format_comments

class TestComments(TestCase):

    def test_comment_wrong_start(self):
        # Create mocks with the correct structure
        comment1 = Mock(spec=Comment)
        comment1.item = Mock()
        comment1.item.comment = "! Start of comment\n"
        
        comment2 = Mock(spec=Comment)
        comment2.item = Mock()
        comment2.item.comment = "! End of comment *!\n"
        
        comments = [comment1, comment2]
        self.assertFalse(is_doc4for_comment(comments))

    def test_comment_wrong_end(self):
        comment1 = Mock(spec=Comment)
        comment1.item = Mock()
        comment1.item.comment = "!!* Start of comment\n"
        
        comment2 = Mock(spec=Comment)
        comment2.item = Mock()
        comment2.item.comment = "! End of comment *\n"
        
        comments = [comment1, comment2]
        self.assertFalse(is_doc4for_comment(comments))

    def test_single_line_comment(self):
        comment1 = Mock(spec=Comment)
        comment1.item = Mock()
        comment1.item.comment = "!!* One line comment *!\n"
        
        comments = [comment1]
        self.assertTrue(is_doc4for_comment(comments))

    def test_multi_line_comment(self):
        comment1 = Mock(spec=Comment)
        comment1.item = Mock()
        comment1.item.comment = "!!*\n"
        
        comment2 = Mock(spec=Comment)
        comment2.item = Mock()
        comment2.item.comment = "!First comment\n"
        
        comment3 = Mock(spec=Comment)
        comment3.item = Mock()
        comment3.item.comment = "!Second comment\n"
        
        comment4 = Mock(spec=Comment)
        comment4.item = Mock()
        comment4.item.comment = "!Third comment\n"
        
        comment5 = Mock(spec=Comment)
        comment5.item = Mock()
        comment5.item.comment = "!*!\n"
        
        comments = [comment1, comment2, comment3, comment4, comment5]
        self.assertTrue(is_doc4for_comment(comments))

    def test_empty_comment_stack(self):
        self.assertEqual(format_comments([]), "")

    def test_single_comment(self):
        comment1 = Mock(spec=Comment)
        comment1.item = Mock()
        comment1.item.comment = "!!*\n"
        
        comment2 = Mock(spec=Comment)
        comment2.item = Mock()
        comment2.item.comment = "!Content\n"
        
        comment3 = Mock(spec=Comment)
        comment3.item = Mock()
        comment3.item.comment = "!*!\n"
        
        comments = [comment1, comment2, comment3]
        self.assertEqual(format_comments(comments), "Content\n")

    def test_multiple_comments(self):
        comment1 = Mock(spec=Comment)
        comment1.item = Mock()
        comment1.item.comment = "!!* Start\n"
        
        comment2 = Mock(spec=Comment)
        comment2.item = Mock()
        comment2.item.comment = "! Line 1\n"
        
        comment3 = Mock(spec=Comment)
        comment3.item = Mock()
        comment3.item.comment = "! Line 2\n"
        
        comment4 = Mock(spec=Comment)
        comment4.item = Mock()
        comment4.item.comment = "! Line 3\n"
        
        comment5 = Mock(spec=Comment)
        comment5.item = Mock()
        comment5.item.comment = "! End *!\n"
        
        comments = [comment1, comment2, comment3, comment4, comment5]
        expected = "Start\nLine 1\nLine 2\nLine 3\nEnd\n"
        self.assertEqual(format_comments(comments), expected)

    def test_comments_with_empty_lines(self):
        comment1 = Mock(spec=Comment)
        comment1.item = Mock()
        comment1.item.comment = "!!* Start\n"
        
        comment2 = Mock(spec=Comment)
        comment2.item = Mock()
        comment2.item.comment = "! Line 1\n"
        
        comment3 = Mock(spec=Comment)
        comment3.item = Mock()
        comment3.item.comment = "! \n"
        
        comment4 = Mock(spec=Comment)
        comment4.item = Mock()
        comment4.item.comment = "! \n"
        
        comment5 = Mock(spec=Comment)
        comment5.item = Mock()
        comment5.item.comment = "! End *!\n"
        
        comments = [comment1, comment2, comment3, comment4, comment5]
        expected = "Start\nLine 1\nEnd\n"
        self.assertEqual(format_comments(comments), expected)

    def test_all_empty_comments(self):
        comment1 = Mock(spec=Comment)
        comment1.item = Mock()
        comment1.item.comment = "!!*\n"
        
        comment2 = Mock(spec=Comment)
        comment2.item = Mock()
        comment2.item.comment = "!\n"
        
        comment3 = Mock(spec=Comment)
        comment3.item = Mock()
        comment3.item.comment = "!\n"
        
        comment4 = Mock(spec=Comment)
        comment4.item = Mock()
        comment4.item.comment = "!\n"
        
        comment5 = Mock(spec=Comment)
        comment5.item = Mock()
        comment5.item.comment = "!*!\n"
        
        comments = [comment1, comment2, comment3, comment4, comment5]
        self.assertEqual(format_comments(comments), "\n")

    def test_comments_with_html_formatting(self):
        def mock_format_comment_for_html(content):
            return f"<p>{content}</p>" if content else ""

        global format_comment_for_html
        original_format = format_comment_for_html
        format_comment_for_html = mock_format_comment_for_html

        try:
            comment1 = Mock(spec=Comment)
            comment1.item = Mock()
            comment1.item.comment = "!!*\n"
            
            comment2 = Mock(spec=Comment)
            comment2.item = Mock()
            comment2.item.comment = "! <pre>\n"
            
            comment3 = Mock(spec=Comment)
            comment3.item = Mock()
            comment3.item.comment = "! Formatted\n"
            
            comment4 = Mock(spec=Comment)
            comment4.item = Mock()
            comment4.item.comment = "! </pre>\n"
            
            comment5 = Mock(spec=Comment)
            comment5.item = Mock()
            comment5.item.comment = "!*!\n"
            
            comments = [comment1, comment2, comment3, comment4, comment5]
            expected = "&lt;pre&gt;\nFormatted\n&lt;/pre&gt;\n"
            self.assertEqual(format_comments(comments), expected)
        finally:
            format_comment_for_html = original_format

if __name__ == "__main__":
    unittest.main()

