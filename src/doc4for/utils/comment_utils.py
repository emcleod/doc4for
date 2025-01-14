import re
import html
from typing import List
from fparser.one.block_statements import Comment

def format_comment_for_html(comment: str) -> str:
    """Process a comment string to escape HTML entities and replace code blocks.

    Args:
        comment (str): The input comment string to be processed.

    Returns:
        str: The processed comment string with HTML entities escaped and code blocks
            wrapped in `<code>` tags.

    Examples:
        >>> process_comment("This is a comment with <code>print('Hello, World!')</code>")
        'This is a comment with &lt;code&gt;print(&#39;Hello, World!&#39;)&lt;/code&gt;'

        >>> process_comment("This is a {code} block with {nested {blocks}}")
        'This is a <code>code</code> block with <code>nested {blocks}</code>'
    """
    return re.sub(r'{(.*?)}', r'<code>\1</code>', html.escape(comment))

def is_doc4for_comment(comment_stack: List[Comment]) -> bool:
    if not comment_stack:
        return False
    for comment in comment_stack:
        if comment.content.startswith('!*') and comment_stack[-1].content.rstrip().endswith('*!'):
            return True
    return False

def format_comments(comment_stack: List[Comment]) -> str:
    formatted_comments = []
    for comment in comment_stack:
        content = comment.content.strip()
        if content.startswith('!*'):
            content = content[2:].strip()
        elif content.startswith('!'):
            content = content[1:].strip()
        if content.endswith('*!'):
            content = content[:-2].rstrip()
        formatted_comments.append(format_comment_for_html(content))
    return '\n'.join(formatted_comments) + '\n'


def is_end_of_doc4for_comment(comment: Comment) -> bool:
    return "*!" in comment.content
