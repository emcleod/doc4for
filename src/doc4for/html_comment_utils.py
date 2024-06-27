import re
import html

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
