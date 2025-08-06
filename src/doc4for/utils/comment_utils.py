import re
import html
from typing import List
from fparser.two.Fortran2003 import Comment

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
        if comment.item.comment.startswith('!!*') and comment_stack[-1].item.comment.rstrip().endswith('*!'):
            return True
    return False

#TODO
#        !!* First block with scalars.
#        !!* Second block with array 
#        !!* Third block with single variable 
#        !*!
# should be parsed as a single comment - need to continue until we 
# hit the closing statement
def format_comments(comment_stack: List[Comment]) -> str:
    formatted_comments = []
    is_inside_doc4for_comment = False
    for comment in comment_stack:
        if comment.item.comment:
            if comment.item.comment.startswith("C     "):
                # don't handle old-style comments
                continue
            # get rid of the initial ! but keep everything after including !
            content = comment.item.comment.split("!", 1)[1].strip() 
            if "!*" in content:            
                # clear any previous comments as we just need the doc4for immediately before
                # a Fortran statement
                formatted_comments.clear()
                line = content.split("!*")[1] # get everything after the first occurrence
                if "*!" in line:
                    # single-line comment e.g. !!* Single line comment *!
                    is_inside_doc4for_comment = False
                    line = line.split("*!")[0]
                else:
                    is_inside_doc4for_comment = True
                formatted_comments.append(line.strip())
            elif "*!" in content:
                is_inside_doc4for_comment = False            
                formatted_comments.append(content.split("*!")[0].strip())
            elif is_inside_doc4for_comment:
                formatted_comments.append(content.strip())
    return "\n".join([format_comment_for_html(comment) for comment in formatted_comments if comment]) + "\n" if formatted_comments else ""


def is_end_of_doc4for_comment(comment: Comment) -> bool:
    return "*!" in comment.item.comment

#TODO remove this
def get_formatted_description(comment_stack: List[Comment]) -> str:
   """Get a formatted description from the comment stack.

   Args:
       comment_stack: The stack of comments to format.

   Returns:
       The formatted description or an empty string if not a doc4for comment.
   """
   return format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""
