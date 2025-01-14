import re
from typing import List
from fparser.one.typedecl_statements import TypeDeclarationStatement
from fparser.one.block_statements import Comment
from doc4for.models.variable_models import ParameterDescription
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments

def parse_parameter(
    declaration: TypeDeclarationStatement, comment_stack: List[Comment]
) -> ParameterDescription:
    item_str = declaration.item.line
    name_match = re.search(r"::\s*(\w+)\s*=", item_str)
    if name_match:
        name = name_match.group(1)
        value = item_str.split("=", 1)[1].strip()
    else:
        # Handle cases where the parameter declaration format is not recognized
        name = ""
        value = ""
    length = None
    # find the length of a character
    if declaration.name.lower() == "character":
        # Look for a string value between single quotes
        string_match = re.search(r"'([^']*)'", value)
        if string_match:
            length = str(len(string_match.group(1)))    
    
    parameter_description: ParameterDescription = {
        "description": "",
        "type": str(declaration.name),
        "name": name.strip(),
        "value": value.strip(),
        "dimension": "",
        "attributes": [],
        "length": length
    }
    if is_doc4for_comment(comment_stack):
        parameter_description["description"] = format_comments(comment_stack)
    return parameter_description


def is_parameter(declaration: TypeDeclarationStatement) -> bool:
    """Determines if a TypeDeclarationStatement is a parameter declaration.

    Args:
        declaration: The TypeDeclarationStatement to check

    Returns:
        bool: True if the declaration has the parameter attribute
    """
    # Check if 'parameter' is in the attributes
    return (
        any(attr.lower() == "parameter" for attr in declaration.attrspec)
        if declaration.attrspec
        else False
    )
