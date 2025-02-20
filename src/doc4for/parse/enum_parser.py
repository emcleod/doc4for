from typing import List, Dict, Tuple
from fparser.one.block_statements import (
    Comment,
    Enum,
    Enumerator
)
from doc4for.models.common import EnumDescription, EnumeratorDescription
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments


def parse_enum(enum: Enum, comment_stack: List[Comment]) -> Tuple[str, EnumDescription]:
    description = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""
    enumerators: Dict[str, EnumeratorDescription] = {}
    first_enumerator_name = None
    
    enum_comment_stack = []
    next_implicit_value = 0
    
    for content in enum.content:
        if isinstance(content, Comment) and content.content:
            enum_comment_stack.append(content)
        
        if isinstance(content, Enumerator):
            enum_description = format_comments(enum_comment_stack) if is_doc4for_comment(enum_comment_stack) else ""
            enum_comment_stack.clear()
            
            # Get the original line text to preserve case
            original_line = content.item.line
            
            for item in content.items:
                if '=' in item:
                    parts = item.split('=', 1)
                    lowercase_name = parts[0].strip()
                    lowercase_value = parts[1].strip()
                    
                    # Find the original name and value in the line text
                    if '::' in original_line:
                        decl_text = original_line.split('::', 1)[1].strip()
                        for word in decl_text.split(','):
                            if '=' in word:
                                orig_parts = word.split('=', 1)
                                orig_name = orig_parts[0].strip()
                                if orig_name.lower() == lowercase_name:
                                    name = orig_name
                                    value = orig_parts[1].strip()  # Use original case for value
                                    break
                        else:
                            name = lowercase_name
                            value = lowercase_value
                    else:
                        name = lowercase_name
                        value = lowercase_value
                else:
                    name = item.strip()
                    value = str(next_implicit_value)
                    # Extract the original case for the name when no value is specified
                    if '::' in original_line:
                        decl_text = original_line.split('::', 1)[1].strip()
                        for word in decl_text.split(','):
                            if '=' not in word and word.strip().lower() == name.lower():
                                name = word.strip()
                                break
                    next_implicit_value += 1
                
                if first_enumerator_name is None:
                    first_enumerator_name = name
                
                item_description: EnumeratorDescription = {
                    "name": name,
                    "value": value,
                    "description": enum_description
                }
                enumerators[name] = item_description
    
    enum_description = {
        "name": "__ENUM__" if enum.name is None else enum.name,
        "description": description,
        "attributes": [],
        "enumerators": enumerators,
        "binding_type": None
    }
    return first_enumerator_name, enum_description