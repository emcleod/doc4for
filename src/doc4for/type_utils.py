from typing import List, Dict, Any, Tuple, Optional, Callable, Union
from fparser.one.block_statements import (
    Comment,
    Type,
    SpecificBinding,
    Contains
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.data_models import (
    TypeDescription
)
from doc4for.comment_utils import format_comment_for_html

def update_type_with_parsed_data(type: Any, type_info: TypeDescription) -> None:
    type_info['type_name'] = type.name
    in_contains: bool = False
    for item in type.content:
        if isinstance(item, SpecificBinding):
            if in_contains:
                print (item)
            else:
                type_info['attributes'] = [attr.lower() for attr in item.attrs]
        elif isinstance(item, Contains):
            in_contains = True
