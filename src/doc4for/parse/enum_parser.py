from typing import List, Dict, Tuple, Optional
from fparser.two.Fortran2003 import (
  Enum_Def, 
  Enum_Def_Stmt,
  Enumerator,
  Enumerator_List, # type: ignore[attr-defined]
  Enumerator_Def_Stmt, 
  Comment,
  Name)
from fparser.two.utils import walk
from doc4for.models.common import EnumDescription, EnumeratorDescription, BindingTypeEnum, BindingType
from doc4for.utils.comment_utils import get_formatted_description

def parse_enum(enum: Enum_Def, comment_stack: List[Comment], default_access: Optional[str]) -> Tuple[Optional[str], EnumDescription]:
    description = get_formatted_description(comment_stack)
    enumerators: Dict[str, EnumeratorDescription] = {}
    first_enum_name: Optional[str] = None # for the entry in the module description
    binding_type: Optional[BindingType] = None
    enum_name = "__ENUM__"

    implicit_value = 0
    enum_comment_stack = []
    for node in enum.children:
        if isinstance(node, Comment):
            enum_comment_stack.append(node)
        elif isinstance(node, Enum_Def_Stmt):
            if "BIND(C)" in node.children[0]:
                #TODO can have a kind in F2018, but it's not supported in fparser yet
                binding_type = {"type": BindingTypeEnum.BIND_C, "name": None}
        elif isinstance(node, Enumerator_Def_Stmt):
            enum_desc = get_formatted_description(enum_comment_stack)
            enum_comment_stack.clear()
            enum_list = walk(node, Enumerator_List)[0].children
            for decl in enum_list:
                name: str = ""
                value: str = ""
                if isinstance(decl, Enumerator):
                    # has a value
                    name = decl.children[0].string
                    value = str(decl.children[2])  # More robust for expressions
                    # str() will leave a space after the negative sign 
                    if value.startswith('- '):
                        value = '-' + value[2:].strip()
                    else:
                        value = value.strip()                    
                    # Try to track implicit value for simple numeric cases
                    if value.isdigit() or (value.startswith('-') and value[1:].isdigit()):
                        implicit_value = int(value) + 1
                    else:
                        # For expressions, just increment by 1 from current
                        implicit_value += 1     
                elif isinstance(decl, Name):                
                    name = decl.string
                    value = str(implicit_value)
                    implicit_value += 1  # Only increment when actually using implicit value                
                if not first_enum_name:
                    first_enum_name = name
                enumerators[name] = {
                    "name": name,
                    "value": value,
                    "description": enum_desc
                }

    attributes: List[str] = []
    # Apply default access if no explicit access is specified
    if default_access and "PUBLIC" not in attributes and "PRIVATE" not in attributes:
        attributes.append(default_access)

    enum_description: EnumDescription = {
        "name": enum_name,
        "description": description,
        "attributes": attributes,
        "enumerators": enumerators,
        "binding_type": binding_type
    }
    return first_enum_name, enum_description
