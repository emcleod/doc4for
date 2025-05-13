from fparser.two.Fortran2003 import Type_Declaration_Stmt, Attr_Spec
from fparser.two.utils import walk

def has_attribute(declaration: Type_Declaration_Stmt, attr_name: str) -> bool:
    attributes = [attr.string for attr in walk(declaration, Attr_Spec)]
    return attr_name in attributes
