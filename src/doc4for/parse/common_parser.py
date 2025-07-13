import re
import logging
from typing import List, Dict, Any, Tuple, TypeVar, Generic, Type, Protocol
from fparser.two.Fortran2003 import (
    Comment, 
    Name, 
    Initialization,
    Intrinsic_Type_Spec, 
    Length_Selector,
    Type_Param_Value,
    Int_Literal_Constant,
    Real_Literal_Constant,
    Array_Constructor,
    Explicit_Shape_Spec,
    Level_2_Unary_Expr,
    Declaration_Type_Spec,
    Kind_Selector,
    Assumed_Shape_Spec,
    Language_Binding_Spec,
    Char_Literal_Constant,
    Intrinsic_Function_Reference,
    Char_Selector,
    Level_2_Expr,
    Suffix,
    Component_Decl,
    Component_Initialization,
    Deferred_Shape_Spec
)
from fparser.two.utils import walk
from doc4for.models.variable_models import PolymorphismType
from doc4for.models.dimension_models import ArrayBound, BoundType, Expression
from doc4for.models.common import ExpressionType, BindingType, BindingTypeEnum
from doc4for.models.dimension_models import Dimension

logger: logging.Logger = logging.getLogger(__name__)

T = TypeVar("T")

class HandlerProtocol(Protocol):
    def __call__(self, item: Any, data: T, comment_stack: List[Comment], **kwargs: Any) -> None:
        ...  
class FortranHandler(Generic[T]):
    TYPE_HANDLING = {
        #TODO these all need to be removed when the migration is complete
        # EndModule: None,
        # EndType: None,
        # Contains: None,
        # Implicit: None,
        Comment: None,  # Though we shouldn't get here
        # Private: "WARNING",
        # Public: "WARNING",
    }

    def __init__(self):
        self.handlers: Dict[Type, HandlerProtocol] = {}

    def register_handler(self, item_type: Type, handler: HandlerProtocol) -> None:
        self.handlers[item_type] = handler
 
    def get_handler(self, item_type: Type) -> HandlerProtocol:
        # Check registered handlers first
        for handler_type, handler in self.handlers.items():
            if issubclass(item_type, handler_type):
                return handler
                
        # Then check TYPE_HANDLING
        for type_class, handling in self.TYPE_HANDLING.items():
            if issubclass(item_type, type_class):
                if handling is None:
                    return self.handle_ignored_item
                if handling == "WARNING":
                    return self.handle_unimplemented_item
                    
        return self.handle_unimplemented_item
    
    def handle_unimplemented_item(self, item: Any, data: T, comment_stack: List[Comment], **kwargs: Any) -> None:
        logger.warning("Unimplemented type %s", type(item))
        
    def handle_ignored_item(self, item: Any, data: T, comment_stack: List[Comment], **kwargs: Any) -> None:
        logger.info("Ignored type %s", type(item))

def _extract_type(declaration: Any) -> Tuple[str, PolymorphismType]: #TODO is there a way to narrow from Any
    types = walk(declaration, Intrinsic_Type_Spec)
    if len(types) == 1:
        # we have an intrinsic type
        return types[0].children[0], PolymorphismType.NONE
    types = walk(declaration, Declaration_Type_Spec)
    if len(types) == 1:
        # we have either a TYPE or a CLASS
        type_spec, type_details = types[0].children
        if type_spec == "TYPE":
            return type_details.string, PolymorphismType.NONE
        elif type_spec == "CLASS":
            if isinstance(type_details, str) and type_details == "*":
                return type_details, PolymorphismType.UNLIMITED
            elif hasattr(type_details, "string"):
                return type_details.string, PolymorphismType.LIMITED
    raise ValueError("Not handled yet TODO", types)

def _extract_kind(declaration) -> str:
    #TODO this can be a loop over an array of allowed types
    # e.g. real(kind=8) or real(kind=c_int)
    kind_selector = walk(declaration, Kind_Selector)
    if kind_selector:
        # real(kind=selected_real_kind(15))
        kind = walk(kind_selector, Intrinsic_Function_Reference)
        if kind:
            return kind[0].string
        # real*8 or real(kind=8)
        kind = walk(kind_selector, Int_Literal_Constant)
        if kind:
            return kind[0].string
        kind = walk(kind_selector, Name)
        if kind:
            return kind[0].string
    return None

def _extract_char_length(declaration: Any) -> str:
    #TODO this can be a loop over an array of allowed types
    length_selector = walk(declaration, Length_Selector)
    if length_selector:
        # check for complex expressions first and just return the string
        length_value = walk(length_selector, Level_2_Expr)
        if length_value:
            return length_value[0].string
        # character(len=10)
        length_value = walk(length_selector, Int_Literal_Constant)    
        if length_value:
            # Get the first integer literal found
            return length_value[0].string
        # character(len=*)
        length_value = walk(length_selector, Type_Param_Value)
        if length_value:
            return length_value[0].string
        # character(len=n)
        length_value = walk(length_selector, Name)
        if length_value:
            return length_value[0].string
        # character(char_kind) - probably a bug in the Fortran but may as well handle it
        length_value = walk(length_selector, Intrinsic_Function_Reference)
        if length_value:
            return length_value[0].string
    return None

def _extract_length_and_kind(declaration: Any) -> Tuple[str, str]:
    selector = walk(declaration, Char_Selector)
    length, kind = None, None
    if selector:
        length_selector = selector[0].children[0]
        kind_selector = selector[0].children[1]
        if length_selector:
            length = length_selector.string
        if kind_selector:
            kind = kind_selector.string
            # have an fparser issue with declarations like selected_char_kind('ASCII')
            # so grab it from the parent
            if kind and "F2PY_EXPR_TUPLE" in kind:
                # have to look at the original string in the declaration
                original_string = declaration.string
                pattern = r"selected_char_kind\s*\(\s*['\"](.*?)['\"]\s*\)"
                match = re.search(pattern, original_string)
                if match:
                    actual_value = match.group(1)
                    kind = f"selected_char_kind('{actual_value}')"
    return length, kind

# TODO split these up so each only handles one sort of thing
def _extract_type_info(declaration) -> Dict[str, str]:
    info = {}
    info["base_type"], info["polymorphism_type"] = _extract_type(declaration)
    length = _extract_char_length(declaration)
    kind = _extract_kind(declaration)
    if not length and not kind:
        # use character(len=, kind=) rule
        length, kind = _extract_length_and_kind(declaration)
    # if it's a char but doesn't have a length, Fortran defines the length to be one
    if info["base_type"] == "CHARACTER" and not length:
        length = "1"
    info["length"] = length
    info["kind"] = kind
    info["binding_type"] = _extract_binding_type(walk(declaration, Language_Binding_Spec))
    return info

def _extract_literal_value(node):
    # If it's a Component_Decl, look for Component_Initialization
    if isinstance(node, Component_Decl):
        initializations = walk(node, Component_Initialization)
        if initializations:
            # Extract the value from the initialization (skip the '=' at children[0])
            init_value = initializations[0].children[1] if len(initializations[0].children) > 1 else None
            if init_value:
                # Recursively process the value
                return _extract_literal_value(init_value)
        return None
    
    # Handle unary expressions (like negative numbers)
    if isinstance(node, Level_2_Unary_Expr):
        operator = str(node.children[0])
        operand = node.children[1]
        
        if operator == '-' and isinstance(operand, (Int_Literal_Constant, Real_Literal_Constant)):
            value = str(operand.children[0]) if operand.children else str(operand)
            return f'-{value}'
    
    # Check if it's a literal constant
    if isinstance(node, (Int_Literal_Constant, Real_Literal_Constant)):
        return str(node.children[0]) if node.children else str(node)
    
    return str(node)
# def _extract_literal_value(node):
#     # Handle unary expressions (like negative numbers)
#     if isinstance(node, Level_2_Unary_Expr):
#         # node.children should be (operator, operand)
#         operator = str(node.children[0])
#         operand = node.children[1]
        
#         # If it's a negative literal, combine the operator with the value
#         if operator == '-' and isinstance(operand, (Int_Literal_Constant, Real_Literal_Constant)):
#             value = str(operand.children[0]) if operand.children else str(operand)
#             return f'-{value}'  # Combine the minus with the literal value
    
#     # Check if it's a literal constant
#     if isinstance(node, (Int_Literal_Constant, Real_Literal_Constant)):
#         return str(node.children[0]) if node.children else str(node)
    
#     return str(node)

def _extract_entity_info(entity_decl):
    """Extract name, dimension, and value from an entity declaration."""
    info = {}
    info["name"] = walk(entity_decl, Name)[0].string

    dimension_info = _extract_dimension_info(walk(entity_decl, (Explicit_Shape_Spec, Assumed_Shape_Spec)))
    info["dimension"] = dimension_info
    
    initialization = walk(entity_decl, Initialization)
    if initialization:
        info["value"] = _extract_value_from_initialization(initialization[0])
    return info

def _extract_dimension_info(shape_spec_list) -> Dimension:    
    dimensions = []
    declared_dimensions = walk(shape_spec_list, Deferred_Shape_Spec)
    if declared_dimensions:
        for declared_dimension in declared_dimensions:
            dimensions.append(ArrayBound(BoundType.DEFERRED))
    else:    
        declared_dimensions = walk(shape_spec_list, (Explicit_Shape_Spec, Assumed_Shape_Spec))
        if not declared_dimensions:
            return None
        
        bound_type: BoundType = None
        # find the bound type first
        for declared_dimension in declared_dimensions:
            # we have an assumed shape
            if isinstance(declared_dimension, Assumed_Shape_Spec):
                dimensions.append(ArrayBound(BoundType.ASSUMED_SHAPE))
            else:
                lower_expr: Expression = None
                upper_expr: Expression = None
                bound_type: BoundType = None
                # this is not nice - it depends on the order if the if-else statement
                if walk(declared_dimension, Name):   
                    bound_type = BoundType.VARIABLE
                elif walk(declared_dimension, Int_Literal_Constant):
                    bound_type = BoundType.FIXED
                lower_expr = _node_to_expression(declared_dimension.children[0])
                if not lower_expr:
                    # nothing declared so default to 1
                    lower_expr = Expression(ExpressionType.LITERAL, "1")
                upper_expr = _node_to_expression(declared_dimension.children[1])
                dimensions.append(ArrayBound(bound_type, lower_expr, upper_expr))
    return {"dimensions": dimensions} if dimensions else None

def _node_to_expression(node: Any) -> Expression:
    """Convert an AST node to an Expression object."""
    if node is None:
        return None
    
    # Handle unary expressions (like negative numbers)
    if isinstance(node, Level_2_Unary_Expr):
        # node.children should be (operator, operand)
        operator = str(node.children[0])
        operand = node.children[1]
        
        # If it's a negative literal, combine the operator with the value
        if operator == '-' and isinstance(operand, (Int_Literal_Constant, Real_Literal_Constant)):
            value = str(operand.children[0]) if operand.children else str(operand)
            return Expression(
                expr_type=ExpressionType.LITERAL,
                value=f'-{value}'  # Combine the minus with the literal value
            )
         # If it's a negative variable, combine the operator with the variable name
        elif operator == '-' and isinstance(operand, Name):
            return Expression(
                expr_type=ExpressionType.VARIABLE,  
                value=f'-{str(operand)}'
            )

    # Check if it's a literal constant
    if isinstance(node, (Int_Literal_Constant, Real_Literal_Constant)):
        return Expression(
            expr_type=ExpressionType.LITERAL,
            value=str(node.children[0]) if node.children else str(node)
        )
    
    # Check if it's a variable reference
    if isinstance(node, Name):
        return Expression(
            expr_type=ExpressionType.VARIABLE,
            value=str(node)
        )
    
    # For now, default to literal  
    return Expression(
        expr_type=ExpressionType.LITERAL,
        value=str(node)
    )

def _extract_value_from_initialization(initialization: Initialization) -> str:
    # Get the expression after '='
    if len(initialization.children) > 1:
        value_expr = initialization.children[1]
        # Return the string representation of the expression
        return normalize_negative_numbers(str(value_expr))
    
    return None

def normalize_negative_numbers(value: str) -> str:
    """Remove spaces between minus signs and numbers in string representation."""
    if value is None:
        return None
    # Replace '- ' followed by a digit with '-' followed by the digit
    return re.sub(r'-\s+(\d)', r'-\1', value)


def _extract_array_constructor_value(array_constructor: Array_Constructor) -> str:
    return array_constructor.string #TODO
# The Array_Constructor handling needs improvements to handle cases like complex arrays
# The _extract_type_info function doesn't handle kind parameters (e.g., integer(kind=8))
# There's no handling for uppercase parameter names in tests vs lowercase in the code output
# Need more specific handling for character literals with quotes
# Character length extraction and handling might need refinement

def _extract_binding_type(language_bindings: List[Language_Binding_Spec]) -> BindingType:
    if not language_bindings:
        return None
    names = walk(language_bindings, Char_Literal_Constant)
    if names:
        name_with_quotes = names[0].string
        # Remove surrounding quotes (handles both " and ')
        if (name_with_quotes.startswith('"') and name_with_quotes.endswith('"')) or \
           (name_with_quotes.startswith("'") and name_with_quotes.endswith("'")):
            name = name_with_quotes[1:-1]
        else:
            name = name_with_quotes
        return { "name": name, "type": BindingTypeEnum.BIND_C}
    return { "name": None, "type": BindingTypeEnum.BIND_C }
    
