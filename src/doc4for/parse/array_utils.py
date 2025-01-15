from typing import List, Tuple, Optional
from doc4for.models.dimension_models import ArrayBound
from doc4for.parse.dimension_parser import extract_variable_dimension

def expand_repeat_value(value_str: str) -> list[str]:
    if '*' in value_str:
        count_str, value = value_str.split('*')
        return [value] * int(count_str)
    return [value_str]

def calculate_array_size(dimensions: List[ArrayBound]) -> int:
    array_size = 1
    for dim in dimensions:
        if dim["upper"] is None or dim["lower"] is None:
            # For implied shape arrays, we can't calculate the size
            # It's determined by the initialization
            return None
        upper = int(dim["upper"].value)
        lower = int(dim["lower"].value)
        array_size *= (upper - lower + 1)
    return array_size

def expand_array_values(values: List[str], array_size: int, start_index: int) -> Tuple[str, int]:
    expanded_values = []
    current_size = 0
    value_index = start_index
    
    while current_size < array_size and value_index < len(values):
        expanded = expand_repeat_value(str(values[value_index]))
        expanded_values.extend(expanded)
        current_size += len(expanded)
        value_index += 1
    
    expanded_values = expanded_values[:array_size]
    return ", ".join(expanded_values), value_index - start_index    


def normalize_complex_init(inner_value: str) -> str:
    """Normalize whitespace in complex initializations while preserving DO loops."""
    if not ("," in inner_value and "(" in inner_value and ")" in inner_value):
        return inner_value  # No commas or parens, return as-is

    # Split the string into potential segments, preserving parenthesized expressions
    segments = []
    current = ""
    paren_count = 0

    for char in inner_value:
        if char == ',' and paren_count == 0:
            segments.append(current)
            current = ""
        else:
            current += char
            if char == '(':
                paren_count += 1
            elif char == ')':
                paren_count -= 1

    if current:
        segments.append(current)

    # Check if this looks like a DO loop
    if any(" i=" in seg or " j=" in seg for seg in segments):
        return inner_value  # DO loop, return as-is

    # Not a DO loop, normalize whitespace
    return ', '.join(s.strip() for s in segments)

#TODO maybe have an option in the config to not expand array values i.e. [1.0 * 2, 3.0 * 3]
def parse_initialization_value(entity: str) -> Tuple[str, Optional[str]]:
    if "=>" in entity:
        name, value = [x.strip() for x in entity.split("=>", 1)]
        return name, value
    elif "=" in entity:
        name, value = [x.strip() for x in entity.split("=", 1)]
        
        dimension = extract_variable_dimension(name)
        if dimension:
            # First, check if this is a complex expression (array followed by operation)
            if value.startswith("[") and not value.endswith("]"):
                # This looks like an array followed by an operation - return as-is
                return name, value
            
            # Handle pure array constructors
            if value.startswith("["):
                inner_value = value[1:-1]  # Remove [ ]
            elif value.startswith("(/"):
                inner_value = value[2:-2]  # Remove (/ /)
            # Add handling for implied DO loop
            elif value.startswith("(") and "=" in value:
                # Return the DO loop as-is
                return name, value                
            else:
                # Return as-is for other complex initializations
                return name, value
                
            # Check if this contains derived type constructors
            if "(" in inner_value and ")" in inner_value:
                #TODO should parse the type declaration to see if we're dealing with a derived type
                # This looks like a derived type constructor or function call
                inner_value = normalize_complex_init(inner_value)
                return name, inner_value
            else:
                # Process regular array values
                value_list = [v.strip() for v in inner_value.split(',')]
                array_size = calculate_array_size(dimension["dimensions"])
                if array_size is None:
                    array_size = len(value_list)
                expanded_value, _ = expand_array_values(value_list, array_size, 0)
                return name, expanded_value
        return name, value
    else:
        return entity.strip(), None
    
