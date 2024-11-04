from typing import List, Type, Dict, Optional, Tuple
import re
from fparser.one.block_statements import (
    Module,
    Comment,
    Function,
    Subroutine,
    Program,
    Public,
    Type,
    Use,
    BlockData,
    Common,
    Data,
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.data_models import (
    FunctionDescription,
    SubroutineDescription,
    ParameterDescription,
    TypeDescription,
    ModuleDescription,
    ProgramDescription,
    Uses,
    VariableDescription,
    BlockDataDescription,
    Dimension
)
from doc4for.comment_utils import is_doc4for_comment, format_comments
from doc4for.argument_utils import (
    update_arguments_with_comment_data,
    update_arguments_with_parsed_data,
)
from doc4for.type_utils import update_type_with_parsed_data, extract_dimension


def parse_program(
    program: Program, comment_stack: List[Comment], file_name: str
) -> ProgramDescription:
    program_details: ProgramDescription = {
        "program_name": program.name,
        "file_name": file_name,
        "program_description": "",
        "uses": {},
    }
    if is_doc4for_comment(comment_stack):
        program_details["program_description"] = format_comments(comment_stack)
    for program_child in program.content:
        if isinstance(program_child, Use):
            module_name = program_child.name
            if module_name not in program_details["uses"]:
                uses: Uses = {"module_name": program_child.name, "selections": []}
                program_details["uses"][module_name] = uses
            if (
                not program_child.items
                and program_details["uses"][module_name]["selections"]
            ):
                # everything in the module is used, so any selections are overwritten
                program_details["uses"][module_name]["selections"] = []
            else:
                program_details["uses"][module_name]["selections"].extend(
                    program_child.items
                )
    return program_details


def parse_block_data(
    block_data: BlockData, comment_stack: List[Comment]
) -> BlockDataDescription:
    block_data_details: BlockDataDescription = {
        "name": block_data.name,
        "description": "",
        "common_blocks": {},
    }
    if is_doc4for_comment(comment_stack):
        block_data_details["description"] = format_comments(comment_stack)

    # Track variables we've seen and their types
    variable_descriptions: Dict[str, VariableDescription] = {}
    for item in block_data.content:
        if isinstance(item, TypeDeclarationStatement):
            variables = parse_variable(item, [])  # TODO maintain a comment stack
            for variable in variables:
                variable_descriptions[variable["name"]] = variable
    for item in block_data.content:
        if isinstance(item, Common):
            common_name = item.items[0][0]
            variable_names = item.items[0][1]
            block_data_details["common_blocks"]["name"] = {}
            for variable_name in variable_names:
                block_data_details["common_blocks"]["name"][common_name] = {
                    "description": "",
                    "type": "",
                    "name": variable_name,
                    "dimension": None,
                    "attributes": [],
                    "kind": None,
                    "initial_value": None,
                }
            # # Create entries for these variables, using type info if we already have it
            # block_data_details["common_blocks"][common_name] = {
            #     name: variable_types.get(
            #         name,
            #         {
            #             "description": "",
            #             "type": "",
            #             "name": name,
            #             "dimension": None,
            #             "attributes": [],
            #             "kind": None,
            #             "initial_value": None,
            #         },
            #     )
            #     for name in variables
            # }

        elif isinstance(item, TypeDeclarationStatement):
            pass
            # var_type = item.get_type()
            # for item in item.items:
            #     var_info = {
            #         "description": "",
            #         "type": var_type,
            #         "name": item.name,
            #         "dimension": (
            #             parse_dimension(item.dimension) if item.dimension else None
            #         ),
            #         "attributes": [],
            #         "kind": item.kind if hasattr(item, "kind") else None,
            #         "initial_value": None,
            #     }

            #     # Update our tracking dict
            #     variable_types[item.name] = var_info

            #     # Update any existing common block entries
            #     for common_block in block_data_details["common_blocks"].values():
            #         if item.name in common_block:
            #             common_block[item.name].update(var_info)
        elif isinstance(item, Data):
            pass
            # for var_name, value in zip(item.items, item.values):
            #     for common_block in block_data_details["common_blocks"].values():
            #         if var_name in common_block:
            #             common_block[var_name]["initial_value"] = str(value)

    return block_data_details


def parse_module(
    module: Module, comment_stack: List[Comment], file_name: str
) -> ModuleDescription:
    module_data: ModuleDescription = {
        "module_name": module.name,
        "parameters": {},
        "variables": {},
        "functions": {},
        "subroutines": {},
        "types": {},
        "file_name": file_name,
        "module_description": "",
    }
    if is_doc4for_comment(comment_stack):
        module_data["module_description"] = format_comments(comment_stack)
    return module_data


def parse_function(
    function: Function, comment_stack: List[Comment]
) -> FunctionDescription:
    attributes: List[str] = [
        attr.strip().lower() for attr in function.prefix.split() if attr.strip()
    ]
    function_description: FunctionDescription = {
        "attributes": attributes,
        "description": "",
        "arguments": function.args,
        "in": {},
        "out": {},
        "return": {},
        "binding_type": "",
        "interface": "",
    }
    update_arguments_with_parsed_data(function, function_description)
    if comment_stack:
        update_arguments_with_comment_data(comment_stack, function_description)
    return function_description


def parse_subroutine(
    subroutine: Subroutine, comment_stack: List[Comment]
) -> SubroutineDescription:
    attributes: List[str] = [
        attr.strip().lower() for attr in subroutine.prefix.split() if attr.strip()
    ]
    subroutine_description: SubroutineDescription = {
        "attributes": attributes,
        "description": "",
        "arguments": subroutine.args,
        "in": {},
        "out": {},
        "binding_type": "",
        "interface": "",
    }
    update_arguments_with_parsed_data(subroutine, subroutine_description)
    if comment_stack:
        update_arguments_with_comment_data(comment_stack, subroutine_description)
    return subroutine_description


def parse_type(
    type: Type, comment_stack: List[Comment], public_declarations: List[str]
) -> TypeDescription:
    type_name: str = type.name
    type_description: TypeDescription = {
        "type_name": type_name,
        "attributes": [],
        "description": "",
        "data_components": {},
        "procedures": {},
        "generic_interfaces": {},
        "extends": None,
    }
    if any(spec.startswith("extends") for spec in type.specs):
        extends_spec = next(spec for spec in type.specs if spec.startswith("extends"))
        match = re.search(r"extends\s*\(\s*(\w+)\s*\)", extends_spec)
        if match:
            base_type = match.group(1)
            type_description["extends"] = base_type
    else:
        type_description["attributes"].extend(type.specs)
    if type_name in public_declarations:
        type_description["attributes"].append("public")
    update_type_with_parsed_data(type, type_description)
    if comment_stack:
        type_description["description"] = format_comments(comment_stack)
    return type_description


def parse_variable(
    declaration: TypeDeclarationStatement, comment_stack: List[Comment]
) -> List[VariableDescription]:
    item_str = declaration.item.line
    description = (
        format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""
    )
    base_type = str(declaration.name).lower()
    shared_attributes = get_attributes(declaration)
    kind = extract_kind(declaration)

    variable_descriptions: List[VariableDescription] = []

    if "::" in item_str:
        var_part = item_str.split("::", 1)[1].strip()
    else:
        base_with_kind = base_type
        if "(" in item_str:
            type_end = len(base_type)
            if item_str.find("(") == type_end:
                base_with_kind = item_str[: item_str.find(")") + 1]
            else:
                base_with_kind = base_type
        elif "*" in item_str:
            base_with_kind = (
                item_str[: item_str.find("*")]
                + item_str[item_str.find("*") : item_str.find(" ", item_str.find("*"))]
            )

        var_part = item_str[len(base_with_kind) :].strip()

    var_declarations = [v.strip() for v in var_part.split(",")]

    for var_decl in var_declarations:
        if "=" in var_decl:
            name, initial_value = [x.strip() for x in var_decl.split("=", 1)]
        else:
            name = var_decl.strip()
            initial_value = None

        # Extract dimension using the new method
        dimension = extract_variable_dimension(name)

        # Remove any parenthetical expressions from the name
        name = re.sub(r"\([^)]*\)", "", name).strip()

        # Handle allocatable arrays
        if "allocatable" in shared_attributes and "(:)" in name:
            name = name.replace("(:)", "")

        variable_description: VariableDescription = {
            "description": description,
            "type": base_type,
            "name": name,
            "dimension": dimension,
            "attributes": shared_attributes,
            "kind": kind,
            "initial_value": initial_value,
        }

        variable_descriptions.append(variable_description)

    return variable_descriptions


# TODO public declaration should be handled here?
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
    parameter_description: ParameterDescription = {
        "description": "",
        "type": str(declaration.name),
        "name": name.strip(),
        "value": value.strip(),
        "dimension": "",
        "attributes": [],
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


def get_attributes(declaration: TypeDeclarationStatement) -> List[str]:
    """Extracts attributes from a TypeDeclarationStatement.

    Args:
        declaration: The TypeDeclarationStatement to process

    Returns:
        List[str]: List of attributes (public, private, etc.)
    """
    return declaration.attrspec if declaration.attrspec else []


def extract_kind(declaration: TypeDeclarationStatement) -> Optional[str]:
    """Extract kind specification from a type declaration.

    Args:
        declaration: The type declaration statement

    Returns:
        Optional[str]: The kind specification if present, None otherwise
    """
    if hasattr(declaration, "selector") and isinstance(declaration.selector, Tuple):
        star_value, kind_value = declaration.selector
        return star_value or kind_value or None
    return None


def extract_variable_dimension(name: str) -> Optional[Dimension]:
    """Extract dimension information from a variable name with array specification.

    Args:
        name (str): Variable name potentially including dimension specification

    Returns:
        Optional[Dimension]: The dimension information if present, None otherwise

    Examples:
        "x(10)"     -> {'dimensions': [10]}
        "x(10,20)"  -> {'dimensions': [10, 20]}
        "x"         -> None
    """
    if "(" not in name or ")" not in name:
        return None

    # Extract what's inside the parentheses
    dim_match = re.search(r"\(([^)]+)\)", name)
    if not dim_match:
        return None

    dim_str = dim_match.group(1)

    # Split on comma for multi-dimensional arrays
    dims = []
    for d in dim_str.split(","):
        # For now, just handle simple integer dimensions
        try:
            dims.append(int(d.strip()))
        except ValueError:
            continue  # Skip non-integer dimensions for now

    return {"dimensions": dims} if dims else None
