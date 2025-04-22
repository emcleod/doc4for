import unittest
from pathlib import Path
from typing import cast
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data
from doc4for.models.common import Expression, ExpressionType, BindingTypeEnum
from doc4for.models.dimension_models import ArrayBound, BoundType, Dimension

# Helper function for creating dimension expressions
def create_dimension_expr(lower, upper):
    return ArrayBound(
        bound_type=BoundType.FIXED,
        lower=Expression(expr_type=ExpressionType.LITERAL, value=str(lower), function_name=None, arguments=None),
        stride=None,
        upper=Expression(expr_type=ExpressionType.LITERAL, value=str(upper), function_name=None, arguments=None)
    )

class TestBlockData(TestCase):
    maxDiff=None
    def setUp(self):
        self.setUpPyfakefs()

    def test_unnamed_block_data_no_comments(self):
        self.fs.create_file(
            "/fake/path/blockdata.f90",
            contents="""\
    BLOCK DATA
        COMMON // x, y
        REAL x, y
        DATA x, y/1.0, 2.0/
    END BLOCK DATA
    """,
        )
        result = extract_file_data([Path("/fake/path/blockdata.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
        self.assertEqual(len(file_data["block_data"]), 1)
        block_data = file_data["block_data"]['']
        
        expected = {
            "name": "",
            "description": "",
            "common_blocks": {
                "": {
                    "name": "",
                    "description": "",
                    "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None },
                    "variables": {
                        "x": {
                            "description": "",
                            "type": "real",
                            "name": "x",
                            "dimension": None,
                            "attributes": [],
                            "kind": None,
                            "initial_value": "1.0",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None },
                        },
                        "y": {
                            "description": "",
                            "type": "real",
                            "name": "y",
                            "dimension": None,
                            "attributes": [],
                            "kind": None,
                            "initial_value": "2.0", 
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None },
                        }
                    }
                }
            },
            "data_statements": [{"description": None,
                                "implied_initialisation": "",
                                "variable": "x",
                                "value": "1.0"},
                               {"description": None,
                                "implied_initialisation": "",
                                "variable": "y",
                                "value": "2.0"}],
            "other_variables": {}
        }
        self.assertEqual(block_data, expected)

    def test_unnamed_common_block_data_no_comments(self):
        self.fs.create_file(
            "/fake/path/blockdata.f90",
            contents="""\
BLOCK DATA simple
    COMMON // x, y
    REAL x, y
    DATA x, y/1.0, 2.0/
END BLOCK DATA simple
""",
        )
        result = extract_file_data([Path("/fake/path/blockdata.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
        self.assertEqual(len(file_data["block_data"]), 1)
        block_data = file_data["block_data"]['simple']
        expected = {
            "name": "simple",
            "description": "",
            "common_blocks": {
                "": {
                    "name": "",
                    "description": "",
                    "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None },
                    "variables": {
                        "x": {
                            "description": "",
                            "type": "real",
                            "name": "x",
                            "dimension": None,
                            "attributes": [],
                            "kind": None,
                            "initial_value": "1.0",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None }
                        },
                        "y": {
                            "description": "",
                            "type": "real",
                            "name": "y",
                            "dimension": None,
                            "attributes": [],
                            "kind": None,
                            "initial_value": "2.0",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None }
                        }
                    }
                }
            },
            "data_statements": [{"description": None,
                                 "implied_initialisation": "",
                                 "variable": "x",
                                 "value": "1.0"},
                                {"description": None,
                                 "implied_initialisation": "",
                                 "variable": "y",
                                 "value": "2.0"}],
            "other_variables": {}
        }        
        self.assertEqual(block_data, expected)

    def test_simple_block_data_no_comments(self):
        self.fs.create_file(
            "/fake/path/blockdata.f90",
            contents="""\
BLOCK DATA simple
    COMMON /basic/ x, y
    REAL x, y
    DATA x, y/1.0, 2.0/
END BLOCK DATA simple
""",
        )
        result = extract_file_data([Path("/fake/path/blockdata.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
        self.assertEqual(len(file_data["block_data"]), 1)
        block_data = file_data["block_data"]['simple']
        expected = {
            "name": "simple",
            "description": "",
            "common_blocks": {
                "basic": {
                    "name": "basic",
                    "description": "",
                    "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None },
                    "variables": {
                        "x": {
                            "description": "",
                            "type": "real",
                            "name": "x",
                            "dimension": None,
                            "attributes": [],
                            "kind": None,
                            "initial_value": "1.0",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
                        },
                        "y": {
                            "description": "",
                            "type": "real",
                            "name": "y",
                            "dimension": None,
                            "attributes": [],
                            "kind": None,
                            "initial_value": "2.0",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
                        },
                    }
                }
            },
            "data_statements": [{"description": None,
                                 "implied_initialisation": "",
                                 "variable": "x",
                                 "value": "1.0"},
                                {"description": None,
                                 "implied_initialisation": "",
                                 "variable": "y",
                                 "value": "2.0"}],
            "other_variables": {}
        }
        self.assertEqual(block_data, expected)

    def test_simple_block_data_multiple_line_initialization_no_comments(self):
        self.fs.create_file(
            "/fake/path/blockdata.f90",
            contents="""\
BLOCK DATA simple
    COMMON /basic/ x, y
    REAL x, y
    DATA x /1.0/
    DATA y /2.0/
END BLOCK DATA simple
""",
        )
        result = extract_file_data([Path("/fake/path/blockdata.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
        self.assertEqual(len(file_data["block_data"]), 1)
        block_data = file_data["block_data"]['simple']
        expected = {
            "name": "simple",
            "description": "",
            "common_blocks": {
                "basic": {
                    "name": "basic",
                    "description": "",
                    "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None },
                    "variables": {
                        "x": {
                            "description": "",
                            "type": "real",
                            "name": "x",
                            "dimension": None,
                            "attributes": [],
                            "kind": None,
                            "initial_value": "1.0",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
                        },
                        "y": {
                            "description": "",
                            "type": "real",
                            "name": "y",
                            "dimension": None,
                            "attributes": [],
                            "kind": None,
                            "initial_value": "2.0",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
                        }
                    },
                }
            },
            "data_statements": [{"description": None,
                                 "implied_initialisation": "",
                                 "variable": "x",
                                 "value": "1.0"},
                                {"description": None,
                                 "implied_initialisation": "",
                                 "variable": "y",
                                 "value": "2.0"}],
            "other_variables": {}
        }
        self.assertEqual(block_data, expected)

    def test_simple_block_data_different_order_no_comments(self):
        self.fs.create_file(
            "/fake/path/blockdata.f90",
            contents="""\
BLOCK DATA simple
    COMMON /basic/ x, y, z, theta
    REAL x, y, z, theta
    DATA y, theta /2.0, 0.45/
    DATA z, x /3.0, 1.0/
END BLOCK DATA simple
""",
        )
        result = extract_file_data([Path("/fake/path/blockdata.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
        self.assertEqual(len(file_data["block_data"]), 1)
        block_data = file_data["block_data"]['simple']
        expected = {
            "name": "simple",
            "description": "",
            "common_blocks": {
                "basic": {
                    "name": "basic",
                    "description": "",
                    "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None },
                    "variables": {
                        "x": {
                            "description": "",
                            "type": "real",
                            "name": "x",
                            "dimension": None,
                            "attributes": [],
                            "kind": None,
                            "initial_value": "1.0",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
                        },
                        "y": {
                            "description": "",
                            "type": "real",
                            "name": "y",
                            "dimension": None,
                            "attributes": [],
                            "kind": None,
                            "initial_value": "2.0",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
                        },
                        "z": {
                            "description": "",
                            "type": "real",
                            "name": "z",
                            "dimension": None,
                            "attributes": [],
                            "kind": None,
                            "initial_value": "3.0",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
                        },
                        "theta": {
                            "description": "",
                            "type": "real",
                            "name": "theta",
                            "dimension": None,
                            "attributes": [],
                            "kind": None,
                            "initial_value": "0.45",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
                        },
                    }
                }
            },
            "data_statements": [{"description": None,
                                 "implied_initialisation": "",
                                 "variable": "y",
                                 "value": "2.0"},
                                {"description": None,
                                 "implied_initialisation": "",
                                 "variable": "theta",
                                 "value": "0.45"},
                                {"description": None,
                                 "implied_initialisation": "",
                                 "variable": "z",
                                 "value": "3.0"},
                                {"description": None,
                                 "implied_initialisation": "",
                                 "variable": "x",
                                 "value": "1.0"}],
            "other_variables": {}
        }
        self.assertEqual(block_data, expected)

    def test_block_data_with_single_array_no_comments(self):
        self.fs.create_file(
            "/fake/path/blockdata.f90",
            contents="""\
BLOCK DATA
    COMMON // x
    INTEGER x(10)
    DATA x /1, 2, 3, 4, 5, 6, 7, 8, 9, 10/
END BLOCK DATA
""",
        )
        result = extract_file_data([Path("/fake/path/blockdata.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
        self.assertEqual(len(file_data["block_data"]), 1)
        block_data = file_data["block_data"]['']
        expected = {
            "name": "",
            "description": "",
            "common_blocks": {
                "": {
                    "name": "",
                    "description": "",
                    "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None },
                    "variables": {
                        "x": {
                            "description": "",
                            "type": "integer",
                            "name": "x",
                            "dimension": {'dimensions': [create_dimension_expr(1, 10)]},
                            "attributes": [],
                            "kind": None,
                            "initial_value": "1, 2, 3, 4, 5, 6, 7, 8, 9, 10",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
                        },
                    }
                }
            },
            "data_statements": [{"description": None,
                                 "implied_initialisation": "",
                                 "variable": "x",
                                 "value": "1, 2, 3, 4, 5, 6, 7, 8, 9, 10"}],
            "other_variables": {}
        }
        self.assertEqual(block_data, expected)


    def test_block_data_with_multiple_array_no_comments(self):
        self.fs.create_file(
            "/fake/path/blockdata.f90",
            contents="""\
BLOCK DATA my_block
    COMMON /arrays/ x, y
    INTEGER x(3), y(2)
    DATA x, y /1, 2, 3, 4, 5/
END BLOCK DATA
""",
        )
        result = extract_file_data([Path("/fake/path/blockdata.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
        self.assertEqual(len(file_data["block_data"]), 1)
        block_data = file_data["block_data"]['my_block']
        expected = {
            "name": "my_block",
            "description": "",
            "common_blocks": {
                "arrays": {
                    "name": "arrays",
                    "description": "",
                    "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None },
                    "variables": {
                        "x": {
                            "description": "",
                            "type": "integer",
                            "name": "x",
                            "dimension": {'dimensions': [create_dimension_expr(1, 3)]},
                            "attributes": [],
                            "kind": None,
                            "initial_value": "1, 2, 3",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
                        },
                        "y": {
                            "description": "",
                            "type": "integer",
                            "name": "y",
                            "dimension": {'dimensions': [create_dimension_expr(1, 2)]},
                            "attributes": [],
                            "kind": None,
                            "initial_value": "4, 5",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
                        },
                    }
                }
            },
            "data_statements": [{"description": None,
                               "implied_initialisation": "",
                               "variable": "x",
                               "value": "1, 2, 3"},
                              {"description": None,
                               "implied_initialisation": "",
                               "variable": "y",
                               "value": "4, 5"}],
            "other_variables": {}
        }
        self.assertEqual(block_data, expected)


    def test_complex_block_data_no_comments(self):
        self.fs.create_file("/fake/path/blockdata.f90",
                            contents="""\
    block data simulation_data
        implicit none

        ! Physical parameters
        common /physics/ gravity, air_density, viscosity
        real*8 gravity, air_density
        real viscosity

        ! Simulation settings
        common /settings/ max_iterations, debug_level, use_fast_mode
        integer max_iterations, debug_level
        logical use_fast_mode

        ! Lookup table data
        common /tables/ temp_points, pressure_values, coefficients
        real temp_points(10), pressure_values(10)
        real coefficients(3,3)

        ! Character data
        common /strings/ model_name, version
        character*20 model_name
        character*8  version

        ! Initialize physics values
        data gravity, air_density, viscosity /9.81d0, 1.225d0, 1.81e-5/

        ! Initialize settings
        data max_iterations, debug_level, use_fast_mode /1000, 2, .true./

        ! Initialize temperature points
        data temp_points /273.15, 283.15, 293.15, 303.15, 313.15, 323.15, 333.15, 343.15, 353.15, 363.15/

        ! Initialize pressure values
        data pressure_values /1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5/

        ! Initialize coefficient matrix
        data coefficients /1.0, 2.0, 3.0, &
                          4.0, 5.0, 6.0, &
                          7.0, 8.0, 9.0/

        ! Initialize strings
        data model_name, version /'Atmospheric Model   ', 'v1.2.3  '/

    end block data simulation_data
    """,
        )
        result = extract_file_data([Path("/fake/path/blockdata.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
        self.assertEqual(len(file_data["block_data"]), 1)

        block_data = file_data["block_data"]["simulation_data"]

        # Check block data basics
        self.assertEqual(block_data["name"], "simulation_data")
        self.assertEqual(block_data["description"], "")

        # Check common blocks exist
        self.assertIn("physics", block_data["common_blocks"])
        self.assertIn("settings", block_data["common_blocks"])
        self.assertIn("tables", block_data["common_blocks"])
        self.assertIn("strings", block_data["common_blocks"])

        # Check physics common block
        physics = block_data["common_blocks"]["physics"]["variables"]
        self.assertEqual(physics["gravity"]["type"], "real")
        self.assertEqual(physics["gravity"]["kind"], "8")
        self.assertEqual(physics["gravity"]["initial_value"], "9.81d0")

        self.assertEqual(physics["air_density"]["type"], "real")
        self.assertEqual(physics["air_density"]["kind"], "8")
        self.assertEqual(physics["air_density"]["initial_value"], "1.225d0")

        self.assertEqual(physics["viscosity"]["type"], "real")
        self.assertIsNone(physics["viscosity"]["kind"])
        self.assertEqual(physics["viscosity"]["initial_value"], "1.81e-5")

        # Check settings common block
        settings = block_data["common_blocks"]["settings"]["variables"]
        self.assertEqual(settings["max_iterations"]["type"], "integer")
        self.assertEqual(settings["max_iterations"]["initial_value"], "1000")
        self.assertEqual(settings["debug_level"]["type"], "integer")
        self.assertEqual(settings["debug_level"]["initial_value"], "2")
        self.assertEqual(settings["use_fast_mode"]["type"], "logical")
        self.assertEqual(settings["use_fast_mode"]["initial_value"], ".true.")

        # Check tables common block (arrays)
        tables = block_data["common_blocks"]["tables"]["variables"]
        self.assertEqual(tables["temp_points"]["type"], "real")
        dimension = cast(Dimension, tables["temp_points"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
        initial_value = cast(str, tables["temp_points"]["initial_value"])        
        self.assertEqual(len(initial_value.split(",")), 10)

        self.assertEqual(tables["pressure_values"]["type"], "real")
        dimension = cast(Dimension, tables["pressure_values"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
        initial_value = cast(str, tables["temp_points"]["initial_value"])        
        self.assertEqual(len(initial_value.split(",")), 10)

        self.assertEqual(tables["coefficients"]["type"], "real")
        dimension = cast(Dimension, tables["coefficients"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 3))
        initial_value = cast(str, tables["coefficients"]["initial_value"])        
        self.assertEqual(len(initial_value.split(",")), 9)

        # Check strings common block
        strings = block_data["common_blocks"]["strings"]["variables"]
        self.assertEqual(strings["model_name"]["type"], "character")
        self.assertEqual(strings["model_name"]["length"], "20")
        self.assertEqual(strings["model_name"]["initial_value"], "'Atmospheric Model   '")

        self.assertEqual(strings["version"]["type"], "character")
        self.assertEqual(strings["version"]["length"], "8")
        self.assertEqual(strings["version"]["initial_value"], "'v1.2.3  '")

        # Check data_statements and other_variables fields exist
        self.assertIn("data_statements", block_data)
        self.assertIn("other_variables", block_data)


    def test_unnamed_block_data_with_comments(self):
        self.fs.create_file(
            "/fake/path/blockdata.f90",
            contents="""\
!!*
! A test file for block data
!*!
!!*
! Defines some constants
!*!
BLOCK DATA
    !!* Coordinates *!
    COMMON // x, y
    REAL x, y                   ! this comment should be ignored
    ! as should this one
    DATA x, y/1.0, 2.0/
END BLOCK DATA
""",
        )
        result = extract_file_data([Path("/fake/path/blockdata.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
        self.assertEqual(file_data["file_description"], "\nA test file for block data\n\n")
        self.assertEqual(len(file_data["block_data"]), 1)
        block_data = file_data["block_data"]['']
        expected = {
            "name": "",
            "description": "\nDefines some constants\n\n",
            "common_blocks": {
                "": {
                    "name": "",
                    "description": "Coordinates\n",
                    "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None },
                    "variables": {
                        "x": {
                            "description": "Coordinates\n",
                            "type": "real",
                            "name": "x",
                            "dimension": None,
                            "attributes": [],
                            "kind": None,
                            "initial_value": "1.0",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
                        },
                        "y": {
                            "description": "Coordinates\n",
                            "type": "real",
                            "name": "y",
                            "dimension": None,
                            "attributes": [],
                            "kind": None,
                            "initial_value": "2.0",
                            "length": None,
                            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
                        },
                    }
                }
            },
            "data_statements": [{"description": None,
                                 "implied_initialisation": "",
                                 "variable": "x",
                                 "value": "1.0"},
                                {"description": None,
                                 "implied_initialisation": "",
                                 "variable": "y",
                                 "value": "2.0"}],
            "other_variables": {}
        }
        self.assertEqual(block_data, expected)

    def test_complex_block_data_with_comments(self):
        self.fs.create_file("/fake/path/blockdata.f90",
                            contents="""\
    !!* Simulations *!

    !!*
    ! Simulation data for atmospheric modelling
    ! Contains physics constants and simulation parameters
    !*!
    block data simulation_data
        implicit none

        !!* Physical constants and properties used in the simulation *!
        common /physics/ gravity, air_density, viscosity
        !!* Gravitational acceleration and air properties *!
        real*8 gravity, air_density
        real viscosity

        !!* Configuration for simulation execution *!
        common /settings/ max_iterations, debug_level, use_fast_mode
        integer max_iterations, debug_level  ! Regular comment to ignore
        logical use_fast_mode

        !!*
        ! Lookup tables for temperature and pressure calculations
        ! Used for interpolation in the simulation
        !*!
        common /tables/ temp_points, pressure_values, coefficients
        real temp_points(10), pressure_values(10)
        real coefficients(3,3)

        !!* String identifiers for the simulation *!
        common /strings/ model_name, version
        !!* Full model name *!
        character*20 model_name
        !!* Version string *!
        character*8  version

        ! Initialize physics values (regular comment to ignore)
        data gravity, air_density, viscosity /9.81d0, 1.225d0, 1.81e-5/

        ! Initialize settings
        data max_iterations, debug_level, use_fast_mode /1000, 2, .true./

        ! Initialize temperature points
        data temp_points /273.15, 283.15, 293.15, 303.15, 313.15, 323.15, 333.15, 343.15, 353.15, 363.15/

        ! Initialize pressure values
        data pressure_values /1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5/

        ! Initialize coefficient matrix
        data coefficients /1.0, 2.0, 3.0, &
                        4.0, 5.0, 6.0, &
                        7.0, 8.0, 9.0/

        ! Initialize strings
        data model_name, version /'Atmospheric Model   ', 'v1.2.3  '/

    end block data simulation_data
    """,
        )
        result = extract_file_data([Path("/fake/path/blockdata.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
        self.assertEqual(len(file_data["block_data"]), 1)

        block_data = file_data["block_data"]["simulation_data"]

        # Check block data basics
        self.assertEqual(block_data["name"], "simulation_data")
        self.assertEqual(block_data["description"],
                        "\nSimulation data for atmospheric modelling\nContains physics constants and simulation parameters\n\n")

        # Check common blocks exist
        self.assertIn("physics", block_data["common_blocks"])
        self.assertIn("settings", block_data["common_blocks"])
        self.assertIn("tables", block_data["common_blocks"])
        self.assertIn("strings", block_data["common_blocks"])

        # Check physics common block
        physics = block_data["common_blocks"]["physics"]["variables"]
        self.assertEqual(physics["gravity"]["description"], "Gravitational acceleration and air properties\n")
        self.assertEqual(physics["gravity"]["type"], "real")
        self.assertEqual(physics["gravity"]["kind"], "8")
        self.assertEqual(physics["gravity"]["initial_value"], "9.81d0")

        self.assertEqual(physics["air_density"]["description"], "Gravitational acceleration and air properties\n")
        self.assertEqual(physics["air_density"]["type"], "real")
        self.assertEqual(physics["air_density"]["kind"], "8")
        self.assertEqual(physics["air_density"]["initial_value"], "1.225d0")

        self.assertEqual(physics["viscosity"]["description"], "Physical constants and properties used in the simulation\n")
        self.assertEqual(physics["viscosity"]["type"], "real")
        self.assertIsNone(physics["viscosity"]["kind"])
        self.assertEqual(physics["viscosity"]["initial_value"], "1.81e-5")

        # Check settings common block
        settings = block_data["common_blocks"]["settings"]["variables"]
        self.assertEqual(settings["max_iterations"]["description"], "Configuration for simulation execution\n")
        self.assertEqual(settings["max_iterations"]["type"], "integer")
        self.assertEqual(settings["max_iterations"]["initial_value"], "1000")

        self.assertEqual(settings["debug_level"]["description"], "Configuration for simulation execution\n")
        self.assertEqual(settings["debug_level"]["type"], "integer")
        self.assertEqual(settings["debug_level"]["initial_value"], "2")

        self.assertEqual(settings["use_fast_mode"]["description"], "Configuration for simulation execution\n")
        self.assertEqual(settings["use_fast_mode"]["type"], "logical")
        self.assertEqual(settings["use_fast_mode"]["initial_value"], ".true.")

        # Check tables common block (arrays)
        tables = block_data["common_blocks"]["tables"]["variables"]
        expected_tables_description = "\nLookup tables for temperature and pressure calculations\nUsed for interpolation in the simulation\n\n"

        self.assertEqual(tables["temp_points"]["description"], expected_tables_description)
        self.assertEqual(tables["temp_points"]["type"], "real")        
        dimension = cast(Dimension, tables["temp_points"]["dimension"])        
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
        initial_value = cast(str, tables["temp_points"]["initial_value"])        
        self.assertEqual(len(initial_value.split(",")), 10)

        self.assertEqual(tables["pressure_values"]["description"], expected_tables_description)
        self.assertEqual(tables["pressure_values"]["type"], "real")
        dimension = cast(Dimension, tables["pressure_values"]["dimension"])        
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
        initial_value = cast(str, tables["pressure_values"]["initial_value"])        
        self.assertEqual(len(initial_value.split(",")), 10)

        self.assertEqual(tables["coefficients"]["description"], expected_tables_description)
        self.assertEqual(tables["coefficients"]["type"], "real")
        dimension = cast(Dimension, tables["coefficients"]["dimension"])        
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 3))
        initial_value = cast(str, tables["coefficients"]["initial_value"])                
        self.assertEqual(len(initial_value.split(",")), 9)

        # Check strings common block
        strings = block_data["common_blocks"]["strings"]["variables"]
        self.assertEqual(strings["model_name"]["description"], "Full model name\n")
        self.assertEqual(strings["model_name"]["type"], "character")
        self.assertEqual(strings["model_name"]["length"], "20")
        self.assertEqual(strings["model_name"]["initial_value"], "'Atmospheric Model   '")

        self.assertEqual(strings["version"]["description"], "Version string\n")
        self.assertEqual(strings["version"]["type"], "character")
        self.assertEqual(strings["version"]["length"], "8")
        self.assertEqual(strings["version"]["initial_value"], "'v1.2.3  '")

        # Check data_statements and other_variables fields exist
        self.assertIn("data_statements", block_data)
        self.assertIn("other_variables", block_data)


    def test_block_data_edge_cases(self):
        self.fs.create_file("/fake/path/edge_cases.f90",
                            contents="""\
    !!* Empty comment *!
    !!*    Whitespace comment    *!
    !!* 
    ! Multiple
    ! Lines with
    ! Lots of text that goes on and on to make this a very long comment
    ! that might cause issues if not handled properly. We want to make sure
    ! that the code can handle comments of any reasonable length without
    ! any problems. This is a good test of robustness.
    !*!
    
BLOCK DATA  ! Unnamed block data

    ! Common blocks with various types of data
    COMMON /block1/ x1, y1
    COMMON /block2/ x2, y2
    COMMON /arrays/ arr
    COMMON /strings/ str

    ! Variable declarations
    REAL x1, y1, x2, y2
    REAL arr(10)
    CHARACTER*5 str

    ! Initializations
    DATA x1, y1 /1.0, 2.0/
    DATA x2, y2 /3.0, 4.0/
    DATA arr /3*0.0, 2*1.0, 5.0, 4*6.0/
    DATA str /'ABC'/

END BLOCK DATA

!!* Another block data in the same file *!
BLOCK DATA other_data
    COMMON /other/ a, b
    INTEGER a, b
    DATA a, b /1, 2/
END BLOCK DATA other_data

    """,
                            )
        result = extract_file_data([Path("/fake/path/edge_cases.f90")])

        # Check file basics
        self.assertEqual(len(result), 1)
        file_data = result[0]

        # Check that we found both block data units
        self.assertEqual(len(file_data["block_data"]), 2)
        self.assertEqual(file_data["file_description"], "Empty comment\n")

        # Check unnamed block data
        unnamed_block = file_data["block_data"][""]
        self.assertEqual(len(unnamed_block["common_blocks"]), 4)

        self.assertEqual(unnamed_block["description"],
                         "Whitespace comment\n\nMultiple\nLines " +
                         "with\nLots of text that goes on and on to" +
                         " make this a very long comment\nthat might" +
                         " cause issues if not handled properly. We want " +
                         "to make sure\nthat the code can handle comments " +
                         "of any reasonable length without\nany problems."
                         " This is a good test of robustness.\n\n")

        # Check variables in different common blocks
        self.assertIn("block1", unnamed_block["common_blocks"])
        self.assertIn("block2", unnamed_block["common_blocks"])
        self.assertIn("arrays", unnamed_block["common_blocks"])
        self.assertIn("strings", unnamed_block["common_blocks"])

        self.assertIn("x1", unnamed_block["common_blocks"]["block1"]["variables"])
        self.assertIn("x2", unnamed_block["common_blocks"]["block2"]["variables"])
        self.assertIn("arr", unnamed_block["common_blocks"]["arrays"]["variables"])
        self.assertIn("str", unnamed_block["common_blocks"]["strings"]["variables"])

        # Check initializations
        self.assertEqual(
            unnamed_block["common_blocks"]["block1"]["variables"]["x1"]["initial_value"], "1.0")
        self.assertEqual(
            unnamed_block["common_blocks"]["block2"]["variables"]["x2"]["initial_value"], "3.0")
        # check array initialization (has repeats)
        self.assertEqual(unnamed_block["common_blocks"]["arrays"]["variables"]["arr"]["initial_value"],
                         "0.0, 0.0, 0.0, 1.0, 1.0, 5.0, 6.0, 6.0, 6.0, 6.0")
        # Check character initialization
        self.assertEqual(
            unnamed_block["common_blocks"]["strings"]["variables"]["str"]["initial_value"], "'ABC'")

        # Check data_statements and other_variables fields exist
        self.assertIn("data_statements", unnamed_block)
        self.assertIn("other_variables", unnamed_block)

        # Check other block data
        other_block = file_data["block_data"]["other_data"]
        self.assertEqual(other_block["description"],
                         "Another block data in the same file\n")
        self.assertEqual(other_block["common_blocks"]
                         ["other"]["variables"]["a"]["initial_value"], "1")
        self.assertEqual(other_block["common_blocks"]
                         ["other"]["variables"]["b"]["initial_value"], "2")
        
        # Check data_statements and other_variables fields exist
        self.assertIn("data_statements", other_block)
        self.assertIn("other_variables", other_block)


    def test_block_data_array_initialization_styles(self):
        self.fs.create_file("/fake/path/blockdata.f90",
                            contents="""\
BLOCK DATA array_init
    ! Common blocks must be declared first
    COMMON /old_style/ arr1
    COMMON /new_style/ arr2
    COMMON /mixed/ arr3
    COMMON /simple/ arr4
    COMMON /f90_style/ arr5, arr6

    ! Variable declarations
    INTEGER arr1(5)
    INTEGER arr2(6)
    REAL arr3(4)
    INTEGER arr4(3)
    INTEGER arr5(5)
    INTEGER arr6(5)

    ! Initializations must be in DATA statements
    DATA arr1 /1, 2*2, 2*3/
    DATA arr2 /1, 2*2, 3, 2*4/
    DATA arr3 /1.0, 2*2.0, 1.0/
    DATA arr4 /3*1/
    DATA arr5 /1, 2, 3, 4, 5/
    DATA arr6 /2, 4, 6, 8, 10/
    
END BLOCK DATA array_init
    """,
        )
        result = extract_file_data([Path("/fake/path/blockdata.f90")])
        
        # Basic checks
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(len(file_data["block_data"]), 1)
        
        block_data = file_data["block_data"]["array_init"]
        self.assertEqual(len(block_data["common_blocks"]), 5)
        
        # Check old style array constructor
        old_style = block_data["common_blocks"]["old_style"]["variables"]["arr1"]
        self.assertEqual(old_style["type"], "integer")
        self.assertEqual(old_style["initial_value"], "1, 2, 2, 3, 3")
        dimension = cast(Dimension, old_style["dimension"])        
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        
        # Check new style array constructor
        new_style = block_data["common_blocks"]["new_style"]["variables"]["arr2"]
        self.assertEqual(new_style["type"], "integer")
        self.assertEqual(new_style["initial_value"], "1, 2, 2, 3, 4, 4")
        dimension = cast(Dimension, new_style["dimension"])        
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 6))
        
        # Check mixed format
        mixed = block_data["common_blocks"]["mixed"]["variables"]["arr3"]
        self.assertEqual(mixed["type"], "real")
        self.assertEqual(mixed["initial_value"], "1.0, 2.0, 2.0, 1.0")
        dimension = cast(Dimension, mixed["dimension"])        
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 4))
        
        # Check simple repeat
        simple = block_data["common_blocks"]["simple"]["variables"]["arr4"]
        self.assertEqual(simple["type"], "integer")
        self.assertEqual(simple["initial_value"], "1, 1, 1")
        dimension = cast(Dimension, simple["dimension"])        
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        
        # Check F90 sequence constructor
        f90_seq = block_data["common_blocks"]["f90_style"]["variables"]["arr5"]
        self.assertEqual(f90_seq["type"], "integer")
        self.assertEqual(f90_seq["initial_value"], "1, 2, 3, 4, 5")
        dimension = cast(Dimension, f90_seq["dimension"])        
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        
        # Check F90 arithmetic sequence constructor
        f90_arith = block_data["common_blocks"]["f90_style"]["variables"]["arr6"]
        self.assertEqual(f90_arith["type"], "integer")
        self.assertEqual(f90_arith["initial_value"], "2, 4, 6, 8, 10")
        dimension = cast(Dimension, f90_arith["dimension"])        
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        
        # Check data_statements and other_variables fields exist
        self.assertIn("data_statements", block_data)
        self.assertIn("other_variables", block_data)


if __name__ == "__main__":
    unittest.main()