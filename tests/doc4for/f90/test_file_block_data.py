import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data


class TestBlockData(TestCase):

    def setUp(self):
        self.setUpPyfakefs()

#TODO out of order variables and common blocks
    def test_unnamed_block_data_no_comments(self):
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
                "basic": {
                    "x": {
                        "description": "",
                        "type": "real",
                        "name": "x",
                        "dimension": None,
                        "attributes": [],
                        "kind": None,
                        "initial_value": "1.0",
                    },
                    "y": {
                        "description": "",
                        "type": "real",
                        "name": "y",
                        "dimension": None,
                        "attributes": [],
                        "kind": None,
                        "initial_value": "2.0",
                    },
                }
            },
        }
        self.assertEqual(block_data, expected)

#     def test_simple_block_data_no_comments(self):
#         self.fs.create_file(
#             "/fake/path/blockdata.f90",
#             contents="""\
# BLOCK DATA simple
#     COMMON /basic/ x, y
#     REAL x, y
#     DATA x, y/1.0, 2.0/
# END BLOCK DATA simple
# """,
#         )
#         result = extract_file_data([Path("/fake/path/blockdata.f90")])
#         self.assertEqual(len(result), 1)
#         file_data = result[0]
#         self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
#         self.assertEqual(len(file_data["block_data"]), 1)
#         block_data = file_data["block_data"]['simple']
#         expected = {
#             "name": "simple",
#             "description": "",
#             "common_blocks": {
#                 "basic": {
#                     "x": {
#                         "description": "",
#                         "type": "real",
#                         "name": "x",
#                         "dimension": None,
#                         "attributes": [],
#                         "kind": None,
#                         "initial_value": "1.0",
#                     },
#                     "y": {
#                         "description": "",
#                         "type": "real",
#                         "name": "y",
#                         "dimension": None,
#                         "attributes": [],
#                         "kind": None,
#                         "initial_value": "2.0",
#                     },
#                 }
#             },
#         }
#         self.assertEqual(block_data, expected)


#     def test_simple_block_data_comments(self):
#         self.fs.create_file(
#             "/fake/path/blockdata.f90",
#             contents="""\
# BLOCK DATA simple
#     COMMON /basic/ x, y
#     REAL x, y
#     DATA x, y/1.0, 2.0/
# END BLOCK DATA simple
# """,
#         )
#         result = extract_file_data([Path("/fake/path/blockdata.f90")])
#         self.assertEqual(len(result), 1)
#         file_data = result[0]
#         self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
#         self.assertEqual(len(file_data["block_data"]), 1)

#     def test_block_data_no_comments(self):
#         self.fs.create_file(
#             "/fake/path/blockdata.f90",
#             contents="""\
# BLOCK DATA constants_init
#     COMMON /math/ pi, e
#     COMMON /phys/ g, c

#     REAL pi, e     ! Mathematical constants
#     REAL g, c      ! Physical constants

#     ! Initialize mathematical constants
#     DATA pi/3.141592654/, e/2.718281828/

#     ! Initialize physical constants
#     DATA g/9.81/, c/299792458.0/
# END BLOCK DATA constants_init
# """,
#         )
#         result = extract_file_data([Path("/fake/path/blockdata.f90")])
#         self.assertEqual(len(result), 1)
#         file_data = result[0]
#         self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
#         self.assertEqual(len(file_data["block_data"]), 1)

#     def test_block_data_comments(self):
#         self.fs.create_file(
#             "/fake/path/blockdata.f90",
#             contents="""\
# BLOCK DATA constants_init
#     COMMON /math/ pi, e
#     COMMON /phys/ g, c

#     REAL pi, e     ! Mathematical constants
#     REAL g, c      ! Physical constants

#     ! Initialize mathematical constants
#     DATA pi/3.141592654/, e/2.718281828/

#     ! Initialize physical constants
#     DATA g/9.81/, c/299792458.0/
# END BLOCK DATA constants_init
# """,
#         )
#         result = extract_file_data([Path("/fake/path/blockdata.f90")])
#         self.assertEqual(len(result), 1)
#         file_data = result[0]
#         self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
#         self.assertEqual(len(file_data["block_data"]), 1)

#     def test_complex_block_data_no_comments(self):
#         self.fs.create_file(
#             "/fake/path/blockdata.f90",
#             contents="""\
# block data simulation_data
#     implicit none

#     ! Physical parameters
#     common /physics/ gravity, air_density, viscosity
#     real*8 gravity, air_density
#     real viscosity

#     ! Simulation settings
#     common /settings/ max_iterations, debug_level, use_fast_mode
#     integer max_iterations, debug_level
#     logical use_fast_mode

#     ! Lookup table data
#     common /tables/ temp_points, pressure_values, coefficients
#     real temp_points(10), pressure_values(10)
#     real coefficients(3,3)

#     ! Character data
#     common /strings/ model_name, version
#     character*20 model_name
#     character*8  version

#     ! Initialize physics values
#     data gravity, air_density, viscosity /9.81d0, 1.225d0, 1.81e-5/

#     ! Initialize settings
#     data max_iterations, debug_level, use_fast_mode /1000, 2, .true./

#     ! Initialize temperature points
#     data temp_points /273.15, 283.15, 293.15, 303.15, 313.15,
#                      323.15, 333.15, 343.15, 353.15, 363.15/

#     ! Initialize pressure values
#     data pressure_values /1.0, 1.5, 2.0, 2.5, 3.0,
#                          3.5, 4.0, 4.5, 5.0, 5.5/

#     ! Initialize coefficient matrix
#     data coefficients /1.0, 2.0, 3.0,
#                       4.0, 5.0, 6.0,
#                       7.0, 8.0, 9.0/

#     ! Initialize strings
#     data model_name, version /'Atmospheric Model   ', 'v1.2.3  '/

# end block data simulation_data
# """,
#         )
#         result = extract_file_data([Path("/fake/path/blockdata.f90")])
#         self.assertEqual(len(result), 1)
#         file_data = result[0]
#         self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
#         self.assertEqual(len(file_data["block_data"]), 1)

#     def test_complex_block_data_comments(self):
#         self.fs.create_file(
#             "/fake/path/blockdata.f90",
#             contents="""\
# block data simulation_data
#     implicit none

#     ! Physical parameters
#     common /physics/ gravity, air_density, viscosity
#     real*8 gravity, air_density
#     real viscosity

#     ! Simulation settings
#     common /settings/ max_iterations, debug_level, use_fast_mode
#     integer max_iterations, debug_level
#     logical use_fast_mode

#     ! Lookup table data
#     common /tables/ temp_points, pressure_values, coefficients
#     real temp_points(10), pressure_values(10)
#     real coefficients(3,3)

#     ! Character data
#     common /strings/ model_name, version
#     character*20 model_name
#     character*8  version

#     ! Initialize physics values
#     data gravity, air_density, viscosity /9.81d0, 1.225d0, 1.81e-5/

#     ! Initialize settings
#     data max_iterations, debug_level, use_fast_mode /1000, 2, .true./

#     ! Initialize temperature points
#     data temp_points /273.15, 283.15, 293.15, 303.15, 313.15,
#                      323.15, 333.15, 343.15, 353.15, 363.15/

#     ! Initialize pressure values
#     data pressure_values /1.0, 1.5, 2.0, 2.5, 3.0,
#                          3.5, 4.0, 4.5, 5.0, 5.5/

#     ! Initialize coefficient matrix
#     data coefficients /1.0, 2.0, 3.0,
#                       4.0, 5.0, 6.0,
#                       7.0, 8.0, 9.0/

#     ! Initialize strings
#     data model_name, version /'Atmospheric Model   ', 'v1.2.3  '/

# end block data simulation_data
# """,
#         )
#         result = extract_file_data([Path("/fake/path/blockdata.f90")])
#         self.assertEqual(len(result), 1)
#         file_data = result[0]
#         self.assertEqual(file_data["file_name"], "/fake/path/blockdata.f90")
#         self.assertEqual(len(file_data["block_data"]), 1)

if __name__ == "__main__":
    unittest.main()
