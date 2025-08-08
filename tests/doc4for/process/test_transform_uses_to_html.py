import unittest
from typing import List, Dict
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.process.generate_uses_tree import transform_uses_to_html_references
from doc4for.f90.generate_file_tree import extract_file_data
from doc4for.models.common import UseType, Uses
from doc4for.models.module_models import ModuleDescription
from doc4for.models.file_models import FileDescription

class TestUsesLinkTransformation(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()

    def test_simple_uses(self):
        self.fs.create_file(
            "/fake/path/constants.f90",
            contents="""\
    module constants
    use iso_fortran_env

    real(real64), parameter :: PI = 3.14159
    real(real64), parameter :: E = 2.71828
    real(real64), parameter :: AVOGADROS_NUMBER = 6.022e23
    end module constants
    """)
        self.fs.create_file(
            "/fake/path/maths.f90",
            contents="""\
    module maths
        use iso_fortran_env
        use constants
    end module maths
    """)

        descriptions: List[FileDescription] = extract_file_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/maths.f90"),
            ])
        constants: Dict[str, Uses]
        maths: Dict[str, Uses]
        for file_desc in descriptions:
            if file_desc["file_name"] == "/fake/path/constants.f90":
                constants = list(file_desc["modules"].values())[0]["uses"]
            else:
                maths = list(file_desc["modules"].values())[0]["uses"]

        constants_uses: Dict[str, Uses] = {            
            "iso_fortran_env": {
                "module_name": "iso_fortran_env",
                "selections": [],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE  
            }
        }
        maths_uses: Dict[str, Uses] = {            
            "iso_fortran_env": {
                "module_name": "iso_fortran_env",
                "selections": [],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE  
            },
            "constants": {
                "module_name": "constants",
                "selections": [],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }    
        }
        self.assertEqual(constants, constants_uses)
        self.assertEqual(maths, maths_uses)

        transform_uses_to_html_references(descriptions)
        # Verify that the module references are correctly transformed
        self.assertEqual(len(descriptions), 2)
        for file_desc in descriptions:
            if file_desc["file_name"] == "constants.f90":
                constants = list(file_desc["modules"].values())[0]["uses"]
            else:
                maths = list(file_desc["modules"].values())[0]["uses"]

        constants_uses: Dict[str, Uses] = {            
            "iso_fortran_env": {
                "module_name": "", # not in the list of processed modules, so no link
                "selections": [],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE  
            }
        }
        maths_uses: Dict[str, Uses] = {            
            "iso_fortran_env": {
                "module_name": "", # not in the list of processed modules, so no link
                "selections": [],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE  
            },
            "constants": {
                "module_name": "constants.html",
                "selections": [],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }    
        }
        self.assertEqual(constants, constants_uses)
        self.assertEqual(maths, maths_uses)

    def test_simple_only(self):
        self.fs.create_file(
            "/fake/path/constants.f90",
            contents="""\
    module constants
    use iso_fortran_env

    real(real64), parameter :: PI = 3.14159
    real(real64), parameter :: E = 2.71828
    real(real64), parameter :: AVOGADROS_NUMBER = 6.022e23
    end module constants
    """)
        self.fs.create_file(
            "/fake/path/maths.f90",
            contents="""\
    module maths
        use iso_fortran_env
        use constants, only: PI
    end module maths
    """)

        descriptions: List[FileDescription] = extract_file_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/maths.f90"),
            ])
        
        # Before transformation
        constants: Dict[str, Uses]
        maths: Dict[str, Uses]
        for file_desc in descriptions:
            if file_desc["file_name"] == "/fake/path/constants.f90":
                constants = list(file_desc["modules"].values())[0]["uses"]
            else:
                maths = list(file_desc["modules"].values())[0]["uses"]

        constants_uses: Dict[str, Uses] = {            
            "iso_fortran_env": {
                "module_name": "iso_fortran_env",
                "selections": [],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE  
            }
        }
        maths_uses: Dict[str, Uses] = {            
            "iso_fortran_env": {
                "module_name": "iso_fortran_env",
                "selections": [],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE  
            },
            "constants": {
                "module_name": "constants",
                "selections": ["PI"],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }    
        }
        self.assertEqual(constants, constants_uses)
        self.assertEqual(maths, maths_uses)
        
        transform_uses_to_html_references(descriptions)
        
        # After transformation
        for file_desc in descriptions:
            if file_desc["file_name"] == "constants.f90":
                constants = list(file_desc["modules"].values())[0]["uses"]
            else:
                maths = list(file_desc["modules"].values())[0]["uses"]
        
        constants_uses: Dict[str, Uses] = {            
            "iso_fortran_env": {
                "module_name": "",  # not in the list of processed modules, so no link
                "selections": [],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE  
            }
        }
        maths_uses: Dict[str, Uses] = {            
            "iso_fortran_env": {
                "module_name": "",  # not in the list of processed modules, so no link
                "selections": [],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE  
            },
            "constants": {
                "module_name": "constants.html",
                "selections": ["constants.html#parameter-PI"],  # PI is transformed to a link
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }    
        }
        self.assertEqual(constants, constants_uses)
        self.assertEqual(maths, maths_uses)
            
    def test_uses_with_renames(self):
        self.fs.create_file(
            "/fake/path/constants.f90",
            contents="""\
    module constants
    use iso_fortran_env

    real(real64), parameter :: PI = 3.14159
    real(real64), parameter :: E = 2.71828
    real(real64), parameter :: AVOGADROS_NUMBER = 6.022e23
    end module constants
    """)
        self.fs.create_file(
            "/fake/path/maths.f90",
            contents="""\
    module maths
        use iso_fortran_env, big_real => real64
        use constants, only: PI, AN => AVOGADROS_NUMBER
    end module maths
    """)

        descriptions: List[FileDescription] = extract_file_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/maths.f90"),
            ])
        
        # Before transformation
        constants: Dict[str, Uses]
        maths: Dict[str, Uses]
        for file_desc in descriptions:
            if file_desc["file_name"] == "/fake/path/constants.f90":
                constants = list(file_desc["modules"].values())[0]["uses"]
            else:
                maths = list(file_desc["modules"].values())[0]["uses"]

        constants_uses: Dict[str, Uses] = {            
            "iso_fortran_env": {
                "module_name": "iso_fortran_env",
                "selections": [],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE  
            }
        }
        maths_uses: Dict[str, Uses] = {
            "iso_fortran_env": {
                "module_name": "iso_fortran_env",
                "selections": ["real64"],
                "renames": [{"local": "big_real", "original": "real64"}],
                "description": "",
                "use_type": UseType.NONE  
            },
            "constants": {
                "module_name": "constants",
                "selections": ["PI", "AVOGADROS_NUMBER"],
                "renames": [{"local": "AN", "original": "AVOGADROS_NUMBER"}],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(constants, constants_uses)
        self.assertEqual(maths, maths_uses)
        
        transform_uses_to_html_references(descriptions)
        
        # After transformation
        for file_desc in descriptions:
            if file_desc["file_name"] == "constants.f90":
                constants = list(file_desc["modules"].values())[0]["uses"]
            else:
                maths = list(file_desc["modules"].values())[0]["uses"]

        constants_uses: Dict[str, Uses] = {
            "iso_fortran_env": {
                "module_name": "",  # not in the list of processed modules
                "selections": [],  # no selections
                "renames": [],
                "description": "",
                "use_type": UseType.NONE  
            }
        }
        maths_uses: Dict[str, Uses] = {
            "iso_fortran_env": {
                "module_name": "",  # not in the list of processed modules
                "selections": ["real64"],  # not transformed since module not in descriptions
                "renames": [{"local": "big_real", "original": "real64"}],
                "description": "",
                "use_type": UseType.NONE  
            },
            "constants": {
                "module_name": "constants.html",
                "selections": ["constants.html#parameter-PI", "constants.html#parameter-AVOGADROS_NUMBER"],
                "renames": [{"local": "AN", "original": "AVOGADROS_NUMBER"}],
                "description": "",
                "use_type": UseType.NONE
            }    
        }
        self.assertEqual(constants, constants_uses)
        self.assertEqual(maths, maths_uses)
        
    def test_external_module(self):
        self.fs.create_file(
            "/fake/path/maths.f90",
            contents="""\
    module maths
        use external_module
    end module maths
    """)

        descriptions: List[FileDescription] = extract_file_data([
            Path("/fake/path/maths.f90"),
            ])
        
        maths: Dict[str, Uses]
        for file_desc in descriptions:
            maths = list(file_desc["modules"].values())[0]["uses"]
        
        maths_uses: Dict[str, Uses] = {
            "external_module": {
                "module_name": "external_module",
                "selections": [],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(maths, maths_uses)
        
        transform_uses_to_html_references(descriptions)
        
        # After transformation
        for file_desc in descriptions:
            maths = list(file_desc["modules"].values())[0]["uses"]
        
        maths_uses: Dict[str, Uses] = {
            "external_module": {
                "module_name": "",  # external module not in descriptions, so empty string
                "selections": [],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(maths, maths_uses)

    def test_block_data_uses(self):
        """Test use statements in block data units"""
        self.fs.create_file(
            "/fake/path/constants.f90",
            contents="""\
    module constants
        real, parameter :: PI = 3.14159
        real, parameter :: E = 2.71828
    end module constants
    """)
        self.fs.create_file(
            "/fake/path/shared_data.f90",
            contents="""\
    block data shared_data
        use constants, only: PI
        common /math_consts/ pi_value
        real :: pi_value
        data pi_value /3.14159/
    end block data shared_data
    """)

        descriptions: List[FileDescription] = extract_file_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/shared_data.f90"),
        ])
        
        # Check block data uses before transformation
        shared_file = [d for d in descriptions if d["file_name"] == "/fake/path/shared_data.f90"][0]
        block_data = shared_file["block_data"]["shared_data"]
        
        expected_uses = {
            "constants": {
                "module_name": "constants",
                "selections": ["PI"],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(block_data["uses"], expected_uses)
        
        # After transformation
        transform_uses_to_html_references(descriptions)
        
        expected_uses_after = {
            "constants": {
                "module_name": "constants.html",
                "selections": ["constants.html#parameter-PI"],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(block_data["uses"], expected_uses_after)

    def test_program_uses(self):
        """Test use statements in main programs"""
        self.fs.create_file(
            "/fake/path/utils.f90",
            contents="""\
    module utils
        contains
        subroutine print_message(msg)
            character(*), intent(in) :: msg
            print *, msg
        end subroutine print_message
    end module utils
    """)
        self.fs.create_file(
            "/fake/path/main.f90",
            contents="""\
    program main
        use utils, only: print_message
        use iso_fortran_env, only: error_unit
        
        call print_message("Hello, World!")
    end program main
    """)

        descriptions: List[FileDescription] = extract_file_data([
            Path("/fake/path/utils.f90"),
            Path("/fake/path/main.f90"),
        ])
        
        # Check program uses before transformation
        main_file = [d for d in descriptions if d["file_name"] == "/fake/path/main.f90"][0]
        program = main_file["programs"]["main"]
        
        expected_uses = {
            "utils": {
                "module_name": "utils",
                "selections": ["print_message"],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            },
            "iso_fortran_env": {
                "module_name": "iso_fortran_env",
                "selections": ["error_unit"],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(program["uses"], expected_uses)
        
        # After transformation
        transform_uses_to_html_references(descriptions)
        
        expected_uses_after = {
            "utils": {
                "module_name": "utils.html",
                "selections": ["utils.html#subroutine-print_message"],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            },
            "iso_fortran_env": {
                "module_name": "",
                "selections": ["error_unit"],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(program["uses"], expected_uses_after)

    def test_function_uses(self):
        """Test use statements inside functions with various patterns"""
        self.fs.create_file(
            "/fake/path/math_constants.f90",
            contents="""\
    module math_constants
        real, parameter :: PI = 3.14159
        real, parameter :: TWO_PI = 6.28318
        real, parameter :: E = 2.71828
    end module math_constants
    """)
        self.fs.create_file(
            "/fake/path/geometry.f90",
            contents="""\
    module geometry
        contains
        
        real function circle_area(radius)
            use math_constants, only: PI
            real, intent(in) :: radius
            circle_area = PI * radius * radius
        end function circle_area
        
        real function circle_circumference(radius)
            use math_constants, TAU => TWO_PI
            real, intent(in) :: radius
            circle_circumference = TAU * radius
        end function circle_circumference
        
        real function exponential_growth(x)
            use math_constants, only: EULER => E
            real, intent(in) :: x
            exponential_growth = EULER ** x
        end function exponential_growth
        
    end module geometry
    """)

        descriptions: List[FileDescription] = extract_file_data([
            Path("/fake/path/math_constants.f90"),
            Path("/fake/path/geometry.f90"),
        ])
        
        # Check function uses before transformation
        geometry_file = [d for d in descriptions if d["file_name"] == "/fake/path/geometry.f90"][0]
        geometry = geometry_file["modules"]["geometry"]
        
        # Test only clause
        circle_area_func = geometry["functions"]["circle_area"]
        expected_uses_area = {
            "math_constants": {
                "module_name": "math_constants",
                "selections": ["PI"],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(circle_area_func["uses"], expected_uses_area)
        
        # Test rename without only
        circle_circumference_func = geometry["functions"]["circle_circumference"]
        expected_uses_circumference = {
            "math_constants": {
                "module_name": "math_constants",
                "selections": ["TWO_PI"],
                "renames": [{"local": "TAU", "original": "TWO_PI"}],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(circle_circumference_func["uses"], expected_uses_circumference)
        
        # Test only with rename
        exponential_growth_func = geometry["functions"]["exponential_growth"]
        expected_uses_exponential = {
            "math_constants": {
                "module_name": "math_constants",
                "selections": ["E"],
                "renames": [{"local": "EULER", "original": "E"}],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(exponential_growth_func["uses"], expected_uses_exponential)
        
        # After transformation
        transform_uses_to_html_references(descriptions)
        
        # Check transformations
        expected_uses_area_after = {
            "math_constants": {
                "module_name": "math_constants.html",
                "selections": ["math_constants.html#parameter-PI"],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(circle_area_func["uses"], expected_uses_area_after)
        
        expected_uses_circumference_after = {
            "math_constants": {
                "module_name": "math_constants.html",
                "selections": ["math_constants.html#parameter-TWO_PI"],
                "renames": [{"local": "TAU", "original": "TWO_PI"}],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(circle_circumference_func["uses"], expected_uses_circumference_after)
        
        expected_uses_exponential_after = {
            "math_constants": {
                "module_name": "math_constants.html",
                "selections": ["math_constants.html#parameter-E"],
                "renames": [{"local": "EULER", "original": "E"}],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(exponential_growth_func["uses"], expected_uses_exponential_after)


    def test_subroutine_uses(self):
        """Test use statements inside subroutines with various patterns"""
        self.fs.create_file(
            "/fake/path/io_utils.f90",
            contents="""\
    module io_utils
        integer, parameter :: DEFAULT_UNIT = 6
        integer, parameter :: ERROR_UNIT = 0
        integer, parameter :: LOG_UNIT = 99
        
        contains
        
        subroutine write_formatted(msg)
            character(*), intent(in) :: msg
            write(DEFAULT_UNIT, '(A)') msg
        end subroutine write_formatted
        
        subroutine write_error(msg)
            character(*), intent(in) :: msg
            write(ERROR_UNIT, '(A)') "ERROR: " // msg
        end subroutine write_error
        
    end module io_utils
    """)
        self.fs.create_file(
            "/fake/path/logging.f90",
            contents="""\
    module logging
        contains
        
        subroutine log_error(error_msg)
            use iso_fortran_env, only: error_unit
            use io_utils, only: write_formatted
            character(*), intent(in) :: error_msg
            
            write(error_unit, *) "ERROR: ", error_msg
            call write_formatted("Error logged")
        end subroutine log_error
        
        subroutine log_info(info_msg)
            use io_utils, stdout => DEFAULT_UNIT, log => LOG_UNIT
            character(*), intent(in) :: info_msg
            
            write(stdout, *) "INFO: ", info_msg
            write(log, *) "Logged: ", info_msg
        end subroutine log_info
        
        subroutine log_warning(warn_msg)
            use io_utils, only: write_err => write_error, write_fmt => write_formatted
            character(*), intent(in) :: warn_msg
            
            call write_err("Warning: " // warn_msg)
            call write_fmt("Warning logged")
        end subroutine log_warning
        
    end module logging
    """)

        descriptions: List[FileDescription] = extract_file_data([
            Path("/fake/path/io_utils.f90"),
            Path("/fake/path/logging.f90"),
        ])
        
        # Check subroutine uses before transformation
        logging_file = [d for d in descriptions if d["file_name"] == "/fake/path/logging.f90"][0]
        logging = logging_file["modules"]["logging"]
        
        # Test multiple uses with only
        log_error_sub = logging["subroutines"]["log_error"]
        expected_uses_error = {
            "iso_fortran_env": {
                "module_name": "iso_fortran_env",
                "selections": ["error_unit"],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            },
            "io_utils": {
                "module_name": "io_utils",
                "selections": ["write_formatted"],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(log_error_sub["uses"], expected_uses_error)
        
        # Test multiple renames without only
        log_info_sub = logging["subroutines"]["log_info"]
        expected_uses_info = {
            "io_utils": {
                "module_name": "io_utils",
                "selections": ["DEFAULT_UNIT", "LOG_UNIT"],
                "renames": [
                    {"local": "stdout", "original": "DEFAULT_UNIT"},
                    {"local": "log", "original": "LOG_UNIT"}
                ],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(log_info_sub["uses"], expected_uses_info)
        
        # Test only with multiple renames
        log_warning_sub = logging["subroutines"]["log_warning"]
        expected_uses_warning = {
            "io_utils": {
                "module_name": "io_utils",
                "selections": ["write_error", "write_formatted"],
                "renames": [
                    {"local": "write_err", "original": "write_error"},
                    {"local": "write_fmt", "original": "write_formatted"}
                ],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(log_warning_sub["uses"], expected_uses_warning)
        
        # After transformation
        transform_uses_to_html_references(descriptions)
        
        # Check transformations
        expected_uses_error_after = {
            "iso_fortran_env": {
                "module_name": "",
                "selections": ["error_unit"],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            },
            "io_utils": {
                "module_name": "io_utils.html",
                "selections": ["io_utils.html#subroutine-write_formatted"],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(log_error_sub["uses"], expected_uses_error_after)
        
        expected_uses_info_after = {
            "io_utils": {
                "module_name": "io_utils.html",
                "selections": [
                    "io_utils.html#parameter-DEFAULT_UNIT",
                    "io_utils.html#parameter-LOG_UNIT"
                ],
                "renames": [
                    {"local": "stdout", "original": "DEFAULT_UNIT"},
                    {"local": "log", "original": "LOG_UNIT"}
                ],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(log_info_sub["uses"], expected_uses_info_after)
        
        expected_uses_warning_after = {
            "io_utils": {
                "module_name": "io_utils.html",
                "selections": [
                    "io_utils.html#subroutine-write_error",
                    "io_utils.html#subroutine-write_formatted"
                ],
                "renames": [
                    {"local": "write_err", "original": "write_error"},
                    {"local": "write_fmt", "original": "write_formatted"}
                ],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(log_warning_sub["uses"], expected_uses_warning_after)


    def test_standalone_function_uses(self):
        """Test use statements in standalone functions (outside modules)"""
        self.fs.create_file(
            "/fake/path/constants.f90",
            contents="""\
    module constants
        real, parameter :: GRAVITY = 9.81
        real, parameter :: AIR_DENSITY = 1.225
    end module constants
    """)
        self.fs.create_file(
            "/fake/path/physics.f90",
            contents="""\
    real function calculate_drag(velocity, area)
        use constants, only: rho => AIR_DENSITY
        real, intent(in) :: velocity, area
        real, parameter :: Cd = 0.47  ! drag coefficient for sphere
        
        calculate_drag = 0.5 * rho * velocity**2 * area * Cd
    end function calculate_drag

    real function free_fall_velocity(time)
        use constants, only: GRAVITY
        real, intent(in) :: time
        
        free_fall_velocity = GRAVITY * time
    end function free_fall_velocity
    """)

        descriptions: List[FileDescription] = extract_file_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/physics.f90"),
        ])
        
        physics_file = [d for d in descriptions if d["file_name"] == "/fake/path/physics.f90"][0]
        
        # Check standalone function uses before transformation
        drag_func = physics_file["functions"]["calculate_drag"]
        expected_uses_drag = {
            "constants": {
                "module_name": "constants",
                "selections": ["AIR_DENSITY"],
                "renames": [{"local": "rho", "original": "AIR_DENSITY"}],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(drag_func["uses"], expected_uses_drag)
        
        fall_func = physics_file["functions"]["free_fall_velocity"]
        expected_uses_fall = {
            "constants": {
                "module_name": "constants",
                "selections": ["GRAVITY"],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(fall_func["uses"], expected_uses_fall)
        
        # After transformation
        transform_uses_to_html_references(descriptions)
        
        expected_uses_drag_after = {
            "constants": {
                "module_name": "constants.html",
                "selections": ["constants.html#parameter-AIR_DENSITY"],
                "renames": [{"local": "rho", "original": "AIR_DENSITY"}],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(drag_func["uses"], expected_uses_drag_after)
        
        expected_uses_fall_after = {
            "constants": {
                "module_name": "constants.html",
                "selections": ["constants.html#parameter-GRAVITY"],
                "renames": [],
                "description": "",
                "use_type": UseType.NONE
            }
        }
        self.assertEqual(fall_func["uses"], expected_uses_fall_after)
        
if __name__ == "__main__":
    unittest.main()