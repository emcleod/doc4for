import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.variable_models import PolymorphismType

class TestFunctionAnnotations(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_in_annotation_name_match(self):
        self.fs.create_file(
            "/fake/path/in_annotation.f90",
            contents="""\
    module in_annotation_module
    contains
    !!*
    ! A function with @in annotation
    ! @in x
    ! @in y
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation

    end module in_annotation_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/in_annotation.f90")])

        module = result[0]
        function = module["functions"]["test_in_annotation"]
        self.assertEqual(function["description"], "\nA function with @in annotation\n\n")
        inputs = function["in"]
        self.assertEqual(inputs["x"], {"type": "REAL", "description": "", "dimension": None, "enum_type": None, "interface_name": None})
        self.assertEqual(inputs["y"], {"type": "REAL", "description": "", "dimension": None, "enum_type": None, "interface_name": None})

    def test_in_annotation_description(self):
        self.fs.create_file(
            "/fake/path/in_annotation.f90",
            contents="""\
    module in_annotation_module
    contains
    !!*
    ! A function with @in annotation
    ! @in x  The first argument
    ! @in y
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation

    end module in_annotation_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/in_annotation.f90")])

        module = result[0]
        function = module["functions"]["test_in_annotation"]
        self.assertEqual(function["description"], "\nA function with @in annotation\n\n")
        inputs = function["in"]
        self.assertEqual(inputs["x"], {"type": "REAL", "description": "The first argument", "dimension": None, "enum_type": None, "interface_name": None})
        self.assertEqual(inputs["y"], {"type": "REAL", "description": "", "dimension": None, "enum_type": None, "interface_name": None})

    def test_missing_description_warning(self):
        self.fs.create_file("/fake/path/test.f90", contents="""
        module test_module
        contains
        !!*
        ! @in x
        !*!
        function test_func(x) result(res)
            real, intent(in) :: x
            real :: res
        end function
        end module
        """)
        
        with self.assertLogs("doc4for.parse.procedure_parser", level="WARNING") as cm:
            extract_module_data([Path("/fake/path/test.f90")])
            self.assertIn("Warning: No description provided for argument 'x'", cm.output[0])

    def test_in_annotation_name_mismatch(self):
        self.fs.create_file(
            "/fake/path/in_annotation.f90",
            contents="""\
    module in_annotation_module
    contains
    !!*
    ! A function with @in annotation
    ! @in z
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x
        integer, intent(in) :: y
        real :: res
        res = x + y
    end function test_in_annotation
    end module in_annotation_module
                            """,
        )

        with self.assertLogs("doc4for.parse.procedure_parser", level="WARNING") as cm:
            result = extract_module_data([Path("/fake/path/in_annotation.f90")])

            module = result[0]
            function = module["functions"]["test_in_annotation"]
            inputs = function["in"]
            self.assertEqual(inputs["x"], {"type": "REAL", "description": "", "dimension": None, "enum_type": None, "interface_name": None})
            self.assertEqual(inputs["y"], {"type": "INTEGER", "description": "", "dimension": None, "enum_type": None, "interface_name": None})
            self.assertNotIn("z", inputs)
            # check that there's a warning for wrong variable name in comment
            self.assertIn("Warning: 'in' annotation 'z' not found in arguments [['x', 'y']]", cm.output[0])


    def test_out_annotation_name_match(self):
        self.fs.create_file(
            "/fake/path/out_annotation.f90",
            contents="""\
    module out_annotation_module
    contains
    !!*
    ! A function with @out annotation
    ! @out res
    !*!
    function test_out_annotation(x, y) result(res)
        real, intent(in) :: x
        real, intent(out) :: y
        real :: res
        res = x + y
    end function test_out_annotation
    end module out_annotation_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/out_annotation.f90")])

        module = result[0]
        function = module["functions"]["test_out_annotation"]
        self.assertEqual(function["description"], "\nA function with @out annotation\n\n")
        inputs = function["in"]
        self.assertEqual(inputs["x"], {"type": "REAL", "description": "", "dimension": None, "interface_name": None, "enum_type": None})
        outputs = function["out"]
        self.assertEqual(outputs["y"], {"type": "REAL", "description": "", "dimension": None, "interface_name": None, "enum_type": None})

    def test_out_annotation_name_mismatch(self):
        self.fs.create_file(
            "/fake/path/out_annotation.f90",
            contents="""\
    module out_annotation_module
    contains
    !!*
    ! A function with @out annotation
    ! @out z
    !*!
    function test_out_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_out_annotation
    end module out_annotation_module
                            """,
        )

        with self.assertLogs("doc4for.parse.procedure_parser", level="WARNING") as cm:
            result = extract_module_data([Path("/fake/path/out_annotation.f90")])

            module = result[0]
            function = module["functions"]["test_out_annotation"]
            inputs = function["in"]
            self.assertEqual(inputs["x"], {"type": "REAL", "description": "", "dimension": None, "enum_type": None, "interface_name": None})
            self.assertEqual(inputs["y"], {"type": "REAL", "description": "", "dimension": None, "enum_type": None, "interface_name": None})
            self.assertNotIn("z", inputs)
            # check that there's a warning for wrong variable name in comment
            self.assertIn("Warning: 'out' annotation 'z' not found in arguments [[]]", cm.output[0])

    def test_inout_annotation_name_match(self):
        self.fs.create_file(
            "/fake/path/inout_annotation.f90",
            contents="""\
    module inout_annotation_module
    contains
    !!*
    ! A function with @inout annotation
    ! @inout x
    !*!
    function test_inout_annotation(x, y) result(res)
        real, intent(inout) :: x
        real, intent(in) :: y
        real :: res
        res = x + y
    end function test_inout_annotation
    end module inout_annotation_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/inout_annotation.f90")])

        module = result[0]
        function = module["functions"]["test_inout_annotation"]
        self.assertEqual(function["description"], "\nA function with @inout annotation\n\n")
        inputs = function["in"]
        outputs = function["out"]
        self.assertEqual(inputs["x"], {"type": "REAL", "description": "", "dimension": None, "interface_name": None, "enum_type": None})
        self.assertEqual(inputs["y"], {"type": "REAL", "description": "", "dimension": None, "interface_name": None, "enum_type": None})
        self.assertEqual(outputs["x"], {"type": "REAL", "description": "", "dimension": None, "interface_name": None, "enum_type": None})

    def test_inout_annotation_name_mismatch(self):
        self.fs.create_file(
            "/fake/path/inout_annotation.f90",
            contents="""\
    module inout_annotation_module
    contains
    !!*
    ! A function with @inout annotation
    ! @inout z
    !*!
    function test_inout_annotation(x, y) result(res)
        real, intent(inout) :: x
        real, intent(inout) :: y
        real :: res
        res = x + y
    end function test_inout_annotation
    end module inout_annotation_module
                            """,
        )

        with self.assertLogs("doc4for.parse.procedure_parser", level="WARNING") as cm:
            result = extract_module_data([Path("/fake/path/inout_annotation.f90")])

            module = result[0]
            function = module["functions"]["test_inout_annotation"]
            inputs = function["in"]
            outputs = function["out"]
            self.assertIn("x", inputs)
            self.assertIn("y", inputs)
            self.assertNotIn("z", inputs)
            self.assertIn("x", outputs)
            self.assertIn("y", outputs)
            self.assertNotIn("z", outputs)

            # check that there's a warning for wrong variable name in comment
            self.assertIn("Warning: 'inout' annotation 'z' not found in arguments [['x', 'y'], ['x', 'y']]", cm.output[0])

    def test_return_type_with_description(self):
        self.fs.create_file(
            "/fake/path/return_type.f90",
            contents="""\
    module return_type_module
    contains
    !!*
    ! A function with return type
    ! @return  The result
    !*!
    function test_return_type(x, y) result(res)
        real, intent(in) :: x, y
        integer :: res
        res = int(x + y)
    end function test_return_type
    end module return_type_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/return_type.f90")])

        module = result[0]
        function = module["functions"]["test_return_type"]
        self.assertEqual(function["description"], "\nA function with return type\n\n")
        inputs = function["in"]
        self.assertEqual(inputs["x"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "enum_type": None, 
                                       "interface_name": None
                                       })
        self.assertEqual(inputs["y"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "enum_type": None, 
                                       "interface_name": None
                                       })
        results = function["return"]
        self.assertEqual(results, {"type": "INTEGER", 
                                   "description": "The result", 
                                   "dimension": None, 
                                   "interface_name": None, 
                                   "enum_type": None
                                   })

    def test_no_description_for_return(self):
        self.fs.create_file(
            "/fake/path/return_type.f90",
            contents="""\
    module return_type_module
    contains
    !!*
    ! A function with return type
    ! @return
    !*!
    function test_return_type(x, y) result(res)
        real, intent(in) :: x, y
        integer :: res
        res = int(x + y)
    end function test_return_type
    end module return_type_module
                            """,
        )
        with self.assertLogs("doc4for.parse.procedure_parser", level="WARNING") as cm:
            extract_module_data([Path("/fake/path/return_type.f90")])
            self.assertIn("Warning: No description provided for 'return' annotation", cm.output[0])

    def test_no_return_annotation(self):
        self.fs.create_file(
            "/fake/path/return_type.f90",
            contents="""\
    module return_type_module
    contains
    !!*
    ! A function with return type
    !*!
    function test_return_type(x, y) result(res)
        real, intent(in) :: x, y
        integer :: res
        res = int(x + y)
    end function test_return_type
    end module return_type_module
                            """,
        )
        with self.assertLogs("doc4for.parse.procedure_parser", level="WARNING") as cm:
            extract_module_data([Path("/fake/path/return_type.f90")])
            self.assertIn("Warning: no annotation for return", cm.output[0])

    def test_fully_annotated_function(self):
        self.fs.create_file(
            "/fake/path/annotation.f90",
            contents="""\
    module annotation_module
    contains
    !!*
    ! A function with all annotations.
    ! @in x The first variable
    ! @out y The second
    ! variable
    ! @inout z  The final variable
    ! @return The result
    !*!
    function test_all_annotations(x, y, z) result(res)
        real, intent(in) :: x
        real, intent(out) :: y
        integer, intent(inout) :: z
        real :: res
        res = x + y
    end function test_all_annotations

    end module annotation_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/annotation.f90")])

        module = result[0]
        function = module["functions"]["test_all_annotations"]
        self.assertEqual(function["description"], "\nA function with all annotations.\n\n\n")
        inputs = function["in"]
        outputs = function["out"]
        self.assertEqual(inputs["x"], {"type": "REAL", "description": "The first variable", "dimension": None, "enum_type": None, "interface_name": None})
        self.assertEqual(inputs["z"], {"type": "INTEGER", "description": "The final variable", "dimension": None, "enum_type": None, "interface_name": None})
        self.assertEqual(outputs["y"], {"type": "REAL", "description": "The second variable", "dimension": None, "enum_type": None, "interface_name": None})
        self.assertEqual(outputs["z"], {"type": "INTEGER", "description": "The final variable", "dimension": None, "enum_type": None, "interface_name": None})
        self.assertEqual(function["return"], {"type": "REAL", "description": "The result", "dimension": None, "interface_name": None, "enum_type": None})

if __name__ == "__main__":
    unittest.main()
