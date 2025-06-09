import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.variable_models import PolymorphismType

class TestSubroutineAnnotations(TestCase):

    def setUp(self):
        self.setUpPyfakefs()

    def test_subroutine_in_annotation_name_match(self):
        self.fs.create_file(
            "/fake/path/subroutine_in_annotation.f90",
            contents="""\
    module subroutine_in_annotation_module
    contains
    !!*
    ! A subroutine with @in annotation
    ! @in x  
    ! @in y  
    !*!
    subroutine test_subroutine_in_annotation(x, y)
        real, intent(in) :: x, y
        print *, x + y
    end subroutine test_subroutine_in_annotation
    end module subroutine_in_annotation_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/subroutine_in_annotation.f90")])

        module = result[0]
        subroutine = module["subroutines"]["test_subroutine_in_annotation"]
        self.assertEqual(subroutine["description"], "A subroutine with @in annotation\n\n")
        inputs = subroutine["in"]
        self.assertEqual(inputs["x"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "default_value": None,
                                       "attributes": [],
                                       "kind": None,
                                       "length": None,
                                       "polymorphism_type": PolymorphismType.NONE
                                       })
        self.assertEqual(inputs["y"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "default_value": None,
                                       "attributes": [],
                                       "kind": None,
                                       "length": None,
                                       "polymorphism_type": PolymorphismType.NONE
                                       })

    def test_subroutine_in_annotation_name_mismatch(self):
        self.fs.create_file(
            "/fake/path/subroutine_in_annotation.f90",
            contents="""\
    module subroutine_in_annotation_module
    contains
    !!*
    ! A subroutine with @in annotation
    ! @in z
    !*!
    subroutine test_subroutine_in_annotation(x, y)
        real, intent(in) :: x, y
        print *, x + y
    end subroutine test_subroutine_in_annotation
    end module subroutine_in_annotation_module
                            """,
        )
        with self.assertLogs("doc4for.parse.procedure_parser", level="WARNING") as cm:
            result = extract_module_data([Path("/fake/path/subroutine_in_annotation.f90")])

            module = result[0]
            subroutine = module["subroutines"]["test_subroutine_in_annotation"]
            inputs = subroutine["in"]
            self.assertEqual(inputs["x"], {"type": "REAL", 
                                           "description": "", 
                                           "dimension": None, 
                                           "interface_name": None, 
                                           "enum_type": None,
                                           "default_value": None,
                                            "attributes": [],
                                            "kind": None,
                                            "length": None,
                                            "polymorphism_type": PolymorphismType.NONE
                                            })
            self.assertEqual(inputs["y"], {"type": "REAL", 
                                           "description": "", 
                                           "dimension": None, 
                                           "interface_name": None,
                                           "enum_type": None,
                                           "default_value": None,
                                            "attributes": [],
                                            "kind": None,
                                            "length": None,
                                            "polymorphism_type": PolymorphismType.NONE
                                            })
            self.assertNotIn("z", inputs)
            self.assertIn("Warning: 'in' annotation 'z' not found in arguments [['x', 'y']]", cm.output[0])

    def test_subroutine_out_annotation_name_match(self):
        self.fs.create_file(
            "/fake/path/subroutine_out_annotation.f90",
            contents="""\
    module subroutine_out_annotation_module
    contains
    !!*
    ! A subroutine with @out annotation
    ! @out y
    !*!
    subroutine test_subroutine_out_annotation(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = x + 1.0
    end subroutine test_subroutine_out_annotation
    end module subroutine_out_annotation_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/subroutine_out_annotation.f90")])

        module = result[0]
        subroutine = module["subroutines"]["test_subroutine_out_annotation"]
        inputs = subroutine["in"]
        self.assertEqual(inputs["x"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "default_value": None,
                                       "attributes": [],
                                       "kind": None,
                                       "length": None,
                                       "polymorphism_type": PolymorphismType.NONE
                                       })
        outputs = subroutine["out"]
        self.assertEqual(outputs["y"], {"type": "REAL", 
                                        "description": "", 
                                        "dimension": None, 
                                        "interface_name": None, 
                                        "enum_type": None,
                                        "default_value": None,
                                        "attributes": [],
                                        "kind": None,
                                        "length": None,
                                        "polymorphism_type": PolymorphismType.NONE
                                        })

    def test_subroutine_out_annotation_name_mismatch(self):
        self.fs.create_file(
            "/fake/path/subroutine_out_annotation.f90",
            contents="""\
    module subroutine_out_annotation_module
    contains
    !!*
    ! A subroutine with @out annotation
    ! @out z
    !*!
    subroutine test_subroutine_out_annotation(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = x + 1.0
    end subroutine test_subroutine_out_annotation
    end module subroutine_out_annotation_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/subroutine_out_annotation.f90")])

        module = result[0]
        subroutine = module["subroutines"]["test_subroutine_out_annotation"]
        self.assertEqual(subroutine["description"], "A subroutine with @out annotation\n\n")
        inputs = subroutine["in"]
        self.assertEqual(inputs["x"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "attributes": [],
                                       "kind": None,
                                       "length": None,
                                       "default_value": None,
                                       "polymorphism_type": PolymorphismType.NONE
                                       })
        outputs = subroutine["out"]
        self.assertEqual(outputs["y"], {"type": "REAL", 
                                        "description": "", 
                                        "dimension": None, 
                                        "interface_name": None, 
                                        "enum_type": None,
                                        "attributes": [],
                                        "kind": None,
                                        "length": None,
                                        "default_value": None,
                                        "polymorphism_type": PolymorphismType.NONE
                                        })
        self.assertNotIn("z", outputs)

    def test_subroutine_inout_annotation_name_match(self):
        self.fs.create_file(
            "/fake/path/subroutine_inout_annotation.f90",
            contents="""\
    module subroutine_inout_annotation_module
    contains
    !!*
    ! A subroutine with @inout annotation
    ! @inout x
    !*!
    subroutine test_subroutine_inout_annotation(x)
        real, intent(inout) :: x
        x = x + 1.0
    end subroutine test_subroutine_inout_annotation
    end module subroutine_inout_annotation_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/subroutine_inout_annotation.f90")])

        module = result[0]
        subroutine = module["subroutines"]["test_subroutine_inout_annotation"]
        inputs = subroutine["in"]
        outputs = subroutine["out"]
        self.assertEqual(inputs["x"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "default_value": None,
                                       "attributes": [],
                                       "kind": None,
                                       "length": None,
                                       "polymorphism_type": PolymorphismType.NONE
                                       })
        self.assertEqual(outputs["x"], {"type": "REAL", 
                                        "description": "", 
                                        "dimension": None, 
                                        "interface_name": None, 
                                        "enum_type": None,
                                        "default_value": None,
                                        "attributes": [],
                                        "kind": None,
                                        "length": None,
                                        "polymorphism_type": PolymorphismType.NONE
                                        })

    def test_subroutine_inout_annotation_name_mismatch(self):
        self.fs.create_file(
            "/fake/path/subroutine_inout_annotation.f90",
            contents="""\
    module subroutine_inout_annotation_module
    contains
    !!*
    ! A subroutine with @inout annotation
    ! @inout z
    !*!
    subroutine test_subroutine_inout_annotation(x)
        real, intent(inout) :: x
        x = x + 1.0
    end subroutine test_subroutine_inout_annotation
    end module subroutine_inout_annotation_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/subroutine_inout_annotation.f90")])

        module = result[0]
        subroutine = module["subroutines"]["test_subroutine_inout_annotation"]
        inputs = subroutine["in"]
        outputs = subroutine["out"]
        self.assertEqual(inputs["x"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "default_value": None,
                                       "attributes": [],
                                       "kind": None,
                                       "length": None,
                                       "polymorphism_type": PolymorphismType.NONE
                                       })
        self.assertEqual(outputs["x"], {"type": "REAL", 
                                        "description": "", 
                                        "dimension": None,
                                        "interface_name": None, 
                                        "enum_type": None,
                                        "default_value": None,
                                        "attributes": [],
                                        "kind": None,
                                        "length": None,
                                        "polymorphism_type": PolymorphismType.NONE
                                        })
        self.assertNotIn("z", inputs)
        self.assertNotIn("z", outputs)

    def test_subroutine_in_annotation_name_match_with_description(self):
        self.fs.create_file(
            "/fake/path/subroutine_in_annotation.f90",
            contents="""\
    module subroutine_in_annotation_module
    contains
    !!*
    ! A subroutine with @in annotation
    ! @in x The first input
    ! @in y The second input
    !*!
    subroutine test_subroutine_in_annotation(x, y)
        real, intent(in) :: x, y
        print *, x + y
    end subroutine test_subroutine_in_annotation
    end module subroutine_in_annotation_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/subroutine_in_annotation.f90")])

        module = result[0]
        subroutine = module["subroutines"]["test_subroutine_in_annotation"]
        inputs = subroutine["in"]
        self.assertEqual(inputs["x"], {"type": "REAL", 
                                       "description": "The first input", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "default_value": None,
                                       "attributes": [],
                                       "kind": None,
                                       "length": None,
                                       "polymorphism_type": PolymorphismType.NONE
                                       })
        self.assertEqual(inputs["y"], {"type": "REAL", 
                                       "description": "The second input", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "default_value": None,
                                       "attributes": [],
                                       "kind": None,
                                       "length": None,
                                       "polymorphism_type": PolymorphismType.NONE
                                       })

    def test_subroutine_out_annotation_name_match_with_description(self):
        self.fs.create_file(
            "/fake/path/subroutine_out_annotation.f90",
            contents="""\
    module subroutine_out_annotation_module
    contains
    !!*
    ! A subroutine with @out annotation
    ! @in x The input
    ! @out y The output
    !*!
    subroutine test_subroutine_out_annotation(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = x + 1.0
    end subroutine test_subroutine_out_annotation
    end module subroutine_out_annotation_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/subroutine_out_annotation.f90")])

        module = result[0]
        subroutine = module["subroutines"]["test_subroutine_out_annotation"]
        inputs = subroutine["in"]
        self.assertEqual(inputs["x"], {"type": "REAL", 
                                       "description": "The input", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "default_value": None,
                                       "attributes": [],
                                       "kind": None,
                                       "length": None,
                                       "polymorphism_type": PolymorphismType.NONE
                                       })
        outputs = subroutine["out"]
        self.assertEqual(outputs["y"], {"type": "REAL", 
                                        "description": "The output", 
                                        "dimension": None, 
                                        "interface_name": None, 
                                        "enum_type": None,
                                        "default_value": None,
                                        "attributes": [],
                                        "kind": None,
                                        "length": None,
                                        "polymorphism_type": PolymorphismType.NONE
                                        })

    def test_subroutine_inout_annotation_name_match_with_description(self):
        self.fs.create_file(
            "/fake/path/subroutine_inout_annotation.f90",
            contents="""\
    module subroutine_inout_annotation_module
    contains
    !!*
    ! A subroutine with @inout annotation
    ! @inout x The variable to be updated
    !*!
    subroutine test_subroutine_inout_annotation(x)
        real, intent(inout) :: x
        x = x + 1.0
    end subroutine test_subroutine_inout_annotation
    end module subroutine_inout_annotation_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/subroutine_inout_annotation.f90")])

        module = result[0]
        subroutine = module["subroutines"]["test_subroutine_inout_annotation"]
        inputs = subroutine["in"]
        outputs = subroutine["out"]
        self.assertEqual(inputs["x"], {"type": "REAL", 
                                       "description": "The variable to be updated", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "default_value": None,
                                       "attributes": [],
                                       "kind": None,
                                       "length": None,
                                       "polymorphism_type": PolymorphismType.NONE
                                       })
        self.assertEqual(outputs["x"], {"type": "REAL", 
                                        "description": "The variable to be updated", 
                                        "dimension": None, 
                                        "interface_name": None, 
                                        "enum_type": None,
                                        "default_value": None,
                                        "attributes": [],
                                        "kind": None,
                                        "length": None,
                                        "polymorphism_type": PolymorphismType.NONE
                                        })

if __name__ == "__main__":
    unittest.main()
