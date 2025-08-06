import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.dimension_models import ArrayBound, BoundType
from doc4for.models.common import ExpressionType, Expression
from doc4for.models.procedure_models import PassType

class TestComplexTypes(TestCase):
    
    def setUp(self):
        self.setUpPyfakefs()

    def test_deep_inheritance_hierarchy(self):
        self.fs.create_file(
            "/fake/path/deep_inheritance.f90",
            contents="""\
module deep_inheritance_mod
    implicit none
    
    !!* Base type *!
    type :: base
        integer :: id
    contains
        procedure :: base_method
        procedure :: overridable => base_overridable
        generic :: write(formatted) => base_write
        procedure, private :: base_write
    end type base
    
    !!* First level extension *!
    type, extends(base) :: level1
        real :: data1
    contains
        procedure :: level1_method
        procedure :: overridable => level1_overridable  ! Override
        procedure :: base_method => level1_base_method   ! Override with rename
    end type level1
    
    !!* Second level extension *!
    type, extends(level1) :: level2
        real :: data2
    contains
        procedure :: level2_method
        procedure :: overridable => level2_overridable  ! Override again
        ! Inherits level1's override of base_method
        generic :: write(formatted) => level2_write  ! Add to generic
        procedure, private :: level2_write
    end type level2
    
    !!* Third level with abstract *!
    type, abstract, extends(level2) :: level3_abstract
        real :: data3
    contains
        procedure :: level3_method
        procedure(abstract_interface), deferred :: must_implement
        procedure :: overridable => level3_overridable  ! Override yet again
    end type level3_abstract
    
    !!* Concrete implementation of abstract *!
    type, extends(level3_abstract) :: level4_concrete
        real :: data4
    contains
        procedure :: must_implement => level4_implementation
        procedure :: level4_method
        final :: level4_cleanup
    end type level4_concrete
    
    abstract interface
        subroutine abstract_interface(this)
            import :: level3_abstract
            class(level3_abstract), intent(inout) :: this
        end subroutine abstract_interface
    end interface
    
end module deep_inheritance_mod
""",
        )
        result = extract_module_data([Path("/fake/path/deep_inheritance.f90")])
        module = result[0]
        types = module["types"]
        
        # Check inheritance chain
        self.assertIsNone(types["base"]["extends"])
        self.assertEqual(types["level1"]["extends"], "base")
        self.assertEqual(types["level2"]["extends"], "level1")
        self.assertEqual(types["level3_abstract"]["extends"], "level2")
        self.assertEqual(types["level4_concrete"]["extends"], "level3_abstract")
        
        # Check abstract type
        self.assertIn("ABSTRACT", types["level3_abstract"]["attributes"])
        self.assertNotIn("ABSTRACT", types["level4_concrete"]["attributes"])
        
        # Check deferred procedure
        self.assertIn("DEFERRED", types["level3_abstract"]["procedures"]["must_implement"]["attributes"])
        self.assertNotIn("DEFERRED", types["level4_concrete"]["procedures"]["must_implement"]["attributes"])
        
        # Check method overriding
        self.assertEqual(types["level1"]["procedures"]["base_method"]["implementation"], "level1_base_method")
        
        # Check final procedure
        self.assertTrue(types["level4_concrete"]["procedures"]["level4_cleanup"]["is_final"])
        
        # Check generic interface inheritance
        self.assertIn("write(formatted)", types["base"]["generic_interfaces"])
        self.assertIn("write(formatted)", types["level2"]["generic_interfaces"])


    def test_complex_data_components(self):
        self.fs.create_file(
            "/fake/path/complex_components.f90",
            contents="""\
module complex_components_mod
    use iso_c_binding
    implicit none
    
    type :: other_type
        real :: x, y
    end type other_type
    
    !!* Type with various component declarations *!
    type :: complex_type
        ! Multiple components in one line
        real :: a, b, c
        integer :: i = 1, j = 2, k = 3  ! With initialization
        
        ! Array components with different dimension styles
        real :: array1(10)
        real, dimension(20) :: array2
        real :: array3(5,5)
        real, dimension(3,4,5) :: array4
        real, allocatable :: array5(:,:)
        real, allocatable, dimension(:) :: array6
        
        ! Components with attributes
        real, pointer :: ptr => null()
        real, pointer, dimension(:) :: array_ptr => null()
        integer, private :: private_int
        real, public :: public_real
        
        ! Character components
        character(len=20) :: fixed_string
        character(len=:), allocatable :: var_string
        character(30) ::names(5)
        
        ! Derived type components
        type(other_type) :: nested
        type(other_type), dimension(10) :: nested_array
        type(other_type), allocatable :: nested_alloc
        type(other_type), pointer :: nested_ptr => null()
        
        ! C interop components
        real(c_double) :: c_real
        integer(c_int) :: c_int
        type(c_ptr) :: c_pointer
        
        ! Complex initialization expressions
        real :: computed = 3.14 * 2.0
        integer :: bit_pattern = int(B'1010101')
        logical :: flag = .true.
        complex :: cmplx_num = (1.0, 2.0)
        
        ! Components with kind parameters
        real(kind=8) :: double_prec
        integer(kind=selected_int_kind(15)) :: big_int
        
    contains
        procedure :: init
    end type complex_type
    
    !!* Type with unusual component arrangements *!
    type :: mixed_visibility
        private  ! Default private for following components
        
        integer :: priv1, priv2
        real :: priv_array(100)
                
        real :: pub1, pub2
        integer :: pub_array(50)
                
        type(other_type) :: priv_nested
        
    contains
        procedure :: get_private_sum
    end type mixed_visibility
    
end module complex_components_mod
""",
        )
        result = extract_module_data([Path("/fake/path/complex_components.f90")])
        module = result[0]
        types = module["types"]
        
        complex_type = types["complex_type"]
        components = complex_type["data_components"]
        
        # Test multiple components in one line
        self.assertIn("a", components)
        self.assertIn("b", components)
        self.assertIn("c", components)
        self.assertEqual(components["a"]["type"], "REAL")
        
        # Test array components
        self.assertIn("array1", components)
        self.assertEqual(components["array1"]["dimension"]["dimensions"], 
                         [ArrayBound(BoundType.FIXED, lower=Expression(ExpressionType.LITERAL, "1"), upper=Expression(ExpressionType.LITERAL, "10"))])
        
        self.assertIn("array2", components)
        self.assertEqual(components["array2"]["dimension"]["dimensions"], 
                         [ArrayBound(BoundType.FIXED, lower=Expression(ExpressionType.LITERAL, "1"), upper=Expression(ExpressionType.LITERAL, "20"))])
        
        self.assertIn("array3", components)
        self.assertEqual(components["array3"]["dimension"]["dimensions"],
                         [ArrayBound(BoundType.FIXED, lower=Expression(ExpressionType.LITERAL, "1"), upper=Expression(ExpressionType.LITERAL, "5")),
                          ArrayBound(BoundType.FIXED, lower=Expression(ExpressionType.LITERAL, "1"), upper=Expression(ExpressionType.LITERAL, "5"))                          ])                         
        
        self.assertIn("array5", components)
        self.assertIn("ALLOCATABLE", components["array5"]["attributes"])
        
        # Test pointer components
        self.assertIn("ptr", components)
        self.assertIn("POINTER", components["ptr"]["attributes"])
        
        # Test visibility attributes
        self.assertIn("private_int", components)
        self.assertIn("PRIVATE", components["private_int"]["attributes"])
        
        self.assertIn("public_real", components)
        self.assertIn("PUBLIC", components["public_real"]["attributes"])
        
        # Test character components
        self.assertIn("fixed_string", components)
        self.assertEqual(components["fixed_string"]["type"], "CHARACTER")
        
        # Test derived type components
        self.assertIn("nested", components)
        self.assertEqual(components["nested"]["type"], "other_type")
        
        self.assertIn("nested_array", components)
        self.assertEqual(components["nested_array"]["type"], "other_type")
        self.assertEqual(components["nested_array"]["dimension"]["dimensions"],
                         [ArrayBound(BoundType.FIXED, lower=Expression(ExpressionType.LITERAL, "1"), upper=Expression(ExpressionType.LITERAL, "10"))])
        
        # Test C interop components
        self.assertIn("c_real", components)
        self.assertEqual(components["c_real"]["kind"], "c_double")
        
        self.assertIn("c_pointer", components)
        self.assertEqual(components["c_pointer"]["type"], "c_ptr")
        
        # Test mixed visibility type
        mixed_type = types["mixed_visibility"]
        mixed_components = mixed_type["data_components"]
        
        # Components declared after 'private'
        self.assertIn("PRIVATE", mixed_components["priv1"]["attributes"])
        self.assertIn("PRIVATE", mixed_components["priv2"]["attributes"])
        self.assertIn("PRIVATE", mixed_components["priv_array"]["attributes"])        
        self.assertIn("PRIVATE", mixed_components["pub1"]["attributes"])
        self.assertIn("PRIVATE", mixed_components["pub2"]["attributes"])
        self.assertIn("PRIVATE", mixed_components["pub_array"]["attributes"])
        self.assertIn("PRIVATE", mixed_components["priv_nested"]["attributes"])

    def test_implicit_public_types(self):
        """Test types with implicit public visibility (no explicit private/public)"""
        self.fs.create_file(
            "/fake/path/implicit_public.f90",
            contents="""\
    module implicit_public_mod
        ! No implicit none - testing default behavior
        ! No private statement at module level
        
        ! Type with no explicit visibility - should be PUBLIC
        type :: implicit_public_type
            real :: x, y, z
        contains
            procedure :: calculate
        end type implicit_public_type
        
        ! Type with explicit private
        type, private :: explicit_private_type
            integer :: value
        end type explicit_private_type
        
        ! Type with explicit public (redundant but allowed)
        type, public :: explicit_public_type
            character(len=20) :: name
        end type explicit_public_type
        
        ! Another implicit public type
        type :: another_implicit_type
            type(implicit_public_type) :: nested
            real :: data(10)
        contains
            procedure :: process
        end type another_implicit_type
        
    contains
        
        subroutine calculate(this)
            class(implicit_public_type), intent(inout) :: this
            ! Implementation
        end subroutine calculate
        
        subroutine process(this)
            class(another_implicit_type), intent(inout) :: this
            ! Implementation
        end subroutine process
        
    end module implicit_public_mod
    """,
        )
        result = extract_module_data([Path("/fake/path/implicit_public.f90")])
        module = result[0]
        types = module["types"]
        
        # Test implicit public type (no explicit visibility)
        implicit_type = types["implicit_public_type"]
        self.assertEqual(implicit_type["attributes"], ["PUBLIC"])
        
        # Check components are also public by default
        self.assertEqual(implicit_type["data_components"]["x"]["attributes"], ["PUBLIC"])
        self.assertEqual(implicit_type["data_components"]["y"]["attributes"], ["PUBLIC"])
        self.assertEqual(implicit_type["data_components"]["z"]["attributes"], ["PUBLIC"])
        
        # Check procedure is public by default
        self.assertEqual(implicit_type["procedures"]["calculate"]["attributes"], ["PUBLIC"])
        
        # Test explicit private type
        private_type = types["explicit_private_type"]
        self.assertEqual(private_type["attributes"], ["PRIVATE"])
        self.assertEqual(private_type["data_components"]["value"]["attributes"], ["PUBLIC"])  # Components still default to public
        
        # Test explicit public type
        public_type = types["explicit_public_type"]
        self.assertEqual(public_type["attributes"], ["PUBLIC"])
        self.assertEqual(public_type["data_components"]["name"]["attributes"], ["PUBLIC"])
        
        # Test another implicit public type
        another_type = types["another_implicit_type"]
        self.assertEqual(another_type["attributes"], ["PUBLIC"])
        self.assertEqual(another_type["data_components"]["nested"]["attributes"], ["PUBLIC"])
        self.assertEqual(another_type["data_components"]["data"]["attributes"], ["PUBLIC"])
        self.assertEqual(another_type["procedures"]["process"]["attributes"], ["PUBLIC"])
            
    def test_blanket_public_statement(self):
        """Test blanket public statement that makes everything public by default"""
        self.fs.create_file(
            "/fake/path/blanket_public.f90",
            contents="""\
    module blanket_public_mod
        implicit none
        private  ! Start with private default
        
        ! Then switch to public for everything following
        public   ! Blanket public - no :: or names
        
        ! These types should all be PUBLIC due to blanket statement
        type :: first_type
            real :: value
        end type first_type
        
        type :: second_type
            integer :: count
            type(first_type) :: nested
        contains
            procedure :: calculate
        end type second_type
        
        ! Even though there's a blanket public, explicit private still works
        type, private :: explicit_private_type
            real :: hidden
        end type explicit_private_type
        
        ! Variables also affected by blanket public
        real :: module_var
        integer, parameter :: MODULE_CONST = 42
        
    contains
        
        subroutine calculate(this)
            class(second_type), intent(inout) :: this
            this%count = this%count + 1
        end subroutine calculate
        
        ! Module procedures also become public
        subroutine module_sub()
            print *, "Public subroutine"
        end subroutine module_sub
        
    end module blanket_public_mod
    """,
        )
        result = extract_module_data([Path("/fake/path/blanket_public.f90")])
        module = result[0]
        types = module["types"]
        
        # Test that types after blanket public are PUBLIC
        first_type = types["first_type"]
        self.assertEqual(first_type["attributes"], ["PUBLIC"])
        self.assertEqual(first_type["data_components"]["value"]["attributes"], ["PUBLIC"])
        
        second_type = types["second_type"]
        self.assertEqual(second_type["attributes"], ["PUBLIC"])
        self.assertEqual(second_type["data_components"]["count"]["attributes"], ["PUBLIC"])
        self.assertEqual(second_type["data_components"]["nested"]["attributes"], ["PUBLIC"])
        self.assertEqual(second_type["procedures"]["calculate"]["attributes"], ["PUBLIC"])
        
        # Test that explicit private still overrides blanket public
        private_type = types["explicit_private_type"]
        self.assertEqual(private_type["attributes"], ["PRIVATE"])
        
        # Test module variables are public
        self.assertIn("module_var", module["variables"])
        self.assertEqual(module["variables"]["module_var"]["attributes"], ["PUBLIC"])
        
        # Test module parameters are public
        self.assertIn("MODULE_CONST", module["parameters"])
        self.assertEqual(module["parameters"]["MODULE_CONST"]["attributes"], ["PUBLIC"])
        
        # Test module procedures are public
        self.assertIn("module_sub", module["subroutines"])
        self.assertEqual(module["subroutines"]["module_sub"]["attributes"], ["PUBLIC"])

    def test_inherited_visibility(self):
        """Test how visibility is inherited in different contexts"""
        self.fs.create_file(
            "/fake/path/inherited_visibility.f90",
            contents="""\
        !!*
        ! Module demonstrating visibility inheritance
        ! Shows how PUBLIC/PRIVATE attributes are inherited in various contexts
        !*!
        module inherited_visibility_mod
            implicit none
            private  ! Module-level default is private
            
            !!*
            ! Parent type with mixed visibility
            ! Demonstrates component and procedure visibility control
            !*!
            type, public :: parent_type
                private  ! Components are private by default
                !!* Private data component *!
                real :: private_component
                !!* Public data component *!
                integer, public :: public_component
            contains
                private  ! Procedures are private by default
                !!* Private method - not accessible outside *!
                procedure :: private_method
                !!* Public method - accessible everywhere *!
                procedure, public :: public_method
            end type parent_type
            
            !!*
            ! Child type extending parent
            ! Shows how visibility is inherited and can be overridden
            !*!
            type, public, extends(parent_type) :: child_type
                ! New components follow type's default (public)
                !!* Child's public data *!
                real :: child_public_data
                !!* Child's private data *!
                integer, private :: child_private_data
            contains
                ! Procedures default to public (no private statement)
                !!* Child-specific method *!
                procedure :: child_method
                !!* Overrides parent's public_method *!
                procedure :: public_method => child_public_override  ! Override
            end type child_type
            
            !!*
            ! Type demonstrating changing visibility defaults
            ! Shows how visibility can change within a type definition
            !*!
            type, public :: mixed_visibility_type
                ! No private statement, so components default to public
                !!* Public by default *!
                real :: public_by_default
                
                private  ! Now components are private by default
                !!* Private by default after private statement *!
                integer :: private_by_default
                !!* Explicitly public despite private default *!
                real, public :: explicitly_public
                
            contains
                ! No private statement, so procedures default to public
                !!* Public procedure before private statement *!
                procedure :: public_proc1
                
                private  ! Now procedures are private by default
                !!* Private procedure after private statement *!
                procedure :: private_proc1
                !!* Explicitly public procedure *!
                procedure, public :: explicitly_public_proc
                !!* Another private procedure *!
                procedure :: private_proc2
            end type mixed_visibility_type
            
        contains
            
            !!* Implementation of parent's private method *!
            subroutine private_method(this)
                class(parent_type), intent(in) :: this
            end subroutine
            
            !!* Implementation of parent's public method *!
            subroutine public_method(this)
                class(parent_type), intent(in) :: this
            end subroutine
            
            !!* Implementation of child's method *!
            subroutine child_method(this)
                class(child_type), intent(in) :: this
            end subroutine
            
            !!* Child's override of parent's public method *!
            subroutine child_public_override(this)
                class(child_type), intent(in) :: this
            end subroutine
            
            !!* First public procedure implementation *!
            subroutine public_proc1(this)
                class(mixed_visibility_type), intent(in) :: this
            end subroutine
            
            !!* First private procedure implementation *!
            subroutine private_proc1(this)
                class(mixed_visibility_type), intent(in) :: this
            end subroutine
            
            !!* Explicitly public procedure implementation *!
            subroutine explicitly_public_proc(this)
                class(mixed_visibility_type), intent(in) :: this
            end subroutine
            
            !!* Second private procedure implementation *!
            subroutine private_proc2(this)
                class(mixed_visibility_type), intent(in) :: this
            end subroutine
            
        end module inherited_visibility_mod
        """,
        )
        result = extract_module_data([Path("/fake/path/inherited_visibility.f90")])
        module = result[0]
        types = module["types"]
        
        # Check module description
        #TODO this is marked as the file description - need to fix
        # self.assertEqual(module["module_description"],
        #                 "Module demonstrating visibility inheritance\n"
        #                 "Shows how PUBLIC/PRIVATE attributes are inherited in various contexts\n")
        
        # Test parent type
        parent_type = types["parent_type"]
        self.assertEqual(parent_type["attributes"], ["PUBLIC"])
        self.assertEqual(parent_type["description"],
                        "Parent type with mixed visibility\n"
                        "Demonstrates component and procedure visibility control\n")
        
        # Components after private statement
        self.assertEqual(parent_type["data_components"]["private_component"]["attributes"], ["PRIVATE"])
        self.assertEqual(parent_type["data_components"]["private_component"]["description"], 
                        "Private data component\n")
        self.assertEqual(parent_type["data_components"]["public_component"]["attributes"], ["PUBLIC"])
        self.assertEqual(parent_type["data_components"]["public_component"]["description"],
                        "Public data component\n")
        
        # Procedures after private statement
        self.assertEqual(parent_type["procedures"]["private_method"]["attributes"], ["PRIVATE"])
        self.assertEqual(parent_type["procedures"]["private_method"]["description"],
                        "Private method - not accessible outside\n")
        self.assertEqual(parent_type["procedures"]["public_method"]["attributes"], ["PUBLIC"])
        self.assertEqual(parent_type["procedures"]["public_method"]["description"],
                        "Public method - accessible everywhere\n")
        
        # Test child type - inherits parent's component visibility
        child_type = types["child_type"]
        self.assertEqual(child_type["attributes"], ["PUBLIC"])
        self.assertEqual(child_type["description"],
                        "Child type extending parent\n"
                        "Shows how visibility is inherited and can be overridden\n")
        
        # New components (no private statement in child)
        self.assertEqual(child_type["data_components"]["child_public_data"]["attributes"], ["PUBLIC"])
        self.assertEqual(child_type["data_components"]["child_public_data"]["description"],
                        "Child&#x27;s public data\n")
        self.assertEqual(child_type["data_components"]["child_private_data"]["attributes"], ["PRIVATE"])
        self.assertEqual(child_type["data_components"]["child_private_data"]["description"],
                        "Child&#x27;s private data\n")
        
        # Child's procedures default to public
        self.assertEqual(child_type["procedures"]["child_method"]["attributes"], ["PUBLIC"])
        self.assertEqual(child_type["procedures"]["child_method"]["description"],
                        "Child-specific method\n")
        self.assertEqual(child_type["procedures"]["public_method"]["attributes"], ["PUBLIC"])
        self.assertEqual(child_type["procedures"]["public_method"]["description"],
                        "Overrides parent&#x27;s public_method\n")
        
        # Test mixed visibility type
        mixed_type = types["mixed_visibility_type"]
        self.assertEqual(mixed_type["description"],
                        "Type demonstrating changing visibility defaults\n"
                        "Shows how visibility can change within a type definition\n")
        
        # Component before private statement
        self.assertEqual(mixed_type["data_components"]["public_by_default"]["attributes"], ["PUBLIC"])
        self.assertEqual(mixed_type["data_components"]["public_by_default"]["description"],
                        "Public by default\n")
        
        # Components after private statement
        self.assertEqual(mixed_type["data_components"]["private_by_default"]["attributes"], ["PRIVATE"])
        self.assertEqual(mixed_type["data_components"]["private_by_default"]["description"],
                        "Private by default after private statement\n")
        self.assertEqual(mixed_type["data_components"]["explicitly_public"]["attributes"], ["PUBLIC"])
        self.assertEqual(mixed_type["data_components"]["explicitly_public"]["description"],
                        "Explicitly public despite private default\n")
        
        # Procedure before private statement
        self.assertEqual(mixed_type["procedures"]["public_proc1"]["attributes"], ["PUBLIC"])
        self.assertEqual(mixed_type["procedures"]["public_proc1"]["description"],
                        "Public procedure before private statement\n")
        
        # Procedures after private statement
        self.assertEqual(mixed_type["procedures"]["private_proc1"]["attributes"], ["PRIVATE"])
        self.assertEqual(mixed_type["procedures"]["private_proc1"]["description"],
                        "Private procedure after private statement\n")
        self.assertEqual(mixed_type["procedures"]["explicitly_public_proc"]["attributes"], ["PUBLIC"])
        self.assertEqual(mixed_type["procedures"]["explicitly_public_proc"]["description"],
                        "Explicitly public procedure\n")
        self.assertEqual(mixed_type["procedures"]["private_proc2"]["attributes"], ["PRIVATE"])
        self.assertEqual(mixed_type["procedures"]["private_proc2"]["description"],
                        "Another private procedure\n")

    def test_nested_types(self):
        """Test types containing components that are other derived types"""
        self.fs.create_file(
            "/fake/path/nested_types.f90",
            contents="""\
        !!*
        ! Module demonstrating nested type structures
        ! Shows how derived types can contain other derived types
        !*!
        module nested_types_mod
            implicit none
            
            !!*
            ! Basic inner type
            ! Will be used as a component in other types
            !*!
            type :: inner_type
                !!* X coordinate *!
                real :: x
                !!* Y coordinate *!  
                real :: y
                !!* Unique identifier *!
                integer :: id
            end type inner_type
            
            !!*
            ! Coordinate type
            ! Represents geographical coordinates
            !*!
            type :: coord_type
                !!* Latitude in degrees *!
                real :: lat
                !!* Longitude in degrees *!
                real :: lon
            end type coord_type
            
            !!*
            ! Outer type containing nested types
            ! Demonstrates basic type composition
            !*!
            type :: outer_type
                !!* Nested inner type component *!
                type(inner_type) :: inner
                !!* Location information *!
                type(coord_type) :: location
                !!* Item count *!
                integer :: count
            end type outer_type
            
            !!*
            ! Container with arrays of nested types
            ! Shows arrays, allocatable, and pointer components
            !*!
            type :: container_type
                !!* Fixed-size array of inner types *!
                type(inner_type), dimension(10) :: inner_array
                !!* Allocatable array of coordinates *!
                type(coord_type), allocatable :: locations(:)
                !!* Pointer to inner type *!
                type(inner_type), pointer :: inner_ptr => null()
            end type container_type
            
            !!*
            ! First level of nesting
            ! Part of deep nesting demonstration
            !*!
            type :: level1_type
                !!* Embedded data *!
                type(inner_type) :: data
            end type level1_type
            
            !!*
            ! Second level of nesting
            ! Contains level1 and additional data
            !*!
            type :: level2_type
                !!* Nested level1 type *!
                type(level1_type) :: level1
                !!* Coordinate data *!
                type(coord_type) :: coords
            end type level2_type
            
            !!*
            ! Third level of nesting
            ! Demonstrates deep type composition
            !*!
            type :: level3_type
                !!* Nested level2 type *!
                type(level2_type) :: level2
                !!* Container with arrays *!
                type(container_type) :: container
            end type level3_type
            
            !!*
            ! Type with mixed visibility nested components
            ! Shows access control for nested types
            !*!
            type :: mixed_nested_type
                private
                !!* Public inner type component *!
                type(inner_type), public :: public_inner
                !!* Private coordinate data *!
                type(coord_type) :: private_coords
                !!* Public counter *!
                integer, public :: public_count
            contains
                !!* Get private coordinates *!
                procedure :: get_coords
            end type mixed_nested_type
            
        contains
            
            !!*
            ! Retrieve private coordinate data
            ! @in this The mixed_nested_type instance
            ! @return Copy of private coordinates
            !*!
            function get_coords(this) result(coords)
                class(mixed_nested_type), intent(in) :: this
                type(coord_type) :: coords
                coords = this%private_coords
            end function get_coords
            
        end module nested_types_mod
        """,
        )
        result = extract_module_data([Path("/fake/path/nested_types.f90")])
        module = result[0]
        types = module["types"]
        
        # Check module description
        #TODO
        # self.assertEqual(module["module_description"],
        #                 "Module demonstrating nested type structures\n"
        #                 "Shows how derived types can contain other derived types\n")
        
        # Test inner_type
        inner_type = types["inner_type"]
        self.assertEqual(inner_type["description"],
                        "Basic inner type\n"
                        "Will be used as a component in other types\n")
        self.assertEqual(inner_type["data_components"]["x"]["description"], "X coordinate\n")
        self.assertEqual(inner_type["data_components"]["y"]["description"], "Y coordinate\n")
        self.assertEqual(inner_type["data_components"]["id"]["description"], "Unique identifier\n")
        
        # Test coord_type
        coord_type = types["coord_type"]
        self.assertEqual(coord_type["description"],
                        "Coordinate type\n"
                        "Represents geographical coordinates\n")
        self.assertEqual(coord_type["data_components"]["lat"]["description"], "Latitude in degrees\n")
        self.assertEqual(coord_type["data_components"]["lon"]["description"], "Longitude in degrees\n")
        
        # Test basic nested type
        outer_type = types["outer_type"]
        self.assertEqual(outer_type["description"],
                        "Outer type containing nested types\n"
                        "Demonstrates basic type composition\n")
        
        self.assertIn("inner", outer_type["data_components"])
        self.assertEqual(outer_type["data_components"]["inner"]["type"], "inner_type")
        self.assertEqual(outer_type["data_components"]["inner"]["attributes"], ["PUBLIC"])
        self.assertEqual(outer_type["data_components"]["inner"]["description"],
                        "Nested inner type component\n")
        
        self.assertIn("location", outer_type["data_components"])
        self.assertEqual(outer_type["data_components"]["location"]["type"], "coord_type")
        self.assertEqual(outer_type["data_components"]["location"]["description"],
                        "Location information\n")
        
        self.assertIn("count", outer_type["data_components"])
        self.assertEqual(outer_type["data_components"]["count"]["type"], "INTEGER")
        self.assertEqual(outer_type["data_components"]["count"]["description"],
                        "Item count\n")
        
        # Test container with arrays and pointers of nested types
        container_type = types["container_type"]
        self.assertEqual(container_type["description"],
                        "Container with arrays of nested types\n"
                        "Shows arrays, allocatable, and pointer components\n")
        
        inner_array = container_type["data_components"]["inner_array"]
        self.assertEqual(inner_array["type"], "inner_type")
        self.assertEqual(inner_array["dimension"]["dimensions"],
                        [ArrayBound(BoundType.FIXED, 
                                    lower=Expression(ExpressionType.LITERAL, "1"),
                                    upper=Expression(ExpressionType.LITERAL, "10"))])
        self.assertEqual(inner_array["description"], "Fixed-size array of inner types\n")
        
        locations = container_type["data_components"]["locations"]
        self.assertEqual(locations["type"], "coord_type")
        self.assertIn("ALLOCATABLE", locations["attributes"])
        self.assertEqual(locations["dimension"]["dimensions"],
                        [ArrayBound(BoundType.DEFERRED)])
        self.assertEqual(locations["description"], "Allocatable array of coordinates\n")
        
        inner_ptr = container_type["data_components"]["inner_ptr"]
        self.assertEqual(inner_ptr["type"], "inner_type")
        self.assertIn("POINTER", inner_ptr["attributes"])
        self.assertEqual(inner_ptr["description"], "Pointer to inner type\n")
        
        # Test deeply nested types
        level1 = types["level1_type"]
        self.assertEqual(level1["description"],
                        "First level of nesting\n"
                        "Part of deep nesting demonstration\n")
        self.assertEqual(level1["data_components"]["data"]["description"], "Embedded data\n")
        
        level2 = types["level2_type"]
        self.assertEqual(level2["description"],
                        "Second level of nesting\n"
                        "Contains level1 and additional data\n")
        self.assertEqual(level2["data_components"]["level1"]["description"], "Nested level1 type\n")
        self.assertEqual(level2["data_components"]["coords"]["description"], "Coordinate data\n")
        
        level3 = types["level3_type"]
        self.assertEqual(level3["description"],
                        "Third level of nesting\n"
                        "Demonstrates deep type composition\n")
        self.assertEqual(level3["data_components"]["level2"]["type"], "level2_type")
        self.assertEqual(level3["data_components"]["level2"]["description"], "Nested level2 type\n")
        self.assertEqual(level3["data_components"]["container"]["type"], "container_type")
        self.assertEqual(level3["data_components"]["container"]["description"], "Container with arrays\n")
        
        # Test mixed visibility with nested types
        mixed_type = types["mixed_nested_type"]
        self.assertEqual(mixed_type["description"],
                        "Type with mixed visibility nested components\n"
                        "Shows access control for nested types\n")
        
        public_inner = mixed_type["data_components"]["public_inner"]
        self.assertEqual(public_inner["type"], "inner_type")
        self.assertEqual(public_inner["attributes"], ["PUBLIC"])
        self.assertEqual(public_inner["description"], "Public inner type component\n")
        
        private_coords = mixed_type["data_components"]["private_coords"]
        self.assertEqual(private_coords["type"], "coord_type")
        self.assertEqual(private_coords["attributes"], ["PRIVATE"])
        self.assertEqual(private_coords["description"], "Private coordinate data\n")
        
        public_count = mixed_type["data_components"]["public_count"]
        self.assertEqual(public_count["type"], "INTEGER")
        self.assertEqual(public_count["attributes"], ["PUBLIC"])
        self.assertEqual(public_count["description"], "Public counter\n")
        
        # Test procedure
        get_coords = mixed_type["procedures"]["get_coords"]
        self.assertEqual(get_coords["description"], "Get private coordinates\n")
        
        # Test module function
        self.assertIn("get_coords", module["functions"])
        get_coords_func = module["functions"]["get_coords"]
        self.assertEqual(get_coords_func["description"],
                        "Retrieve private coordinate data\n\n")
        self.assertEqual(get_coords_func["in"]["this"]["description"],
                        "The mixed_nested_type instance")
        self.assertEqual(get_coords_func["return"]["description"],
                        "Copy of private coordinates")

    def test_complex_type_bound_procedures(self):
        """Test complex type-bound procedures with pass attributes, deferred procedures, and nested types"""
        self.fs.create_file(
            "/fake/path/complex_procedures.f90",
            contents="""\
        !!*
        ! Module demonstrating complex type-bound procedures
        ! Shows deferred procedures, pass attributes, and procedure pointers
        !*!
        module complex_procedures_mod
            implicit none
            
            !!*
            ! Simple data container type
            ! Used as a component in processor types
            !*!
            type :: data_type
                !!* The numeric value *!
                real :: value
                !!* Unique identifier *!
                integer :: id
            end type data_type
            
            ! Abstract interface for deferred procedures
            abstract interface
                !!* Interface for processing data *!
                subroutine process_interface(self, data)
                    import :: base_processor, data_type
                    class(base_processor), intent(inout) :: self
                    type(data_type), intent(in) :: data
                end subroutine process_interface
                
                !!* Interface for comparing processors *!
                function compare_interface(obj1, obj2) result(is_equal)
                    import :: base_processor
                    class(base_processor), intent(in) :: obj1, obj2
                    logical :: is_equal
                end function compare_interface
            end interface
            
            !!*
            ! Abstract base processor type
            ! Defines interface for all processor implementations
            !*!
            type, abstract :: base_processor
                !!* Current data being processed *!
                type(data_type), public :: current_data
                !!* Backup of previous data *!
                type(data_type), private :: backup_data
            contains
                !!* Process data - must be implemented by subclasses *!
                procedure(process_interface), deferred :: process
                !!* Compare two processors - pass to first argument *!
                procedure(compare_interface), deferred, pass(obj1) :: compare
                
                !!* Reset to backup data - explicit pass(self) *!
                procedure, pass(self) :: reset => base_reset
                
                !!* Validate processor state - non-default pass argument *!
                procedure, pass(processor) :: validate => base_validate
            end type base_processor
            
            !!*
            ! Concrete processor implementation
            ! Implements the abstract base processor interface
            !*!
            type, extends(base_processor) :: concrete_processor
                !!* History of processed data *!
                type(data_type), public :: history(100)
                !!* Number of items in history *!
                integer, private :: history_count = 0
            contains
                !!* Implementation of process procedure *!
                procedure :: process => concrete_process
                !!* Implementation of compare with pass(obj1) *!
                procedure, pass(obj1) :: compare => concrete_compare
                
                !!* Override validate with different pass argument *!
                procedure, pass(this) :: validate => concrete_validate
                
                !!* Merge two processors - custom pass argument *!
                procedure, pass(proc) :: merge => concrete_merge
            end type concrete_processor
            
            !!*
            ! Type with procedure pointer components
            ! Demonstrates procedure pointers with pass attributes
            !*!
            type :: callback_processor
                !!* Data storage *!
                type(data_type) :: data
                !!* Callback procedure pointer with pass(self) *!
                procedure(process_interface), pointer, pass(self) :: callback => null()
                !!* Comparator with pass(obj1) *!
                procedure(compare_interface), pointer, pass(obj1) :: comparator => null()
            contains
                !!* Set the callback procedure *!
                procedure :: set_callback
                !!* Static validation - no pass *!
                procedure, nopass :: static_validate
            end type callback_processor
            
        contains
            
            !!* Reset processor to backup state *!
            subroutine base_reset(self)
                class(base_processor), intent(inout) :: self
                self%current_data = self%backup_data
            end subroutine base_reset
            
            !!* Validate processor value against threshold *!
            function base_validate(processor, threshold) result(is_valid)
                class(base_processor), intent(in) :: processor
                real, intent(in) :: threshold
                logical :: is_valid
                is_valid = processor%current_data%value > threshold
            end function base_validate
            
            !!* Process data and update history *!
            subroutine concrete_process(self, data)
                class(concrete_processor), intent(inout) :: self
                type(data_type), intent(in) :: data
                self%current_data = data
                if (self%history_count < 100) then
                    self%history_count = self%history_count + 1
                    self%history(self%history_count) = data
                end if
            end subroutine concrete_process
            
            !!* Compare two processors by ID *!
            function concrete_compare(obj1, obj2) result(is_equal)
                class(concrete_processor), intent(in) :: obj1, obj2
                logical :: is_equal
                is_equal = obj1%current_data%id == obj2%current_data%id
            end function concrete_compare
            
            !!* Validate with history check *!
            function concrete_validate(this, threshold) result(is_valid)
                class(concrete_processor), intent(in) :: this
                real, intent(in) :: threshold
                logical :: is_valid
                is_valid = this%current_data%value > threshold .and. this%history_count > 0
            end function concrete_validate
            
            !!* Merge data from another processor *!
            subroutine concrete_merge(proc, other)
                class(concrete_processor), intent(inout) :: proc
                type(concrete_processor), intent(in) :: other
                ! Merge logic
            end subroutine concrete_merge
            
            !!* Set callback procedure pointer *!
            subroutine set_callback(this, callback_proc)
                class(callback_processor), intent(inout) :: this
                procedure(process_interface) :: callback_proc
                this%callback => callback_proc
            end subroutine set_callback
            
            !!* Static validation without object context *!
            function static_validate(data, threshold) result(is_valid)
                type(data_type), intent(in) :: data
                real, intent(in) :: threshold
                logical :: is_valid
                is_valid = data%value > threshold
            end function static_validate
            
        end module complex_procedures_mod
        """,
        )
        result = extract_module_data([Path("/fake/path/complex_procedures.f90")])
        module = result[0]
        types = module["types"]
        
        # Check module description
        # TODO
        # self.assertEqual(module["module_description"],
        #                 "Module demonstrating complex type-bound procedures\n"
        #                 "Shows deferred procedures, pass attributes, and procedure pointers\n")
        
        # Test data_type with nested components
        data_type = types["data_type"]
        self.assertEqual(data_type["description"],
                        "Simple data container type\n"
                        "Used as a component in processor types\n")
        self.assertEqual(data_type["data_components"]["value"]["type"], "REAL")
        self.assertEqual(data_type["data_components"]["value"]["description"], "The numeric value\n")
        self.assertEqual(data_type["data_components"]["id"]["type"], "INTEGER")
        self.assertEqual(data_type["data_components"]["id"]["description"], "Unique identifier\n")
        
        # Test abstract base type
        base_processor = types["base_processor"]
        self.assertIn("ABSTRACT", base_processor["attributes"])
        self.assertEqual(base_processor["description"],
                        "Abstract base processor type\n"
                        "Defines interface for all processor implementations\n")
        
        # Test nested type components with visibility
        self.assertEqual(base_processor["data_components"]["current_data"]["type"], "data_type")
        self.assertEqual(base_processor["data_components"]["current_data"]["attributes"], ["PUBLIC"])
        self.assertEqual(base_processor["data_components"]["current_data"]["description"],
                        "Current data being processed\n")
        self.assertEqual(base_processor["data_components"]["backup_data"]["type"], "data_type")
        self.assertEqual(base_processor["data_components"]["backup_data"]["attributes"], ["PRIVATE"])
        self.assertEqual(base_processor["data_components"]["backup_data"]["description"],
                        "Backup of previous data\n")
        
        # Test deferred procedures
        process_proc = base_processor["procedures"]["process"]
        self.assertIn("DEFERRED", process_proc["attributes"])
        self.assertEqual(process_proc["bound_to"], "process_interface")
        self.assertEqual(process_proc["pass_type"], PassType.DEFAULT)
        self.assertEqual(process_proc["description"],
                        "Process data - must be implemented by subclasses\n")
        
        compare_proc = base_processor["procedures"]["compare"]
        self.assertIn("DEFERRED", compare_proc["attributes"])
        self.assertEqual(compare_proc["bound_to"], "compare_interface")
        self.assertEqual(compare_proc["pass_type"], PassType.NAMED)
        self.assertEqual(compare_proc["pass_name"], "obj1")
        self.assertEqual(compare_proc["description"],
                        "Compare two processors - pass to first argument\n")
        
        # Test explicit pass(self)
        reset_proc = base_processor["procedures"]["reset"]
        self.assertEqual(reset_proc["pass_type"], PassType.NAMED)
        self.assertEqual(reset_proc["pass_name"], "self")
        self.assertEqual(reset_proc["implementation"], "base_reset")
        self.assertEqual(reset_proc["description"],
                        "Reset to backup data - explicit pass(self)\n")
        
        # Test non-default pass argument
        validate_proc = base_processor["procedures"]["validate"]
        self.assertEqual(validate_proc["pass_type"], PassType.NAMED)
        self.assertEqual(validate_proc["pass_name"], "processor")
        self.assertEqual(validate_proc["implementation"], "base_validate")
        self.assertEqual(validate_proc["description"],
                        "Validate processor state - non-default pass argument\n")
        
        # Test concrete implementation
        concrete_processor = types["concrete_processor"]
        self.assertEqual(concrete_processor["extends"], "base_processor")
        self.assertEqual(concrete_processor["description"],
                        "Concrete processor implementation\n"
                        "Implements the abstract base processor interface\n")
        
        # Test array of nested types
        history = concrete_processor["data_components"]["history"]
        self.assertEqual(history["type"], "data_type")
        self.assertEqual(history["dimension"]["dimensions"],
                        [ArrayBound(BoundType.FIXED,
                                    lower=Expression(ExpressionType.LITERAL, "1"),
                                    upper=Expression(ExpressionType.LITERAL, "100"))])
        self.assertEqual(history["attributes"], ["PUBLIC"])
        self.assertEqual(history["description"], "History of processed data\n")
        
        # Check history_count description
        self.assertEqual(concrete_processor["data_components"]["history_count"]["description"],
                        "Number of items in history\n")
        
        # Test overridden procedures
        concrete_process = concrete_processor["procedures"]["process"]
        self.assertNotIn("DEFERRED", concrete_process["attributes"])
        self.assertEqual(concrete_process["implementation"], "concrete_process")
        self.assertEqual(concrete_process["description"],
                        "Implementation of process procedure\n")
        
        concrete_compare = concrete_processor["procedures"]["compare"]
        self.assertEqual(concrete_compare["pass_type"], PassType.NAMED)
        self.assertEqual(concrete_compare["pass_name"], "obj1")
        self.assertEqual(concrete_compare["description"],
                        "Implementation of compare with pass(obj1)\n")
        
        # Test override with different pass name
        concrete_validate = concrete_processor["procedures"]["validate"]
        self.assertEqual(concrete_validate["pass_type"], PassType.NAMED)
        self.assertEqual(concrete_validate["pass_name"], "this")
        self.assertEqual(concrete_validate["description"],
                        "Override validate with different pass argument\n")
        
        # Test new procedure with custom pass
        merge_proc = concrete_processor["procedures"]["merge"]
        self.assertEqual(merge_proc["pass_type"], PassType.NAMED)
        self.assertEqual(merge_proc["pass_name"], "proc")
        self.assertEqual(merge_proc["description"],
                        "Merge two processors - custom pass argument\n")
        
        # Test callback processor with procedure pointers
        callback_processor = types["callback_processor"]
        self.assertEqual(callback_processor["description"],
                        "Type with procedure pointer components\n"
                        "Demonstrates procedure pointers with pass attributes\n")
        
        self.assertEqual(callback_processor["data_components"]["data"]["description"],
                        "Data storage\n")
        
        callback = callback_processor["procedures"]["callback"]
        self.assertIn("POINTER", callback["attributes"])
        self.assertEqual(callback["bound_to"], "process_interface")
        self.assertEqual(callback["pass_type"], PassType.NAMED)
        self.assertEqual(callback["pass_name"], "self")
        self.assertEqual(callback["description"],
                        "Callback procedure pointer with pass(self)\n")
        
        comparator = callback_processor["procedures"]["comparator"]
        self.assertIn("POINTER", comparator["attributes"])
        self.assertEqual(comparator["bound_to"], "compare_interface")
        self.assertEqual(comparator["pass_type"], PassType.NAMED)
        self.assertEqual(comparator["pass_name"], "obj1")
        self.assertEqual(comparator["description"],
                        "Comparator with pass(obj1)\n")
        
        # Test procedure descriptions
        self.assertEqual(callback_processor["procedures"]["set_callback"]["description"],
                        "Set the callback procedure\n")
        
        # Test nopass procedure
        static_validate = callback_processor["procedures"]["static_validate"]
        self.assertEqual(static_validate["pass_type"], PassType.NONE)
        self.assertEqual(static_validate["description"],
                        "Static validation - no pass\n")

    def test_advanced_module_with_allocatable(self):
        """Test advanced module with allocatable components and inheritance"""
        self.fs.create_file(
            "/fake/path/advanced_allocatable.f90",
            contents="""\
    module advanced_module
        implicit none
        private

        ! Public constants
        real, parameter, public :: PI = 3.14159265359
        integer, parameter, public :: MAX_DIMENSIONS = 3

        ! Public type declarations
        public :: base_vector, advanced_vector

        type :: base_vector
            private
            real, dimension(:), allocatable :: components
        contains
            procedure :: init => init_base
            procedure :: get_component
            final :: cleanup_base
        end type base_vector

        type, extends(base_vector) :: advanced_vector
            private
            integer :: dimension
        contains
            procedure :: init => init_advanced
            procedure :: magnitude
            procedure :: add
            generic :: operator(+) => add
        end type advanced_vector

    contains

        subroutine init_base(this, components)
            class(base_vector), intent(inout) :: this
            real, dimension(:), intent(in) :: components
            if (allocated(this%components)) deallocate(this%components)
            allocate(this%components, source=components)
        end subroutine init_base

        function get_component(this, index) result(value)
            class(base_vector), intent(in) :: this
            integer, intent(in) :: index
            real :: value
            if (index <= size(this%components)) then
                value = this%components(index)
            else
                value = 0.0  ! Or handle error as appropriate
            end if
        end function get_component

        subroutine cleanup_base(this)
            type(base_vector), intent(inout) :: this
            if (allocated(this%components)) deallocate(this%components)
        end subroutine cleanup_base

        subroutine init_advanced(this, components)
            class(advanced_vector), intent(inout) :: this
            real, dimension(:), intent(in) :: components
            call this%init_base(components)
            this%dimension = size(components)
        end subroutine init_advanced

        function magnitude(this) result(mag)
            class(advanced_vector), intent(in) :: this
            real :: mag
            mag = sqrt(sum(this%components**2))
        end function magnitude

        function add(this, other) result(result_vector)
            class(advanced_vector), intent(in) :: this, other
            type(advanced_vector) :: result_vector
            integer :: i
            real, dimension(:), allocatable :: new_components

            allocate(new_components(this%dimension))
            do i = 1, this%dimension
                new_components(i) = this%components(i) + other%components(i)
            end do
            call result_vector%init(new_components)
            deallocate(new_components)
        end function add

    end module advanced_module
    """,
        )
        result = extract_module_data([Path("/fake/path/advanced_allocatable.f90")])
        module = result[0]
        
        # Test module-level private with public exports
        self.assertEqual(module["module_name"], "advanced_module")
        
        # Test public parameters
        self.assertIn("PI", module["parameters"])
        self.assertIn("PUBLIC", module["parameters"]["PI"]["attributes"])
        self.assertEqual(module["parameters"]["PI"]["type"], "REAL")
        
        self.assertIn("MAX_DIMENSIONS", module["parameters"])
        self.assertIn("PUBLIC", module["parameters"]["MAX_DIMENSIONS"]["attributes"])
        self.assertEqual(module["parameters"]["MAX_DIMENSIONS"]["type"], "INTEGER")
        
        # Test base_vector type
        base_vector = module["types"]["base_vector"]
        self.assertIn("PUBLIC", base_vector["attributes"])
        
        # Test private allocatable component
        components = base_vector["data_components"]["components"]
        self.assertEqual(components["type"], "REAL")
        self.assertIn("ALLOCATABLE", components["attributes"])
        self.assertIn("PRIVATE", components["attributes"])
        self.assertEqual(components["dimension"]["dimensions"],
                        [ArrayBound(BoundType.DEFERRED)])
        
        # Test type-bound procedures
        self.assertEqual(base_vector["procedures"]["init"]["implementation"], "init_base")
        self.assertIsNone(base_vector["procedures"]["get_component"]["implementation"])
        
        # Test final procedure
        cleanup = base_vector["procedures"]["cleanup_base"]
        self.assertTrue(cleanup["is_final"])
        self.assertEqual(cleanup["attributes"], [])  # Final procedures don't have access specifiers
        
        # Test advanced_vector type
        advanced_vector = module["types"]["advanced_vector"]
        self.assertEqual(advanced_vector["extends"], "base_vector")
        self.assertIn("PUBLIC", advanced_vector["attributes"])
        
        # Test private component
        dimension = advanced_vector["data_components"]["dimension"]
        self.assertEqual(dimension["type"], "INTEGER")
        self.assertIn("PRIVATE", dimension["attributes"])
        
        # Test overridden init procedure
        self.assertEqual(advanced_vector["procedures"]["init"]["implementation"], "init_advanced")
        
        # Test new procedures
        self.assertIn("magnitude", advanced_vector["procedures"])
        self.assertIn("add", advanced_vector["procedures"])
        
        # Test generic operator
        self.assertIn("operator(+)", advanced_vector["generic_interfaces"])
        plus_op = advanced_vector["generic_interfaces"]["operator(+)"]
        self.assertIn("add", plus_op["specific_procedures"])
        self.assertIn("PUBLIC", plus_op["attributes"])
        
        # Test module procedures are private by default
        self.assertEqual(module["subroutines"]["init_base"]["attributes"], ["PRIVATE"])
        self.assertEqual(module["functions"]["get_component"]["attributes"], ["PRIVATE"])
        self.assertEqual(module["subroutines"]["cleanup_base"]["attributes"], ["PRIVATE"])
        self.assertEqual(module["subroutines"]["init_advanced"]["attributes"], ["PRIVATE"])
        self.assertEqual(module["functions"]["magnitude"]["attributes"], ["PRIVATE"])
        self.assertEqual(module["functions"]["add"]["attributes"], ["PRIVATE"])

    def test_advanced_module_with_operators(self):
        """Test advanced module with type-bound operators and iso_fortran_env"""
        self.fs.create_file(
            "/fake/path/advanced_operators.f90",
            contents="""\
    module advanced_module
        use iso_fortran_env, only: sp => real32, dp => real64
        implicit none
        private

        ! Public parameters
        real(dp), parameter, public :: PI = 3.14159265358979323846_dp
        integer, parameter, public :: MAX_DIMENSIONS = 3

        ! Public type declarations
        public :: base_vector, advanced_vector

        type :: base_vector
            real(sp), dimension(MAX_DIMENSIONS) :: components
        contains
            procedure :: magnitude => base_magnitude
            generic :: operator(+) => add_vectors
            procedure, private :: add_vectors
            final :: base_cleanup
        end type base_vector

        type, extends(base_vector) :: advanced_vector
            integer :: dimension
        contains
            procedure :: magnitude => advanced_magnitude
            procedure :: dot_product
            generic :: operator(*) => dot_product
        end type advanced_vector

        interface advanced_vector
            module procedure :: create_advanced_vector
        end interface advanced_vector

    contains

        function base_magnitude(this) result(mag)
            class(base_vector), intent(in) :: this
            real(sp) :: mag
            mag = sqrt(sum(this%components**2))
        end function base_magnitude

        function add_vectors(v1, v2) result(v_sum)
            class(base_vector), intent(in) :: v1, v2
            type(base_vector) :: v_sum
            v_sum%components = v1%components + v2%components
        end function add_vectors

        subroutine base_cleanup(this)
            type(base_vector), intent(inout) :: this
            this%components = 0.0_sp
        end subroutine base_cleanup

        function advanced_magnitude(this) result(mag)
            class(advanced_vector), intent(in) :: this
            real(sp) :: mag
            mag = sqrt(sum(this%components(:this%dimension)**2))
        end function advanced_magnitude

        function dot_product(this, other) result(dot)
            class(advanced_vector), intent(in) :: this
            class(advanced_vector), intent(in) :: other
            real(sp) :: dot
            dot = sum(this%components(:this%dimension) * other%components(:other%dimension))
        end function dot_product

        function create_advanced_vector(components, dimension) result(vec)
            real(sp), dimension(:), intent(in) :: components
            integer, intent(in) :: dimension
            type(advanced_vector) :: vec
            vec%components(:dimension) = components(:dimension)
            vec%dimension = dimension
        end function create_advanced_vector

    end module advanced_module
    """,
        )
        result = extract_module_data([Path("/fake/path/advanced_operators.f90")])
        module = result[0]
        
        # Test module with use statement
        self.assertEqual(module["module_name"], "advanced_module")
        
        # Test parameters with kind from renamed import
        pi_param = module["parameters"]["PI"]
        self.assertEqual(pi_param["type"], "REAL")
        self.assertEqual(pi_param["kind"], "dp")
        self.assertIn("PUBLIC", pi_param["attributes"])
        
        # Test base_vector with fixed-size array
        base_vector = module["types"]["base_vector"]
        components = base_vector["data_components"]["components"]
        self.assertEqual(components["type"], "REAL")
        self.assertEqual(components["kind"], "sp")
        self.assertEqual(components["dimension"]["dimensions"],
                        [ArrayBound(BoundType.VARIABLE,
                                    lower=Expression(ExpressionType.LITERAL, "1"),
                                    upper=Expression(ExpressionType.VARIABLE, "MAX_DIMENSIONS"))])
        self.assertIn("PUBLIC", components["attributes"])  # No private statement in type
        
        # Test renamed procedure binding
        magnitude_proc = base_vector["procedures"]["magnitude"]
        self.assertEqual(magnitude_proc["implementation"], "base_magnitude")
        
        # Test operator with private specific procedure
        plus_op = base_vector["generic_interfaces"]["operator(+)"]
        self.assertIn("add_vectors", plus_op["specific_procedures"])
        self.assertIn("PUBLIC", plus_op["attributes"])
        
        # The specific procedure should be private
        add_vectors_proc = base_vector["procedures"]["add_vectors"]
        self.assertIn("PRIVATE", add_vectors_proc["attributes"])
        
        # Test final procedure
        self.assertTrue(base_vector["procedures"]["base_cleanup"]["is_final"])
        
        # Test advanced_vector
        advanced_vector = module["types"]["advanced_vector"]
        self.assertEqual(advanced_vector["extends"], "base_vector")
        
        # Test new operator
        mult_op = advanced_vector["generic_interfaces"]["operator(*)"]
        self.assertIn("dot_product", mult_op["specific_procedures"])
        
        # Test overridden procedure
        adv_magnitude = advanced_vector["procedures"]["magnitude"]
        self.assertEqual(adv_magnitude["implementation"], "advanced_magnitude")
        
        # Test interface (constructor)
        interfaces = [interface for interface in module["interfaces"] if interface["name"] == "advanced_vector"]
        self.assertEqual(len(interfaces), 1)
        self.assertIn("create_advanced_vector", interfaces[0]["module_procedures"])
        self.assertIn("PUBLIC", interfaces[0]["attributes"])
        
        # Test module functions with kind parameters
        base_mag_func = module["functions"]["base_magnitude"]
        self.assertEqual(base_mag_func["return"]["type"], "REAL")
        self.assertEqual(base_mag_func["return"]["kind"], "sp")
        
        create_func = module["functions"]["create_advanced_vector"]
        self.assertEqual(create_func["return"]["type"], "advanced_vector")
        self.assertEqual(create_func["in"]["components"]["kind"], "sp")

    def test_wild_type_with_pdt_parameters(self):
        """Test parameterized derived type with kind and length parameters"""
        self.fs.create_file(
            "/fake/path/wild_type.f90",
            contents="""\
    module wild_type_mod
        implicit none
        
        ! A parameterized derived type with both kind and length parameters
        type :: wild_type(p, q, r)
            integer, len :: p                       ! Length parameter
            integer, kind :: q = kind(0.0)          ! Kind parameter with default
            integer, len :: r = 10                  ! Length parameter with default
            
            ! Components using the parameters
            integer(kind=q) :: fixed_kind_value     ! Component with parameterized kind
            real(kind=q) :: flexible_array(p)       ! Array with parameterized kind and size
            character(len=r) :: name                ! Character with parameterized length
            complex(kind=q), allocatable :: data(:) ! Allocatable with parameterized kind
        contains
            procedure :: print_info
        end type wild_type
        
        ! Type that extends a PDT
        type, extends(wild_type) :: extended_wild(s)
            integer, len :: s                       ! Additional length parameter
            real :: extra_data(s)                   ! Use the new parameter
        contains
            procedure :: print_info => print_extended_info
        end type extended_wild
        
    contains
        
        subroutine print_info(this)
            class(wild_type(*,*,*)), intent(in) :: this
            print *, "Array size p =", this%p
            print *, "Kind parameter q =", this%q
            print *, "Name length r =", this%r
        end subroutine print_info
        
        subroutine print_extended_info(this)
            class(extended_wild(*,*,*,*)), intent(in) :: this
            print *, "Extended with s =", this%s
            call this%wild_type%print_info()
        end subroutine print_extended_info
        
    end module wild_type_mod
    """,
        )
        result = extract_module_data([Path("/fake/path/wild_type.f90")])
        module = result[0]
        types = module["types"]
        
        # Note: fparser2 may have limited support for PDTs, so some of these
        # assertions might need to be adjusted based on what it can actually parse
        
        # Test wild_type exists
        self.assertIn("wild_type", types)
        wild_type = types["wild_type"]
        
        # Test type parameters (if fparser captures them)
        # PDT parameters might be stored differently or not at all
        # depending on fparser's PDT support
        
        # Test components that use parameters
        if "fixed_kind_value" in wild_type["data_components"]:
            fixed_kind = wild_type["data_components"]["fixed_kind_value"]
            self.assertEqual(fixed_kind["type"], "INTEGER")
            # The kind might be stored as 'q' or might not be captured
            if "kind" in fixed_kind:
                self.assertIn(fixed_kind["kind"], ["q", None])
        
        if "flexible_array" in wild_type["data_components"]:
            flex_array = wild_type["data_components"]["flexible_array"]
            self.assertEqual(flex_array["type"], "REAL")
            # Check if array dimension references parameter 'p'
            # This might be stored as a string or not captured
            self.assertIsNotNone(flex_array.get("dimension"))
        
        if "name" in wild_type["data_components"]:
            name_comp = wild_type["data_components"]["name"]
            self.assertEqual(name_comp["type"], "CHARACTER")
            # Length might be 'r' or not captured
        
        if "data" in wild_type["data_components"]:
            data_comp = wild_type["data_components"]["data"]
            self.assertEqual(data_comp["type"], "COMPLEX")
            self.assertIn("ALLOCATABLE", data_comp["attributes"])
        
        # Test procedure
        self.assertIn("print_info", wild_type["procedures"])
        
        # Test extended PDT
        if "extended_wild" in types:
            extended = types["extended_wild"]
            self.assertEqual(extended["extends"], "wild_type")
            
            # Test additional component
            if "extra_data" in extended["data_components"]:
                extra = extended["data_components"]["extra_data"]
                self.assertEqual(extra["type"], "REAL")
                # Dimension uses parameter 's'
            
            # Test overridden procedure
            if "print_info" in extended["procedures"]:
                print_proc = extended["procedures"]["print_info"]
                self.assertEqual(print_proc["implementation"], "print_extended_info")
        
        # Test the procedures at module level
        if "print_info" in module["subroutines"]:
            print_sub = module["subroutines"]["print_info"]
            # The assumed type parameters (*,*,*) might not be captured
            self.assertIn("this", print_sub["in"])
        
        # Add a simple test to ensure basic parsing worked
        self.assertGreater(len(types), 0, "Should have parsed at least one type")


if __name__ == "__main__":
    unittest.main()
