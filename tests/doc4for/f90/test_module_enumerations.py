import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum, BindingType

class TestDerivedTypeBindings(TestCase):

    def setUp(self):
        self.setUpPyfakefs()

    def test_basic_enum(self):
        self.fs.create_file(
            "/fake/path/basic_enum.f90",
            contents="""\
    module color_mod
        implicit none

        !!* Basic color enumeration *!
        enum, bind(c)
            !!* Red color *!
            enumerator :: RED = 1
            !!* Green color *!
            enumerator :: GREEN = 2
            !!* Blue color *!
            enumerator :: BLUE = 3
        end enum

    end module color_mod
    """
        )
        result = extract_module_data([Path('/fake/path/basic_enum.f90')])
        module = result[0]
        
        self.assertIn('enums', module)
        enum = module['enums']['RED']  # using the first enum value for an unnamed enum
        
        self.assertEqual(enum['description'], 'Basic color enumeration\n')
        self.assertEqual(enum['name'], "__ENUM__")
        self.assertEqual(enum['attributes'], [])
        self.assertEqual(enum["binding_type"], { "type": BindingTypeEnum.BIND_C, "name": None})

        # Check enumerators
        self.assertEqual(len(enum['enumerators']), 3)
        enumerators = enum['enumerators']
        self.assertEqual(enumerators['RED']['name'], 'RED')
        self.assertEqual(enumerators['RED']['value'], '1')
        self.assertEqual(enumerators['RED']['description'], 'Red color\n')
        self.assertEqual(enumerators['GREEN']['name'], 'GREEN')
        self.assertEqual(enumerators['GREEN']['value'], '2')
        self.assertEqual(enumerators['GREEN']['description'], 'Green color\n')
        self.assertEqual(enumerators['BLUE']['name'], 'BLUE')
        self.assertEqual(enumerators['BLUE']['value'], '3')
        self.assertEqual(enumerators['BLUE']['description'], 'Blue color\n')

    def test_implicit_values(self):
        self.fs.create_file(
            "/fake/path/implicit_enum.f90",
            contents="""\
    module weekday_mod
        implicit none

        !!* Days of the week *!
        enum, bind(c)
            !!* First day *!
            enumerator :: MONDAY
            !!* Second day *!
            enumerator :: TUESDAY
            !!* Third day *!
            enumerator :: WEDNESDAY
        end enum

    end module weekday_mod
    """
        )
        result = extract_module_data([Path('/fake/path/implicit_enum.f90')])
        module = result[0]
        
        self.assertIn('enums', module)
        enum = module['enums']['MONDAY']  # using the first enum value for an unnamed enum
        
        self.assertEqual(enum['description'], 'Days of the week\n')
        self.assertEqual(enum['name'], "__ENUM__")
        self.assertEqual(enum['attributes'], [])
        self.assertEqual(enum["binding_type"], { "type": BindingTypeEnum.BIND_C, "name": None})

        # Check enumerators
        self.assertEqual(len(enum['enumerators']), 3)
        enumerators = enum['enumerators']
        self.assertEqual(enumerators['MONDAY']['name'], 'MONDAY')
        self.assertEqual(enumerators['MONDAY']['value'], '0')
        self.assertEqual(enumerators['MONDAY']['description'], 'First day\n')
        self.assertEqual(enumerators['TUESDAY']['name'], 'TUESDAY')
        self.assertEqual(enumerators['TUESDAY']['value'], '1')
        self.assertEqual(enumerators['TUESDAY']['description'], 'Second day\n')
        self.assertEqual(enumerators['WEDNESDAY']['name'], 'WEDNESDAY')
        self.assertEqual(enumerators['WEDNESDAY']['value'], '2')
        self.assertEqual(enumerators['WEDNESDAY']['description'], 'Third day\n')
            
    def test_enum_expressions(self):
        self.fs.create_file(
            "/fake/path/enum_expressions.f90",
            contents="""\
    module constants_mod
        implicit none

        !!* Error code enumeration *!
        enum, bind(c)
            !!* Success code *!
            enumerator :: SUCCESS = 0
            !!* Warning level *!
            enumerator :: WARNING = 100
            !!* Error starts at warning + 100 *!
            enumerator :: ERROR = WARNING + 100
            !!* Critical is double error *!
            enumerator :: CRITICAL = ERROR * 2
        end enum

    end module constants_mod
    """
        )
        result = extract_module_data([Path('/fake/path/enum_expressions.f90')])
        module = result[0]
        
        self.assertIn('enums', module)
        enum = module['enums']['SUCCESS']  # using first enum value
        
        self.assertEqual(enum['description'], 'Error code enumeration\n')
        self.assertEqual(enum['name'], "__ENUM__")
        self.assertEqual(enum['attributes'], [])
        self.assertEqual(enum["binding_type"], { "type": BindingTypeEnum.BIND_C, "name": None})

        # Check enumerators
        self.assertEqual(len(enum['enumerators']), 4)
        enumerators = enum['enumerators']
        self.assertEqual(enumerators['SUCCESS']['name'], 'SUCCESS')
        self.assertEqual(enumerators['SUCCESS']['value'], '0')
        self.assertEqual(enumerators['SUCCESS']['description'], 'Success code\n')
        
        self.assertEqual(enumerators['WARNING']['name'], 'WARNING')
        self.assertEqual(enumerators['WARNING']['value'], '100')
        self.assertEqual(enumerators['WARNING']['description'], 'Warning level\n')
        
        self.assertEqual(enumerators['ERROR']['name'], 'ERROR')
        self.assertEqual(enumerators['ERROR']['value'], 'WARNING + 100')
        self.assertEqual(enumerators['ERROR']['description'], 'Error starts at warning + 100\n')
        
        self.assertEqual(enumerators['CRITICAL']['name'], 'CRITICAL')
        self.assertEqual(enumerators['CRITICAL']['value'], 'ERROR * 2')
        self.assertEqual(enumerators['CRITICAL']['description'], 'Critical is double error\n')


    def test_multiple_enums_in_module(self):
        self.fs.create_file(
            "/fake/path/multiple_enums.f90",
            contents="""\
    module multi_enum_mod
        implicit none

        !!* Color choices *!
        enum, bind(c)
            !!* Basic colors *!
            enumerator :: RED, GREEN, BLUE
        end enum

        !!* Status codes *!
        enum, bind(c)
            !!* Operation completed *!
            enumerator :: OK = 0
            !!* Operation failed *!
            enumerator :: FAIL = -1
        end enum

    end module multi_enum_mod
    """
        )
        result = extract_module_data([Path('/fake/path/multiple_enums.f90')])
        module = result[0]
        
        self.assertIn('enums', module)
        self.assertEqual(len(module['enums']), 2)
        
        # First enum (using RED as key)
        color_enum = module['enums']['RED']
        self.assertEqual(color_enum['description'], 'Color choices\n')
        self.assertEqual(color_enum['name'], "__ENUM__")
        self.assertEqual(color_enum['attributes'], [])
        self.assertEqual(color_enum["binding_type"], { "type": BindingTypeEnum.BIND_C, "name": None})
        
        # Check color enum enumerators
        self.assertEqual(len(color_enum['enumerators']), 3)
        color_enumerators = color_enum['enumerators']
        self.assertEqual(color_enumerators['RED']['name'], 'RED')
        self.assertEqual(color_enumerators['RED']['value'], '0')
        self.assertEqual(color_enumerators['RED']['description'], 'Basic colors\n')
        self.assertEqual(color_enumerators['GREEN']['name'], 'GREEN')
        self.assertEqual(color_enumerators['GREEN']['value'], '1')
        self.assertEqual(color_enumerators['GREEN']['description'], 'Basic colors\n')
        self.assertEqual(color_enumerators['BLUE']['name'], 'BLUE')
        self.assertEqual(color_enumerators['BLUE']['value'], '2')
        self.assertEqual(color_enumerators['BLUE']['description'], 'Basic colors\n')
        
        # Second enum (using OK as key)
        status_enum = module['enums']['OK']
        self.assertEqual(status_enum['description'], 'Status codes\n')
        self.assertEqual(status_enum['name'], "__ENUM__")
        self.assertEqual(status_enum['attributes'], [])
        self.assertEqual(status_enum["binding_type"], { "type": BindingTypeEnum.BIND_C, "name": None})
        
        # Check status enum enumerators
        self.assertEqual(len(status_enum['enumerators']), 2)
        status_enumerators = status_enum['enumerators']
        self.assertEqual(status_enumerators['OK']['name'], 'OK')
        self.assertEqual(status_enumerators['OK']['value'], '0')
        self.assertEqual(status_enumerators['OK']['description'], 'Operation completed\n')
        self.assertEqual(status_enumerators['FAIL']['name'], 'FAIL')
        self.assertEqual(status_enumerators['FAIL']['value'], '-1')
        self.assertEqual(status_enumerators['FAIL']['description'], 'Operation failed\n')

    def test_enum_comments_and_spacing(self):
        self.fs.create_file(
            "/fake/path/enum_formatting.f90",
            contents="""\
    module format_mod
        implicit none

        !!* 
        ! Multi-line enum description
        ! with detailed documentation
        !*!
        enum,bind(c)  ! Compact binding
            
            !!* First value with extra space *!
            enumerator::FIRST=1

            !!* 
            ! Multi-line
            ! enumerator description
            !*!
            enumerator :: SECOND = 2
            
        end enum

    end module format_mod
    """
        )
        result = extract_module_data([Path('/fake/path/enum_formatting.f90')])
        module = result[0]
        
        self.assertIn('enums', module)
        
        # Use FIRST as the key
        enum = module['enums']['FIRST']
        
        # Check multi-line description
        self.assertEqual(
            enum['description'], 
            '\nMulti-line enum description\nwith detailed documentation\n\n'
        )
        
        self.assertEqual(enum['name'], "__ENUM__")
        self.assertEqual(enum['attributes'], [])
        self.assertEqual(enum["binding_type"], { "type": BindingTypeEnum.BIND_C, "name": None})
        
        # Check enumerators
        self.assertEqual(len(enum['enumerators']), 2)
        enumerators = enum['enumerators']
        
        # Check FIRST enumerator
        self.assertEqual(enumerators['FIRST']['name'], 'FIRST')
        self.assertEqual(enumerators['FIRST']['value'], '1')
        self.assertEqual(enumerators['FIRST']['description'], 'First value with extra space\n')
        
        # Check SECOND enumerator with multi-line comment
        self.assertEqual(enumerators['SECOND']['name'], 'SECOND')
        self.assertEqual(enumerators['SECOND']['value'], '2')
        self.assertEqual(
            enumerators['SECOND']['description'],
            '\nMulti-line\nenumerator description\n\n'
        )

    def test_enum_with_calculated_values(self):
        self.fs.create_file(
            "/fake/path/calculated_enum.f90",
            contents="""
module power_levels
    implicit none

    !!* Power level definitions *!
    enum, bind(c)
        enumerator :: LOW = 1
        enumerator :: MEDIUM = LOW * 10  ! Should be 10
        enumerator :: HIGH = MEDIUM * 10  ! Should be 100
        enumerator :: MAX = HIGH * 10    ! Should be 1000
    end enum
end module power_levels
"""
        )
        result = extract_module_data([Path('/fake/path/calculated_enum.f90')])
        module = result[0]
        
        # Get the enum
        enum = next(iter(module['enums'].values()))
        
        # Check that calculated values are preserved in the expressions
        self.assertEqual(enum['enumerators']['LOW']['value'], '1')
        self.assertEqual(enum['enumerators']['MEDIUM']['value'], 'LOW * 10')
        self.assertEqual(enum['enumerators']['HIGH']['value'], 'MEDIUM * 10')
        self.assertEqual(enum['enumerators']['MAX']['value'], 'HIGH * 10')

    def test_enum_with_multiple_enumerator_statements(self):
        self.fs.create_file(
            "/fake/path/multi_statement_enum.f90",
            contents="""\
    module multi_statement_enums
        implicit none

        !!* Testing multiple enumerator statements *!
        enum, bind(c)
            enumerator :: A = 1, B = 2
            enumerator :: C = 3, D = 4
            enumerator :: E = 5
        end enum
    end module multi_statement_enums
    """
        )
        result = extract_module_data([Path('/fake/path/multi_statement_enum.f90')])
        module = result[0]
        
        self.assertIn('enums', module)
        # Use A as the key (first enumerator)
        enum = module['enums']['A']
        
        self.assertEqual(enum['description'], 'Testing multiple enumerator statements\n')
        self.assertEqual(enum['name'], "__ENUM__")
        self.assertEqual(enum['attributes'], [])
        self.assertEqual(enum["binding_type"], { "type": BindingTypeEnum.BIND_C, "name": None})
        
        # Check enumerators
        self.assertEqual(len(enum['enumerators']), 5)
        enumerators = enum['enumerators']
        
        # Verify all enumerators across multiple statements
        expected_enumerators = {
            'A': '1', 
            'B': '2', 
            'C': '3', 
            'D': '4', 
            'E': '5'
        }
        
        for name, value in expected_enumerators.items():
            self.assertEqual(enumerators[name]['name'], name)
            self.assertEqual(enumerators[name]['value'], value)
            self.assertEqual(enumerators[name]['description'], '')  # No specific comments

    def test_enum_attribute_handling(self):
        self.fs.create_file(
            "/fake/path/enum_attributes.f90",
            contents="""\
    module attribute_test
        implicit none

        !!* Public enum values *!
        public :: WEEKDAY, WEEKEND  ! Make enum values public

        enum, bind(c)
            enumerator :: WEEKDAY = 1
            enumerator :: WEEKEND = 2
        end enum
    end module attribute_test
    """
        )
        result = extract_module_data([Path('/fake/path/enum_attributes.f90')])
        module = result[0]
        
        self.assertIn('enums', module)
        # Get the enum (using first enumerator name)
        enum = module['enums']['WEEKDAY']
        
        self.assertEqual(enum['description'], 'Public enum values\n')
        self.assertEqual(enum['name'], "__ENUM__")
        self.assertEqual(enum["binding_type"], { "type": BindingTypeEnum.BIND_C, "name": None})
        
        # Check enumerators (no specific attributes on the enum itself)
        self.assertEqual(len(enum['enumerators']), 2)
        enumerators = enum['enumerators']
        
        self.assertEqual(enumerators['WEEKDAY']['name'], 'WEEKDAY')
        self.assertEqual(enumerators['WEEKDAY']['value'], '1')
        
        self.assertEqual(enumerators['WEEKEND']['name'], 'WEEKEND')
        self.assertEqual(enumerators['WEEKEND']['value'], '2')


    def test_enum_various_syntaxes(self):
        self.fs.create_file(
            "/fake/path/enum_syntax_variations.f90",
            contents="""\
    module syntax_variations
        implicit none

        ! Compact syntax
        enum,bind(c)
            enumerator::A=1,B=2,C=3
        end enum

        ! Verbose syntax with extra whitespace
        enum  ,  bind  (  c  )
            enumerator  ::  D  =  4  ,  E  =  5
            enumerator  ::  F  =  6
        end  enum

        ! Capitalization variations
        ENUM, BIND(C)
            ENUMERATOR :: G = 7, H = 8
            Enumerator :: I = 9
        END ENUM
    end module syntax_variations
    """
        )
        result = extract_module_data([Path('/fake/path/enum_syntax_variations.f90')])
        module = result[0]
        
        self.assertIn('enums', module)
        self.assertEqual(len(module['enums']), 3)
        
        # Check first enum (compact syntax)
        first_enum = module['enums']['A']
        self.assertEqual(first_enum['name'], '__ENUM__')
        self.assertEqual(first_enum['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(len(first_enum['enumerators']), 3)
        
        # Check second enum (verbose syntax)
        second_enum = module['enums']['D']
        self.assertEqual(second_enum['name'], '__ENUM__')
        self.assertEqual(second_enum['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(len(second_enum['enumerators']), 3)
        
        # Check third enum (capitalization variations)
        third_enum = module['enums']['G']
        self.assertEqual(third_enum['name'], '__ENUM__')
        self.assertEqual(third_enum['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(len(third_enum['enumerators']), 3)
        
        # Collect all enumerators to check they're all correctly parsed
        all_enumerators = {}
        for enum in module['enums'].values():
            all_enumerators.update({name: details['value'] for name, details in enum['enumerators'].items()})
        
        # Check all enumerators are present with correct values
        expected = {
            'A': '1', 'B': '2', 'C': '3', 
            'D': '4', 'E': '5', 'F': '6',
            'G': '7', 'H': '8', 'I': '9'
        }
        
        self.assertEqual(len(all_enumerators), len(expected))
        for name, value in expected.items():
            self.assertEqual(all_enumerators[name], value)                         
if __name__ == '__main__':
    unittest.main()
