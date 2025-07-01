import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data
from doc4for.models.variable_models import PolymorphismType

class TestBlockDataParsing(TestCase):

    def setUp(self):
        self.setUpPyfakefs()
        
        # Create a fake Fortran file that all tests can access
        self.fs.create_file(
            "/fake/path/physics_constants.f90",
            contents="""\
            
!!* This file contains physical constants used throughout the simulation
! Using BLOCK DATA to initialize common block variables *!

!!* Contains physical constants used in calculations *!
block data physical_constants_init
  ! Initialize the common block variables  
  real(kind=8) :: gravity, pi, planck_constant  
  real(kind=8) :: speed_of_light, boltzmann_constant
  character(len=20) :: units_system
  real(kind=4) :: orphan
  !!* Common block containing physics constants *!
  common /physical_constants/ gravity, pi, planck_constant, &
                            speed_of_light, boltzmann_constant, units_system
  
  ! These values are initialized when the program starts
  data gravity /9.80665/                    ! m/s^2
  data pi /3.14159265358979323846264/      ! dimensionless
  data planck_constant /6.62607015E-34/    ! J⋅s
  data speed_of_light /299792458.0/        ! m/s
  data boltzmann_constant /1.380649E-23/   ! J/K
  data units_system /"SI_MKS"/             ! String identifier for units
  
end block data physical_constants_init

!!* 
! Calculates the kinetic energy, using the usual non-relativistic formula
! for low velocities and the relativistic formula if v > 0.1c
! @in mass  the mass of the object in kilograms
! @in velocity  the velocity of the object in m/s
! @inout kinetic_energy  the calculated kinetic energy
!*!
subroutine calculate_energy(mass, velocity, kinetic_energy)  
  ! Use the constants from the common block
  kinetic_energy = 0.5 * mass * velocity**2
  
  ! Special relativistic calculation if velocity is significant relative to c
  if (velocity > 0.1 * speed_of_light) then
    kinetic_energy = mass * speed_of_light**2 * &
                    (1.0/sqrt(1.0 - (velocity/speed_of_light)**2) - 1.0)
  endif
end subroutine calculate_energy

program physics_simulation
  implicit none
  
  real(kind=8) :: mass, velocity, energy
    
  mass = 1.0     ! kg
  velocity = 1000.0  ! m/s
  
  call calculate_energy(mass, velocity, energy)
  
  print *, "Using ", units_system, " units"
  print *, "Mass: ", mass, " kg"
  print *, "Velocity: ", velocity, " m/s" 
  print *, "Energy: ", energy, " J"
  print *, "Gravity: ", gravity, " m/s^2"
  
end program physics_simulation
"""
        )
        
        # Extract file data once for all tests
        self.result = extract_file_data([Path("/fake/path/physics_constants.f90")])
        self.file_data = self.result[0]
    
    def test_file_info(self):
        self.assertEqual(len(self.result), 1)
        self.assertEqual(self.file_data["file_name"], "/fake/path/physics_constants.f90")
        
    def test_block_data(self):
        self.assertIn("block_data", self.file_data)
        block_data = self.file_data["block_data"]
        self.assertEqual(len(block_data), 1)
        
        # Check block data name
        self.assertIn("physical_constants_init", block_data)
        constants_block = block_data["physical_constants_init"]

        # Test common blocks in block data
        self.assertIn("common_blocks", constants_block)
        common_blocks = constants_block["common_blocks"]
        self.assertIn("physical_constants", common_blocks)
        
        # Check that the orphaned variable is stored        
        self.assertEqual(len(constants_block["other_variables"]), 1)
        other_variable = constants_block["other_variables"]["orphan"]
        self.assertEqual(other_variable, {
            "description": "",
            "type": "REAL",
            "name": "orphan",
            "dimension": None,
            "polymorphism_type": PolymorphismType.NONE,
            "attributes": [],
            "kind": "4",
            "initial_value": None,
            "length": None,
            "binding_type": None,
            "is_saved": False
        })
        # Test physical_constants common block
        phys_block = common_blocks["physical_constants"]
        self.assertIn("variables", phys_block)
        variables = phys_block["variables"]
        self.assertEqual(len(variables), 6)  # Should have 6 variables

        # Check for binding_type (should be DEFAULT in this case)
        self.assertIn("binding_type", phys_block)
        self.assertIsNone(phys_block["binding_type"])

        # Check variable names
        var_names = variables.keys()
        expected_names = ["gravity", "pi", "planck_constant", 
                         "speed_of_light", "boltzmann_constant", "units_system"]
        self.assertListEqual(sorted(var_names), sorted(expected_names))
        
        # Test that "data" statements initialise the common block
        self.assertEqual(variables["gravity"]["initial_value"], "9.80665")
        self.assertEqual(variables["pi"]["initial_value"], "3.14159265358979323846264")
        self.assertEqual(variables["planck_constant"]["initial_value"], "6.62607015E-34")
        self.assertEqual(variables["speed_of_light"]["initial_value"], "299792458.0")
        self.assertEqual(variables["boltzmann_constant"]["initial_value"], "1.380649E-23")
        self.assertEqual(variables["units_system"]["initial_value"], '"SI_MKS"')


    def test_data_statements(self):
        block_data = self.file_data["block_data"]["physical_constants_init"]
        
        # Test 1: Data statements are correctly recorded
        self.assertIn("data_statements", block_data)
        data_stmts = block_data["data_statements"]
        self.assertEqual(len(data_stmts), 6)  # Should have 6 data statements
        
        # Check for specific data values in data statements
        gravity_data = next(d for d in data_stmts if d["variable"] == "gravity")
        self.assertEqual(gravity_data["value"], "9.80665")
        
        pi_data = next(d for d in data_stmts if d["variable"] == "pi")
        self.assertEqual(pi_data["value"], "3.14159265358979323846264")
        
        units_data = next(d for d in data_stmts if d["variable"] == "units_system")
        self.assertEqual(units_data["value"], '"SI_MKS"')
        
        # Test 2: Initial values are also propagated to the variable descriptions
        common_blocks = block_data["common_blocks"]
        phys_constants_block = common_blocks["physical_constants"]
        variables = phys_constants_block["variables"]
        
        # Check that initial values match the data statements
        self.assertEqual(variables["gravity"]["initial_value"], "9.80665")
        self.assertEqual(variables["pi"]["initial_value"], "3.14159265358979323846264")
        self.assertEqual(variables["units_system"]["initial_value"], '"SI_MKS"')
        
        # Verify variable types are preserved
        self.assertEqual(variables["gravity"]["type"], "REAL")
        self.assertEqual(variables["gravity"]["kind"], "8")
        self.assertEqual(variables["units_system"]["type"], "CHARACTER")
        self.assertEqual(variables["units_system"]["length"], "20")   

    def test_subroutines(self):
        self.assertIn("subroutines", self.file_data)
        subroutines = self.file_data["subroutines"]
        self.assertEqual(len(subroutines), 1)
        
        # Check subroutine name and properties
        self.assertIn("calculate_energy", subroutines)
        calc_energy = subroutines["calculate_energy"]
        
        # Test arguments
        self.assertIn("arguments", calc_energy)
        args = calc_energy["arguments"]
        self.assertEqual(len(args), 3)  
        self.assertListEqual(args, ["mass", "velocity", "kinetic_energy"])
        
        # Check argument intents - nothing declared so assume inout and can"t work out the type
        intent_in = calc_energy["in"]
        self.assertEqual(len(intent_in), 3)
        self.assertEqual(intent_in["mass"]["type"], "INTEGER")
        self.assertEqual(intent_in["mass"]["description"], "the mass of the object in kilograms")
        self.assertEqual(intent_in["velocity"]["type"], "REAL")        
        self.assertEqual(intent_in["velocity"]["description"], "the velocity of the object in m/s")
        self.assertEqual(intent_in["kinetic_energy"]["type"], "INTEGER")
        self.assertEqual(intent_in["kinetic_energy"]["description"], "the calculated kinetic energy")

        intent_out = calc_energy["out"]
        self.assertEqual(len(intent_out), 3)
        self.assertEqual(intent_out["mass"]["type"], "INTEGER")
        self.assertEqual(intent_out["mass"]["description"], "the mass of the object in kilograms")
        self.assertEqual(intent_out["velocity"]["type"], "REAL")        
        self.assertEqual(intent_out["velocity"]["description"], "the velocity of the object in m/s")
        self.assertEqual(intent_out["kinetic_energy"]["type"], "INTEGER")
        self.assertEqual(intent_out["kinetic_energy"]["description"], "the calculated kinetic energy")

        # Check binding type
        self.assertIsNone(calc_energy["binding_type"])

    def test_program(self):
        """Test program extraction"""
        self.assertIn("programs", self.file_data)
        programs = self.file_data["programs"]
        self.assertEqual(len(programs), 1)
        
        # Check program name
        self.assertIn("physics_simulation", programs)
        simulation = programs["physics_simulation"]
        
        # Check program details
        self.assertEqual(simulation["file_name"], "/fake/path/physics_constants.f90")
        self.assertEqual(simulation["program_name"], "physics_simulation")
        self.assertEqual(simulation["program_description"], "")
    

if __name__ == "__main__":
    unittest.main()