 module gas_physics
 contains
   function viscosity(temperature)
     real :: viscosity, temperature
     viscosity = 2.0e-3 * temperature**1.5
   end function
 end module 
