library IEEE;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

use work.riscv_klessydra.all;
use work.thread_parameters_klessydra.all;

package TMR_REG_PKG is

function tmr_assign(input_vector: in std_logic_vector(31 downto 0);input_size: in integer := 32)
    return std_logic_vector;

function tmr_bit_assign(input_vector: in std_logic_vector;input_size: in integer := 32;bit_assign: in integer;bit_value: in std_logic)
    return std_logic_vector; 
	
function tmr_FSM_assign(input_vector: in std_logic_vector;input_size: in integer := 32)
    return std_logic_vector;

function tmr_FSM_b_assign(input_vector: in std_logic)
    return std_logic_vector;
	
function tmr_voting(input_vector: in std_logic_vector;input_size: in integer)
        return std_logic_vector;
		
function tmr_b_voting(input_vector: in std_logic_vector)
		return std_logic;
  
end  package TMR_REG_PKG;

package body TMR_REG_PKG is
         
     function tmr_assign(input_vector: in std_logic_vector(31 downto 0);input_size: in integer := 32) return std_logic_vector is
     
        variable tmr_var : std_logic_vector((3*input_size)-1 downto 0);
     
     begin
	 
        tmr_var((3*input_size)-1 downto ((3*input_size) - input_size)) := input_vector;
        tmr_var((2*input_size)-1 downto ((2*input_size) - input_size)) := input_vector;
        tmr_var((input_size-1) downto 0) := input_vector;
        
        return std_logic_vector(tmr_var);
        
     end function;
     
	 function tmr_bit_assign(input_vector: in std_logic_vector;input_size: in integer := 32;bit_assign: in integer;bit_value: in std_logic) return std_logic_vector is
     
        variable tmr_var : std_logic_vector((3*input_size)-1 downto 0);
        variable input_int : std_logic_vector(input_size-1 downto 0);
     begin
        input_int := input_vector;
		input_int(bit_assign) := bit_value;
		
        tmr_var((3*input_size)-1 downto ((3*input_size) - input_size)) := input_int;
        tmr_var((2*input_size)-1 downto ((2*input_size) - input_size)) := input_int;
        tmr_var((input_size-1) downto 0) := input_int;
        
        return std_logic_vector(tmr_var);
        
     end function;
	 
	 function tmr_FSM_assign(input_vector: in std_logic_vector;input_size: in integer := 32) return std_logic_vector is
     
        variable tmr_var : std_logic_vector((3*input_size)-1 downto 0);
		
     begin
	 
        tmr_var((3*input_size)-1 downto ((3*input_size) - input_size)) := input_vector;
        tmr_var((2*input_size)-1 downto ((2*input_size) - input_size)) := input_vector;
        tmr_var((input_size-1) downto 0) := input_vector;
        
        return std_logic_vector(tmr_var);
        
     end function;
	 
	 function tmr_FSM_b_assign(input_vector: in std_logic) return std_logic_vector is
		
		variable tmr_var : std_logic_vector(2 downto 0);
		
     begin
	 
        tmr_var(2) := input_vector;
        tmr_var(1) := input_vector;
        tmr_var(0) := input_vector;
        
        return std_logic_vector(tmr_var);
	 end function;
	 
     function tmr_voting(input_vector: in std_logic_vector;input_size: in integer) return std_logic_vector is
     
        variable tmr_var : std_logic_vector(input_size-1 downto 0);
     
     begin
        
        tmr_var := (input_vector((3*input_size)-1 downto ((3*input_size) - input_size)) and input_vector((2*input_size)-1 downto ((2*input_size) - input_size))) 
				or  (input_vector((3*input_size)-1 downto ((3*input_size) - input_size)) and input_vector((input_size-1) downto 0)) 
				or (input_vector((2*input_size)-1 downto ((2*input_size) - input_size)) and input_vector((input_size-1) downto 0));

        return std_logic_vector(tmr_var);
        
     end function;
	 
	 function tmr_b_voting(input_vector: in std_logic_vector) return std_logic is
     
        variable tmr_var : std_logic;
     
     begin
        
        tmr_var := (input_vector(2) and input_vector(1)) 
                or (input_vector(2) and input_vector(0)) 
                or (input_vector(1) and input_vector(0));

        return std_logic(tmr_var);
        
     end function;
     
     
end package body TMR_REG_PKG;






