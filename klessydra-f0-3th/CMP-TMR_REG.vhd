library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

use work.riscv_klessydra.all;
use work.thread_parameters_klessydra.all;

-- Component Declaration

-- component TMR_REG is
	-- generic (DataWidth : integer );
	-- port (
		-- tmr_reg_i_en				: in std_logic;
		-- tmr_reg_i_clk			: in std_logic;
		-- tmr_reg_i_data			: in std_logic_vector(DataWidth-1 downto 0);
		-- tmr_reg_i_sync_rst_n		: in std_logic;
		-- tmr_reg_i_async_rst_n	: in std_logic;
		-- tmr_reg_o_data			: out std_logic_vector(DataWidth-1 downto 0)
	-- );
-- end component;

-- Component Port map

-- TMR_CMP_"signal_name" : TMR_REG
	-- generic map ( DataWidth => DataWidth )
	-- port map (
		-- tmr_reg_i_en				=>	'1',
		-- tmr_reg_i_clk			=>	clk_i,
		-- tmr_reg_i_data			=>	"signal_input_name",
		-- tmr_reg_i_sync_rst_n		=>	'1',
		-- tmr_reg_i_async_rst_n	=>	'1',
		-- tmr_reg_o_data			=>	"signal_output_name"
	-- );



entity TMR_REG is
	generic (DataWidth : integer := 32 );
	port (
		tmr_reg_i_en			: in std_logic;
		tmr_reg_i_clk			: in std_logic;
		tmr_reg_i_data			: in std_logic_vector(DataWidth-1 downto 0);
		tmr_reg_i_sync_rst_n		: in std_logic;
		tmr_reg_i_async_rst_n		: in std_logic;
		tmr_reg_o_data			: out std_logic_vector(DataWidth-1 downto 0)
	);
end entity;



architecture TMR of TMR_REG is

	signal tmr_reg_o_int_data_A	: std_logic_vector(DataWidth-1 downto 0);
	signal tmr_reg_o_int_data_B	: std_logic_vector(DataWidth-1 downto 0);
	signal tmr_reg_o_int_data_C	: std_logic_vector(DataWidth-1 downto 0);
	
----------------------------------------------------------------------------------------
--- Keep Attribute: prevent optimization
----------------------------------------------------------------------------------------	
	
--	attribute keep : string;
--	attribute keep of tmr_reg_o_int_data_A :signal is "true";
--	attribute keep of tmr_reg_o_int_data_B :signal is "true";
--	attribute keep of tmr_reg_o_int_data_C :signal is "true";
	
	begin
	
	tmr_reg_update : process(tmr_reg_i_clk,tmr_reg_i_async_rst_n)
	begin
			if tmr_reg_i_async_rst_n = '0' then
				tmr_reg_o_int_data_A <= tmr_reg_i_data;
				tmr_reg_o_int_data_B <= tmr_reg_i_data;
				tmr_reg_o_int_data_C <= tmr_reg_i_data;
			elsif ( rising_edge(tmr_reg_i_clk) and tmr_reg_i_en = '1' ) then
				if tmr_reg_i_sync_rst_n = '0' then
					tmr_reg_o_int_data_A <= (others => '0');
					tmr_reg_o_int_data_B <= (others => '0');
					tmr_reg_o_int_data_C <= (others => '0');
				else
					tmr_reg_o_int_data_A <= tmr_reg_i_data;
					tmr_reg_o_int_data_B <= tmr_reg_i_data;
					tmr_reg_o_int_data_C <= tmr_reg_i_data;
				end if;
			end if;
	end process;
	
	tmr_reg_voter : process(all)
	begin
		tmr_reg_o_data <= (tmr_reg_o_int_data_A and tmr_reg_o_int_data_B) or (tmr_reg_o_int_data_A and tmr_reg_o_int_data_C) or (tmr_reg_o_int_data_B and tmr_reg_o_int_data_C);
	end process;

end TMR;
	
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

use work.riscv_klessydra.all;
use work.thread_parameters_klessydra.all;	
		
entity TMR_iREG is
	port (
		tmr_ireg_i_en			: in std_logic;
		tmr_ireg_i_clk			: in std_logic;
		tmr_ireg_i_data			: in harc_range;
		tmr_ireg_i_sync_rst_n		: in std_logic;
		tmr_ireg_i_async_rst_n		: in std_logic;
		tmr_ireg_o_data			: out harc_range
	);
end entity;



architecture iTMR of TMR_iREG is

	signal tmr_ireg_o_int_data_A	: harc_range := 0;
	signal tmr_ireg_o_int_data_B	: harc_range := 0;
	signal tmr_ireg_o_int_data_C	: harc_range := 0;
	signal tmr_ireg_o_std_int_data_A	:std_logic_vector(1 downto 0):= (others => '0');
	signal tmr_ireg_o_std_int_data_B	:std_logic_vector(1 downto 0):= (others => '0');
	signal tmr_ireg_o_std_int_data_C	:std_logic_vector(1 downto 0):= (others => '0');
----------------------------------------------------------------------------------------
--- Keep Attribute: prevent optimization
----------------------------------------------------------------------------------------	
	
--	attribute keep : string;
--	attribute keep of tmr_ireg_o_int_data_A :signal is "true";
--	attribute keep of tmr_ireg_o_int_data_B :signal is "true";
--	attribute keep of tmr_ireg_o_int_data_C :signal is "true";
	
	begin
	tmr_ireg_o_std_int_data_A <= std_logic_vector(to_unsigned(tmr_ireg_o_int_data_A,2));
	tmr_ireg_o_std_int_data_B <= std_logic_vector(to_unsigned(tmr_ireg_o_int_data_B,2));
	tmr_ireg_o_std_int_data_C <= std_logic_vector(to_unsigned(tmr_ireg_o_int_data_C,2));
	
	tmr_ireg_update : process(tmr_ireg_i_clk,tmr_ireg_i_sync_rst_n,tmr_ireg_i_async_rst_n)
	begin
			if tmr_ireg_i_async_rst_n = '0' then
				tmr_ireg_o_int_data_A <= 0;
				tmr_ireg_o_int_data_B <= 0;
				tmr_ireg_o_int_data_C <= 0;
			elsif ( rising_edge(tmr_ireg_i_clk) and tmr_ireg_i_en = '1' ) then
				if tmr_ireg_i_sync_rst_n = '0' then
					tmr_ireg_o_int_data_A <= 0;
					tmr_ireg_o_int_data_B <= 0;
					tmr_ireg_o_int_data_C <= 0;
				else
					tmr_ireg_o_int_data_A <= tmr_ireg_i_data;
					tmr_ireg_o_int_data_B <= tmr_ireg_i_data;
					tmr_ireg_o_int_data_C <= tmr_ireg_i_data;
				end if;
			end if;
	end process;
	
	tmr_ireg_voter : process(all)
	begin
		tmr_ireg_o_data <= to_integer(unsigned((tmr_ireg_o_std_int_data_A and tmr_ireg_o_std_int_data_B)
											   or (tmr_ireg_o_std_int_data_A and tmr_ireg_o_std_int_data_C)
											   or (tmr_ireg_o_std_int_data_B and tmr_ireg_o_std_int_data_C)));
	end process;

end iTMR;				

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

use work.riscv_klessydra.all;
use work.thread_parameters_klessydra.all;	
		
entity TMR_iREG_arst is
	port (
		tmr_ireg_i_en			: in std_logic;
		tmr_ireg_i_clk			: in std_logic;
		tmr_ireg_i_data			: in harc_range;
		tmr_ireg_i_sync_rst_n	: in std_logic;
		tmr_ireg_i_async_rst_n	: in std_logic;
		tmr_ireg_o_data			: out harc_range
	);
end entity;



architecture iTMR_arst of TMR_iREG_arst is

	signal tmr_ireg_o_int_data_A	: harc_range := 0;
	signal tmr_ireg_o_int_data_B	: harc_range := 0;
	signal tmr_ireg_o_int_data_C	: harc_range := 0;
	signal tmr_ireg_o_std_int_data_A	:std_logic_vector(1 downto 0):= (others => '0');
	signal tmr_ireg_o_std_int_data_B	:std_logic_vector(1 downto 0):= (others => '0');
	signal tmr_ireg_o_std_int_data_C	:std_logic_vector(1 downto 0):= (others => '0');
----------------------------------------------------------------------------------------
--- Keep Attribute: prevent optimization
----------------------------------------------------------------------------------------	
	
--	attribute keep : string;
--	attribute keep of tmr_ireg_o_int_data_A :signal is "true";
--	attribute keep of tmr_ireg_o_int_data_B :signal is "true";
--	attribute keep of tmr_ireg_o_int_data_C :signal is "true";
	
	begin
	tmr_ireg_o_std_int_data_A <= std_logic_vector(to_unsigned(tmr_ireg_o_int_data_A,2));
	tmr_ireg_o_std_int_data_B <= std_logic_vector(to_unsigned(tmr_ireg_o_int_data_B,2));
	tmr_ireg_o_std_int_data_C <= std_logic_vector(to_unsigned(tmr_ireg_o_int_data_C,2));
	
	tmr_ireg_update : process(tmr_ireg_i_clk,tmr_ireg_i_sync_rst_n,tmr_ireg_i_async_rst_n)
	begin
			if tmr_ireg_i_async_rst_n = '0' then
					tmr_ireg_o_int_data_A <= tmr_ireg_i_data;
					tmr_ireg_o_int_data_B <= tmr_ireg_i_data;
					tmr_ireg_o_int_data_C <= tmr_ireg_i_data;
			elsif ( rising_edge(tmr_ireg_i_clk) and tmr_ireg_i_en = '1' ) then
				if tmr_ireg_i_sync_rst_n = '0' then
					tmr_ireg_o_int_data_A <= 0;
					tmr_ireg_o_int_data_B <= 0;
					tmr_ireg_o_int_data_C <= 0;
				else
					tmr_ireg_o_int_data_A <= tmr_ireg_i_data;
					tmr_ireg_o_int_data_B <= tmr_ireg_i_data;
					tmr_ireg_o_int_data_C <= tmr_ireg_i_data;
				end if;
			end if;
	end process;
	
	tmr_ireg_voter : process(all)
	begin
		tmr_ireg_o_data <= to_integer(unsigned((tmr_ireg_o_std_int_data_A and tmr_ireg_o_std_int_data_B)
											   or (tmr_ireg_o_std_int_data_A and tmr_ireg_o_std_int_data_C)
											   or (tmr_ireg_o_std_int_data_B and tmr_ireg_o_std_int_data_C)));
	end process;

end iTMR_arst;



			
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

use work.riscv_klessydra.all;
use work.thread_parameters_klessydra.all;

-- Component Declaration

-- component TMR_REG is
	-- generic (DataWidth : integer );
	-- port (
		-- tmr_reg_i_en				: in std_logic;
		-- tmr_reg_i_clk			: in std_logic;
		-- tmr_reg_i_data			: in std_logic_vector(DataWidth-1 downto 0);
		-- tmr_reg_i_sync_rst_n		: in std_logic;
		-- tmr_reg_i_async_rst_n	: in std_logic;
		-- tmr_reg_o_data			: out std_logic_vector(DataWidth-1 downto 0)
	-- );
-- end component;

-- Component Port map

-- TMR_CMP_"signal_name" : TMR_REG
	-- generic map ( DataWidth => DataWidth )
	-- port map (
		-- tmr_reg_i_en				=>	'1',
		-- tmr_reg_i_clk			=>	clk_i,
		-- tmr_reg_i_data			=>	"signal_input_name",
		-- tmr_reg_i_sync_rst_n		=>	'1',
		-- tmr_reg_i_async_rst_n	=>	'1',
		-- tmr_reg_o_data			=>	"signal_output_name"
	-- );



entity TMR_REG_rst_0 is
	generic (DataWidth : integer := 32 );
	port (
		tmr_reg_i_en			: in std_logic;
		tmr_reg_i_clk			: in std_logic;
		tmr_reg_i_data			: in std_logic_vector(DataWidth-1 downto 0);
		tmr_reg_i_sync_rst_n		: in std_logic;
		tmr_reg_i_async_rst_n		: in std_logic;
		tmr_reg_o_data			: out std_logic_vector(DataWidth-1 downto 0)
	);
end entity;



architecture TMRr0 of TMR_REG_rst_0 is

	signal tmr_reg_o_int_data_A	: std_logic_vector(DataWidth-1 downto 0);
	signal tmr_reg_o_int_data_B	: std_logic_vector(DataWidth-1 downto 0);
	signal tmr_reg_o_int_data_C	: std_logic_vector(DataWidth-1 downto 0);
	
----------------------------------------------------------------------------------------
--- Keep Attribute: prevent optimization
----------------------------------------------------------------------------------------	
	
--	attribute keep : string;
--	attribute keep of tmr_reg_o_int_data_A :signal is "true";
--	attribute keep of tmr_reg_o_int_data_B :signal is "true";
--	attribute keep of tmr_reg_o_int_data_C :signal is "true";
	
	begin
	
	tmr_reg_update : process(tmr_reg_i_clk,tmr_reg_i_async_rst_n)
	begin
			if tmr_reg_i_async_rst_n = '0' then
				tmr_reg_o_int_data_A <= (others => '0');
				tmr_reg_o_int_data_B <= (others => '0');
				tmr_reg_o_int_data_C <= (others => '0');
			elsif ( rising_edge(tmr_reg_i_clk) and tmr_reg_i_en = '1' ) then
				if tmr_reg_i_sync_rst_n = '0' then
					tmr_reg_o_int_data_A <= (others => '0');
					tmr_reg_o_int_data_B <= (others => '0');
					tmr_reg_o_int_data_C <= (others => '0');
				else
					tmr_reg_o_int_data_A <= tmr_reg_i_data;
					tmr_reg_o_int_data_B <= tmr_reg_i_data;
					tmr_reg_o_int_data_C <= tmr_reg_i_data;
				end if;
			end if;
	end process;
	
	tmr_reg_voter : process(all)
	begin
		tmr_reg_o_data <= (tmr_reg_o_int_data_A and tmr_reg_o_int_data_B) or (tmr_reg_o_int_data_A and tmr_reg_o_int_data_C) or (tmr_reg_o_int_data_B and tmr_reg_o_int_data_C);
	end process;

end TMRr0;			

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

use work.riscv_klessydra.all;
use work.thread_parameters_klessydra.all;

entity TMR_sREG is
	port (
		tmr_sreg_i_en			: in std_logic;
		tmr_sreg_i_clk			: in std_logic;
		tmr_sreg_i_data			: in std_logic;
		tmr_sreg_i_sync_rst_n	: in std_logic;
		tmr_sreg_i_async_rst_n	: in std_logic;
		tmr_sreg_o_data			: out std_logic
	);
end entity;



architecture sTMR of TMR_sREG is

	signal tmr_sreg_o_int_data_A	: std_logic;
	signal tmr_sreg_o_int_data_B	: std_logic;
	signal tmr_sreg_o_int_data_C	: std_logic;
	
----------------------------------------------------------------------------------------
--- Keep Attribute: prevent optimization
----------------------------------------------------------------------------------------	
	
--	attribute keep : string;
--	attribute keep of tmr_sreg_o_int_data_A :signal is "true";
--	attribute keep of tmr_sreg_o_int_data_B :signal is "true";
--	attribute keep of tmr_sreg_o_int_data_C :signal is "true";
	
	begin
	
	tmr_sreg_update : process(tmr_sreg_i_clk,tmr_sreg_i_sync_rst_n,tmr_sreg_i_async_rst_n)
	begin
			if tmr_sreg_i_async_rst_n = '0' then
				tmr_sreg_o_int_data_A <= '0';
				tmr_sreg_o_int_data_B <= '0';
				tmr_sreg_o_int_data_C <= '0';
			elsif ( rising_edge(tmr_sreg_i_clk) and tmr_sreg_i_en = '1' ) then
				if tmr_sreg_i_sync_rst_n = '0' then
					tmr_sreg_o_int_data_A <= '0';
					tmr_sreg_o_int_data_B <= '0';
					tmr_sreg_o_int_data_C <= '0';
				else
					tmr_sreg_o_int_data_A <= tmr_sreg_i_data;
					tmr_sreg_o_int_data_B <= tmr_sreg_i_data;
					tmr_sreg_o_int_data_C <= tmr_sreg_i_data;
				end if;
			end if;
	end process;
	
	tmr_sreg_voter : process(all)
	begin
		tmr_sreg_o_data <= (tmr_sreg_o_int_data_A and tmr_sreg_o_int_data_B) or (tmr_sreg_o_int_data_A and tmr_sreg_o_int_data_C) or (tmr_sreg_o_int_data_B and tmr_sreg_o_int_data_C);
	end process;

end sTMR;				
