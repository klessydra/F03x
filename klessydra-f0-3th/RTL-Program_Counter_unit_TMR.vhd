-- RTL_Program_counter_unit_TMR
-- Version:		0.2.0
-- all registers TMR




library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

use work.riscv_klessydra.all;
use work.thread_parameters_klessydra.all;


entity Program_Counter is
  port (
    absolute_jump                     : in  std_logic;
    data_we_o_lat                     : in  std_logic;
    PC_offset                         : in  replicated_32b_reg;
    taken_branch                      : in  std_logic;
    set_branch_condition              : in  std_logic;
    set_except_condition              : in  std_logic;
    set_mret_condition                : in  std_logic;
    set_wfi_condition                 : in  std_logic;
    harc_ID                           : in  harc_range;
    harc_IE                           : in  harc_range;
    instr_rvalid_IE                   : in  std_logic;
    pc_IE                             : in  std_logic_vector(31 downto 0);
    MIP, MEPC, MSTATUS, MCAUSE, MTVEC : in  replicated_32b_reg;
    instr_word_IE                     : in  std_logic_vector(31 downto 0);
    reset_state                       : in  std_logic;
    pc_IF                             : out std_logic_vector(31 downto 0);
    harc_IF                           : out harc_range;
    served_except_condition           : out replicated_bit;
    served_mret_condition             : out replicated_bit;
    served_irq                        : in  replicated_bit;
	taken_branch_pending              : out replicated_bit; 
    taken_branch_pc_lat               : out replicated_32b_reg;
    incremented_pc                    : out replicated_32b_reg;
    mepc_incremented_pc               : out replicated_32b_reg;
    mepc_interrupt_pc                 : out replicated_32b_reg;
    irq_pending                       : out replicated_bit;
    branch_condition_pending          : out replicated_bit;
    --except_condition_pending          : out replicated_bit;
    --mret_condition_pending            : out replicated_bit;
    clk_i                             : in  std_logic;
    rst_ni                            : in  std_logic;
    irq_i                             : in  std_logic;
    fetch_enable_i                    : in  std_logic;
    boot_addr_i                       : in  std_logic_vector(31 downto 0);
    instr_gnt_i                       : in  std_logic
    );
end entity;



architecture PC of Program_counter is

  component TMR_REG_rst_0 is
	generic (DataWidth : integer );
	port (
		tmr_reg_i_en			: in std_logic;
		tmr_reg_i_clk			: in std_logic;
		tmr_reg_i_data			: in std_logic_vector(DataWidth-1 downto 0);
		tmr_reg_i_sync_rst_n	: in std_logic;
		tmr_reg_i_async_rst_n	: in std_logic;
		tmr_reg_o_data			: out std_logic_vector(DataWidth-1 downto 0)
	);
  end component;
  
  component TMR_sREG is
	port (
		tmr_sreg_i_en			: in std_logic;
		tmr_sreg_i_clk			: in std_logic;
		tmr_sreg_i_data			: in std_logic;
		tmr_sreg_i_sync_rst_n	: in std_logic;
		tmr_sreg_i_async_rst_n	: in std_logic;
		tmr_sreg_o_data			: out std_logic
	);
  end component;

  component TMR_iREG_arst is
	port (
		tmr_ireg_i_en			: in std_logic;
		tmr_ireg_i_clk			: in std_logic;
		tmr_ireg_i_data			: in harc_range;
		tmr_ireg_i_sync_rst_n	: in std_logic;
		tmr_ireg_i_async_rst_n	: in std_logic;
		tmr_ireg_o_data			: out harc_range
	);
  end component;

  signal served_mret_condition_wire		: replicated_bit;
  signal served_except_condition_wire	: replicated_bit;
  signal wfi_condition_pending_wire		: replicated_bit;
	
  signal served_except_condition_lat     : std_logic;
  signal served_except_condition_lat_wire : std_logic;
  signal pc_update_enable                : replicated_bit;
  signal wfi_condition_pending           : replicated_bit;
  signal taken_branch_replicated         : replicated_bit;
  signal set_branch_condition_replicated : replicated_bit;
  signal set_wfi_condition_replicated    : replicated_bit;
  signal set_except_condition_replicated : replicated_bit;
  signal set_mret_condition_replicated   : replicated_bit;
  signal relative_to_PC                  : replicated_32b_reg;
  signal pc                              : replicated_32b_reg;
  signal pc_wire						 : replicated_32b_reg;
  signal boot_pc                         : std_logic_vector(31 downto 0);

  signal harc_IF_internal                  : harc_range;
  signal harc_IF_wire					   : harc_range;
  signal harc_IF_comb                      : harc_range;
  --signal mret_condition_pending_internal   : replicated_bit;
  signal mepc_incremented_pc_internal      : replicated_32b_reg;
  signal incremented_pc_internal           : replicated_32b_reg;
  signal mepc_interrupt_pc_internal        : replicated_32b_reg;
  signal taken_branch_pc_lat_internal      : replicated_32b_reg;
  signal taken_branch_pending_internal     : replicated_bit;
  signal taken_branch_pending_wire		   : replicated_bit;
  --signal except_condition_pending_internal : replicated_bit;
  signal irq_pending_internal              : replicated_bit;

  procedure pc_update(

    signal taken_branch 			 : in    std_logic;
    signal wfi_condition_pending     : out 	 std_logic;
    signal set_wfi_condition         : in    std_logic;
    signal taken_branch_pending      : in	 std_logic;
	signal taken_branch_pending_wire : out 	 std_logic;
    signal set_except_condition      : in    std_logic;
    signal set_mret_condition        : in    std_logic;
    signal pc                        : out 	 std_logic_vector(31 downto 0);
    signal taken_branch_pc_lat       : in    std_logic_vector(31 downto 0);
    signal incremented_pc            : in    std_logic_vector(31 downto 0);
    signal boot_pc                   : in    std_logic_vector(31 downto 0);
    signal pc_update_enable          : in    std_logic;
    signal served_except_condition   : out   std_logic;
    signal served_mret_condition     : out   std_logic) is
  begin
    if pc_update_enable = '1' then

      if taken_branch = '0' and taken_branch_pending = '0' then
        pc                      	<= incremented_pc;
        served_except_condition 	<= '0';
        served_mret_condition   	<= '0';
		taken_branch_pending_wire 	<= '0';
      elsif taken_branch = '1' or taken_branch_pending = '1' then
        pc                      		<= taken_branch_pc_lat;
        taken_branch_pending_wire    	<= '0';
        served_except_condition 		<= '1' when set_except_condition = '1' else '0';
        served_mret_condition   		<= '1' when set_mret_condition = '1' else '0';
      else
        pc <= boot_pc;
		served_except_condition 	<= '0';
        served_mret_condition   	<= '0';
		taken_branch_pending_wire 	<= '0';
      end if;
    else
      served_except_condition <= '0';
      served_mret_condition   <= '0';
	  wfi_condition_pending   <= '0';
      if taken_branch = '1' then
        taken_branch_pending_wire <= '1';
	  else
		taken_branch_pending_wire <= taken_branch_pending;
      end if;
      if set_except_condition = '1' then
        served_except_condition <= '1';
      end if;
      if set_mret_condition = '1' then
        served_mret_condition <= '1';
      end if;
      if set_wfi_condition = '1' then
         wfi_condition_pending <= '1';
      end if;
    end if;
  end pc_update;

begin
  branch_condition_pending <= (others => '0');
  harc_IF                  <= harc_IF_internal;
  --mret_condition_pending   <= mret_condition_pending_internal;
  mepc_incremented_pc      <= mepc_incremented_pc_internal;
  mepc_interrupt_pc        <= mepc_interrupt_pc_internal;
  taken_branch_pc_lat      <= taken_branch_pc_lat_internal;
  incremented_pc           <= incremented_pc_internal;
  taken_branch_pending     <= taken_branch_pending_internal;
  --except_condition_pending <= except_condition_pending_internal;
  irq_pending              <= irq_pending_internal;

  TMR_CMP_harc_IF : TMR_iREG_arst
	port map (
		tmr_ireg_i_en			=>	instr_gnt_i,
		tmr_ireg_i_clk			=>	clk_i,
		tmr_ireg_i_data			=>	harc_IF_wire,
		tmr_ireg_i_sync_rst_n	=>	'1',
		tmr_ireg_i_async_rst_n	=>	rst_ni,
		tmr_ireg_o_data			=>	harc_IF_internal
	);

	TMR_CMP_served_except_condition_lat : TMR_sREG
		port map (
			tmr_sreg_i_en			=>	'1',
			tmr_sreg_i_clk			=>	clk_i,
			tmr_sreg_i_data			=>	served_except_condition_lat_wire,
			tmr_sreg_i_sync_rst_n	=>	'1',
			tmr_sreg_i_async_rst_n	=>	rst_ni,
			tmr_sreg_o_data			=>	served_except_condition_lat
		);
	
  hardware_context_counter : process(all)
  begin
      
	  if instr_gnt_i = '1' and rst_ni = '1' then
        harc_IF_wire <= harc_IF_internal - 1 when harc_IF_internal > 0 else THREAD_POOL_SIZE -1;
      elsif instr_gnt_i = '0' and rst_ni = '1' then
	    harc_IF_wire <= harc_IF_internal - 1 when harc_IF_internal > 0 else THREAD_POOL_SIZE -1;
	  else
	    harc_IF_wire <= THREAD_POOL_SIZE -1;
	  end if;
	  
  end process hardware_context_counter;

  pc_IF <= pc(harc_IF_internal);

  boot_pc                               <= boot_addr_i(31 downto 8) & std_logic_vector(to_unsigned(128, 8));
  --mepc_incremented_pc_internal(h) 		<= MEPC(harc_IE);
  --mepc_interrupt_pc_internal(harc_IE)   <= MEPC(harc_IE) when MCAUSE(harc_IE)(30) = '0' else std_logic_vector(unsigned(MEPC(harc_IE)) + 4);
  
  
  pc_update_logic : for h in harc_range generate
	
	
	mepc_incremented_pc_internal(h) 		<= MEPC(harc_IE) when h = harc_IE else (others => '0');
	mepc_interrupt_pc_internal(h)   <= 	MEPC(harc_IE) 									when (MCAUSE(harc_IE)(30) = '0' and h = harc_IE) else 
										std_logic_vector(unsigned(MEPC(harc_IE)) + 4) 	when (MCAUSE(harc_IE)(30) = '1' and h = harc_IE) else
										(others => '0');
										
	TMR_CMP_served_except_condition : TMR_sREG
		port map (
			tmr_sreg_i_en			=>	'1',
			tmr_sreg_i_clk			=>	clk_i,
			tmr_sreg_i_data			=>	served_except_condition_wire(h),
			tmr_sreg_i_sync_rst_n	=>	'1',
			tmr_sreg_i_async_rst_n	=>	rst_ni,
			tmr_sreg_o_data			=>	served_except_condition(h)
		);
		
	TMR_CMP_served_mret_condition : TMR_sREG
		port map (
			tmr_sreg_i_en			=>	'1',
			tmr_sreg_i_clk			=>	clk_i,
			tmr_sreg_i_data			=>	served_mret_condition_wire(h),
			tmr_sreg_i_sync_rst_n	=>	'1',
			tmr_sreg_i_async_rst_n	=>	rst_ni,
			tmr_sreg_o_data			=>	served_mret_condition(h)
		);
		
	TMR_CMP_taken_branch_pending : TMR_sREG
		port map (
			tmr_sreg_i_en			=>	'1',
			tmr_sreg_i_clk			=>	clk_i,
			tmr_sreg_i_data			=>	taken_branch_pending_wire(h),
			tmr_sreg_i_sync_rst_n	=>	'1',
			tmr_sreg_i_async_rst_n	=>	rst_ni,
			tmr_sreg_o_data			=>	taken_branch_pending_internal(h)
		);

	TMR_CMP_wfi_condition_pending : TMR_sREG
		port map (
			tmr_sreg_i_en			=>	'1',
			tmr_sreg_i_clk			=>	clk_i,
			tmr_sreg_i_data			=>	wfi_condition_pending_wire(h),
			tmr_sreg_i_sync_rst_n	=>	'1',
			tmr_sreg_i_async_rst_n	=>	rst_ni,
			tmr_sreg_o_data			=>	wfi_condition_pending(h)
		);
	
	TMR_CMP_pc : TMR_REG_rst_0
		generic map ( DataWidth => 32 )
		port map (
			tmr_reg_i_en			=>	pc_update_enable(h),
			tmr_reg_i_clk			=>	clk_i,
			tmr_reg_i_data			=>	pc_wire(h),
			tmr_reg_i_sync_rst_n	=>	'1',
			tmr_reg_i_async_rst_n	=>	rst_ni,
			tmr_reg_o_data			=>	pc(h)
		);	
	
	

    relative_to_PC(h) <= std_logic_vector(to_unsigned(0, 32)) when (absolute_jump = '1')
                         else pc_IE;
    incremented_pc_internal(h) <= std_logic_vector(unsigned(pc(h))+4);
    irq_pending_internal(h)    <= ((MIP(h)(11) or MIP(h)(7) or MIP(h)(3)) and MSTATUS(h)(3));

    set_wfi_condition_replicated(h) <= '1' when set_wfi_condition = '1' and (harc_IE = h)
                                       else '0';
    taken_branch_replicated(h) <= '1' when taken_branch = '1' and (harc_IE = h)
                                  else '0';
    set_branch_condition_replicated(h) <= '1' when set_branch_condition = '1' and (harc_IE = h)
                                          else '0';
    set_except_condition_replicated(h) <= '1' when set_except_condition = '1' and (harc_IE = h)
                                          else '0';
    set_mret_condition_replicated(h) <= '1' when set_mret_condition = '1' and (harc_IE = h)
                                        else '0';


    taken_branch_pc_lat_internal(h) <=
      std_logic_vector(signed(relative_to_PC(h))+signed(PC_offset(h))) when set_branch_condition_replicated(h) = '1'                       else
      std_logic_vector(signed(relative_to_PC(h)))                      when set_wfi_condition_replicated(h) = '1'                           else
      MTVEC(h)                                                         when set_except_condition_replicated(h) = '1'                       else
      mepc_incremented_pc_internal(h)                                  when set_mret_condition_replicated(h) = '1' and MCAUSE(h)(31) = '0' else
      mepc_interrupt_pc_internal(h)                                    when set_mret_condition_replicated(h) = '1' and MCAUSE(h)(31) = '1' else
      MTVEC(h)                                                         when served_irq(h);


    pc_update_enable(h) <= '1' when (instr_gnt_i = '1'
                           and (harc_IF_internal = h
                                or taken_branch_replicated(h) = '1'
                                or set_wfi_condition_replicated(h) = '1'
                                or taken_branch_pending_internal(h) = '1'
                                or wfi_condition_pending(h) = '1'))
							or  reset_state = '1'
                           else '0';

						   
    pc_updater : process(all)
    begin
			pc_wire(h)                            <= pc(h);
        if reset_state = '1' and rst_ni = '1' then
		  served_except_condition_lat_wire <= served_except_condition(h);
          pc_wire(h) <= boot_pc;
		  taken_branch_pending_wire(h) 		  	<= taken_branch_pending(h);
		  wfi_condition_pending_wire(h)         <= wfi_condition_pending(h);
		  served_except_condition_wire(h)       <= served_except_condition(h);
		  served_mret_condition_wire(h)         <= served_mret_condition(h);
        elsif reset_state = '0' and rst_ni = '1' then
		  served_except_condition_lat_wire <= served_except_condition(h);
          pc_update(taken_branch_replicated(h), wfi_condition_pending_wire(h), set_wfi_condition_replicated(h), taken_branch_pending_internal(h),
					taken_branch_pending_wire(h), set_except_condition_replicated(h), set_mret_condition_replicated(h), pc_wire(h),
                    taken_branch_pc_lat_internal(h), incremented_pc_internal(h), boot_pc, pc_update_enable(h), served_except_condition_wire(h),
                    served_mret_condition_wire(h));
        else
			pc_wire(h)                            <= (others => '0');
			taken_branch_pending_wire(h) 		  <= '0';
			wfi_condition_pending_wire(h)         <= '0';
			served_except_condition_wire(h)       <= '0';
			served_mret_condition_wire(h)         <= '0';
			served_except_condition_lat_wire      <= '0';
		end if;
        
    end process;


  end generate pc_update_logic;



end PC;
