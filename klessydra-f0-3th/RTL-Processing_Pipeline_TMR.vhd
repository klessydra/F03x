-- RTL_Processing_Pipeline_TMR
-- Version:		0.3.4
-- FSM-TMR and all registers TMR



library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

use work.riscv_klessydra.all;
use work.thread_parameters_klessydra.all;
use work.TMR_REG_PKG.all;

entity Pipeline is
  port (
    pc_IF                      : in  std_logic_vector(31 downto 0);
    harc_IF                    : in  harc_range;
    irq_pending                : in  replicated_bit;
    csr_instr_done             : in  std_logic;
    csr_access_denied_o        : in  std_logic;
    csr_rdata_o                : in  std_logic_vector (31 downto 0);
    dbg_req_o                  : in  std_logic;
    dbg_halted_o               : in  std_logic;
    MSTATUS                    : in  replicated_32b_reg;
    served_irq     	       	   : out replicated_bit;
    WFI_Instr		       	   : out std_logic;
    reset_state                : out std_logic;
    misaligned_err             : out std_logic;							--TMR
    pc_IE                      : out std_logic_vector(31 downto 0); 	--TMR
    set_branch_condition       : out std_logic;
    taken_branch               : out std_logic;
    set_except_condition       : out std_logic;
    set_mret_condition         : out std_logic;
    set_wfi_condition          : out std_logic;
    csr_instr_req              : out std_logic;							--TMR
    instr_rvalid_IE            : out std_logic;							--TMR
    csr_addr_i                 : out std_logic_vector (11 downto 0);	--TMR
    csr_wdata_i                : out std_logic_vector (31 downto 0);	--TMR
    csr_op_i                   : out std_logic_vector (2 downto 0);		--TMR
    jump_instr                 : out std_logic;
    jump_instr_lat             : out std_logic;							--TMR
    branch_instr               : out std_logic;
    branch_instr_lat           : out std_logic;							--TMR
    data_valid_waiting_counter : out std_logic;
    harc_ID                    : out harc_range;						--TMR
    harc_IE                    : out harc_range;						--TMR
    harc_to_csr                : out harc_range;						--TMR
    instr_word_IE              : out std_logic_vector(31 downto 0);		--TMR
    PC_offset                  : out replicated_32b_reg;
    pc_except_value            : out replicated_32b_reg;
    dbg_ack_i                  : out std_logic;
    ebreak_instr               : out std_logic;
    data_addr_internal         : out std_logic_vector(31 downto 0);
    absolute_jump              : out std_logic;
    regfile                    : out regfile_replicated_array;
    clk_i                      : in  std_logic;
    rst_ni                     : in  std_logic;
    instr_req_o                : out std_logic;
    instr_gnt_i                : in  std_logic;
    instr_rvalid_i             : in  std_logic;
    instr_addr_o               : out std_logic_vector(31 downto 0);
    instr_rdata_i              : in  std_logic_vector(31 downto 0);
    data_req_o_wire_top        : out std_logic;
    data_gnt_i                 : in  std_logic;
    data_rvalid_i              : in  std_logic;
    data_we_o_wire_top         : out std_logic;
    data_be_o                  : out std_logic_vector(3 downto 0);
    data_addr_o                : out std_logic_vector(31 downto 0);
    data_wdata_o               : out std_logic_vector(31 downto 0);
    data_rdata_i               : in  std_logic_vector(31 downto 0);
    data_err_i                 : in  std_logic;
    irq_i                      : in  std_logic;
    debug_halted_o             : out std_logic;
    fetch_enable_i             : in  std_logic;
    core_busy_o                : out std_logic
    );
end entity;


architecture Pipe of Pipeline is

  ---------------------------------------
  -- Components declaration
  ---------------------------------------

  -- TMR register with reset preload (input)
  component TMR_REG is
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
  
  -- TMR register with reset (preload to zero)
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
  
 -- TMR register with 1 bit (TMR-DFF)
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

  -- TMR register with integer inputs
  component TMR_iREG is
	port (
		tmr_ireg_i_en			: in std_logic;
		tmr_ireg_i_clk			: in std_logic;
		tmr_ireg_i_data			: in harc_range;
		tmr_ireg_i_sync_rst_n	: in std_logic;
		tmr_ireg_i_async_rst_n	: in std_logic;
		tmr_ireg_o_data			: out harc_range
	);
	end component;

  -----------------------------------------------
  -- IF FSM
  -----------------------------------------------	
  --type fsm_IF_states is (normal, waiting);
  subtype fsm_IF_states is std_logic;
  constant normal_IF    : fsm_IF_states := '1';
  constant waiting_IF   : fsm_IF_states := '0';
  
  signal nextstate_IF	: fsm_IF_states;
  signal state_IF	: std_logic_vector(2 downto 0);
  
  -----------------------------------------------
  -- IE FSM
  -----------------------------------------------	
  --type fsm_IE_states is (sleep, reset, normal, data_valid_waiting, data_grant_waiting,
  --                       csr_instr_wait_state, debug, first_boot);
  subtype fsm_IE_states is std_logic_vector(2 downto 0);
  constant sleep					: fsm_IE_states := "010";
  constant reset					: fsm_IE_states := "000";
  constant normal					: fsm_IE_states := "001";
  constant data_valid_waiting		: fsm_IE_states := "101";
  constant data_grant_waiting		: fsm_IE_states := "100";
  constant csr_instr_wait_state		: fsm_IE_states := "011";
  constant debug					: fsm_IE_states := "110";
  constant first_boot				: fsm_IE_states := "111";
  
  
  signal nextstate_IE   : fsm_IE_states;
  signal state_IE		: std_logic_vector(8 downto 0);
  signal state_IE_int	: fsm_IE_states;
  --signal state_IE, nextstate_IE : fsm_IE_states;
  attribute KEEP : string;
  attribute KEEP of state_IE_int : signal is "yes";

-- TMR enable signal
	signal tmr_reg_IE_en 	: std_logic;
	signal tmr_reg_WB_en 	: std_logic;
	signal csr_wdata_en		: std_logic;
	signal tmr_reg_ID_en 	: std_logic;
	signal WB_stage_en 		: std_logic;
	signal flush_en			: replicated_bit;

  signal misaligned_err_wire	: std_logic;
  signal csr_instr_req_wire		: std_logic;
  signal instr_rvalid_IE_wire	: std_logic := '0';
  signal instr_rvalid_IE_int	: std_logic;
  signal csr_addr_i_wire        : std_logic_vector (11 downto 0);
  signal csr_wdata_i_wire		: std_logic_vector (31 downto 0);
  signal csr_op_i_wire          : std_logic_vector (2 downto 0);
  signal jump_instr_lat_wire	: std_logic;
  signal branch_instr_lat_wire	: std_logic;
  signal harc_to_csr_wire		: harc_range;
  signal pc_except_value_wire	: replicated_32b_reg;
  
  signal instr_rvalid_state     	: std_logic;
  signal instr_rvalid_state_wire	: std_logic;
  signal busy_ID                	: std_logic;
  signal busy_IE                	: std_logic;
  
  signal load_err, store_err : std_logic;

  signal pass_BEQ_ID   		: std_logic;				--TMR
  signal pass_BEQ_ID_wire   	: std_logic :='0';
  signal pass_BNE_ID   		: std_logic;				--TMR
  signal pass_BNE_ID_wire   	: std_logic :='0';
  signal pass_BLT_ID   		: std_logic;				--TMR
  signal pass_BLT_ID_wire   	: std_logic :='0';
  signal pass_BLTU_ID  		: std_logic;				--TMR
  signal pass_BLTU_ID_wire  	: std_logic :='0';
  signal pass_BGE_ID   		: std_logic;				--TMR
  signal pass_BGE_ID_wire   	: std_logic :='0';
  signal pass_BGEU_ID  		: std_logic;				--TMR
  signal pass_BGEU_ID_wire  	: std_logic :='0';
  signal pass_SLTI_ID  		: std_logic;
  signal pass_SLTIU_ID 		: std_logic;
  signal pass_SLT_ID   		: std_logic;
  signal pass_SLTU_ID  		: std_logic;

  signal WB_RD    		: std_logic_vector(31 downto 0);	--TMR
  signal WB_RD_wire     : std_logic_vector(31 downto 0);
  signal WB_RD_EN 		: std_logic;						--TMR
  signal WB_RD_EN_wire 	: std_logic;

  signal pc_WB     		: std_logic_vector(31 downto 0);
  signal pc_WB_wire     : std_logic_vector(31 downto 0);	--TMR
  signal pc_ID     		: std_logic_vector(31 downto 0);
  signal pc_ID_wire     : std_logic_vector(31 downto 0);	--TMR
  signal pc_ID_lat 		: std_logic_vector(31 downto 0);

  signal instr_word_ID_lat      	 : std_logic_vector(31 downto 0) := (others => '0');
  signal instr_rvalid_ID       	 	 : std_logic;
  signal instr_word_WB          	 : std_logic_vector(31 downto 0);					--TMR
  signal instr_word_WB_wire    	 	 : std_logic_vector(31 downto 0);
  signal instr_rvalid_WB        	 : std_logic;										--TMR
  signal instr_rvalid_WB_wire   	 : std_logic;
  signal decoded_instruction_IE		 : std_logic_vector(INSTR_SET_SIZE-1 downto 0);		--TMR
  signal decoded_instruction_IE_wire 	 : std_logic_vector(INSTR_SET_SIZE-1 downto 0) := (others => '0');
  signal data_we_o_lat 			 		: std_logic;										--TMR
  signal data_we_o_lat_wire 		 : std_logic;

  signal flush_cycle_count  		: replicated_positive_integer;			--TMR
  signal flush_cycle_count_wire  	: replicated_positive_integer;

  signal amo_load_skip 		: std_logic;
  signal amo_load_skip_wire 	: std_logic := '0';									--TMR
  signal amo_load_lat  		: std_logic;
  signal amo_load      		: std_logic;									--TMR
  signal amo_load_wire		: std_logic := '0';
  signal amo_store_lat 		: std_logic;									--TMR
  signal amo_store_lat_wire	: std_logic;
  signal amo_store     		: std_logic;
  signal sw_mip        		: std_logic;									--TMR
  signal sw_mip_wire  		: std_logic := '0';

  signal data_addr_internal_ID  		: std_logic_vector(31 downto 0);
  signal data_be_internal       		: std_logic_vector(3 downto 0);

  signal harc_ID_lat        : harc_range;
  signal harc_WB            : harc_range;								--TMR
  signal harc_WB_wire       : harc_range;
  signal RS1_Data_IE        : std_logic_vector(31 downto 0);			--TMR
  signal RS1_Data_IE_wire   : std_logic_vector(31 downto 0):= (others => '0');
  signal RS2_Data_IE        : std_logic_vector(31 downto 0);			--TMR
  signal RS2_Data_IE_wire   : std_logic_vector(31 downto 0):= (others => '0');
  signal RD_Data_IE         : std_logic_vector(31 downto 0);			--TMR
  signal RD_Data_IE_wire    : std_logic_vector(31 downto 0):= (others => '0');

  signal data_we_o_wire          : std_logic;
  signal data_addr_internal_wire : std_logic_vector(31 downto 0);
  signal pc_IE_wire              : std_logic_vector(31 downto 0) := (others =>'0');
  signal pc_IE_int		 		: std_logic_vector(31 downto 0);
  signal instr_word_IE_wire      : std_logic_vector(31 downto 0) := (others => '0');
  signal instr_word_IE_int	 	: std_logic_vector(31 downto 0);
  signal harc_ID_wire            : harc_range;
  signal harc_ID_int		 	: harc_range;
  signal harc_IE_wire            : harc_range;
  signal harc_IE_int		 	: harc_range;
  signal regfile_wire            : regfile_behavioral_array;
  signal regfile_int             : regfile_replicated_array;
  
begin

  instr_rvalid_IE    <= instr_rvalid_IE_int;	
  data_we_o_wire_top <= data_we_o_wire;
  data_addr_internal <= data_addr_internal_wire;
  harc_ID            <= harc_ID_int;
  harc_IE            <= harc_IE_int;
  regfile            <= regfile_int;
  pc_IE              <= pc_IE_int;
  instr_word_IE      <= instr_word_IE_int;

  -- Generate for all the replicated registers
  Replicated_TMR_reg_gen : for h in 0 to 2 generate
			
			TMR_CMP_flush_cycle_count : TMR_iREG
			port map (
				tmr_ireg_i_en			=>	flush_en(h),
				tmr_ireg_i_clk			=>	clk_i,
				tmr_ireg_i_data			=>	flush_cycle_count_wire(h),
				tmr_ireg_i_sync_rst_n	=>	'1',
				tmr_ireg_i_async_rst_n	=>	rst_ni,
				tmr_ireg_o_data			=>	flush_cycle_count(h)
			);
			
			
			TMR_CMP_pc_except_value : TMR_REG_rst_0 -- no rst
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	tmr_reg_WB_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	pc_except_value_wire(h),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	pc_except_value(h)
			);
  end generate Replicated_TMR_reg_gen;
  
  
  
  
--  Replicated_TMR_reg_file_gen : for h in 0 to 1 generate

	 -- Regfile_TMR_gen
			TMR_CMP_regfile0 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(0),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(0)
			);
			
			TMR_CMP_regfile1 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(1),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(1)
			);
			
			TMR_CMP_regfile2 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(2),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(2)
			);
			
			TMR_CMP_regfile3 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(3),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(3)
			);
			
			TMR_CMP_regfile4 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(4),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(4)
			);
			TMR_CMP_regfile5 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(5),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(5)
			);
			
			TMR_CMP_regfile6 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(6),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(6)
			);
			
			TMR_CMP_regfile7 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(7),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(7)
			);
			
			TMR_CMP_regfile8 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(8),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(8)
			);
			
			TMR_CMP_regfile9 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(9),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(9)
			);
			TMR_CMP_regfile10 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(10),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(10)
			);
			
			TMR_CMP_regfile11 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(11),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(11)
			);
			
			TMR_CMP_regfile12 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(12),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(12)
			);
			
			TMR_CMP_regfile13 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(13),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(13)
			);
			
			TMR_CMP_regfile14 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(14),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(14)
			);
			TMR_CMP_regfile15 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(15),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(15)
			);
			
			TMR_CMP_regfile16 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(16),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(16)
			);
			
			TMR_CMP_regfile17 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(17),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(17)
			);
			
			TMR_CMP_regfile18 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(18),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(18)
			);
			
			TMR_CMP_regfile19 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(19),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(19)
			);
			TMR_CMP_regfile20 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(20),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(20)
			);
			
			TMR_CMP_regfile21 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(21),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(21)
			);
			
			TMR_CMP_regfile22 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(22),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(22)
			);
			
			TMR_CMP_regfile23 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(23),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(23)
			);
			
			TMR_CMP_regfile24 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(24),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(24)
			);
			TMR_CMP_regfile25 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(25),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(25)
			);
			
			TMR_CMP_regfile26 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(26),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(26)
			);
			
			TMR_CMP_regfile27 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(27),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(27)
			);
			
			TMR_CMP_regfile28 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(28),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(28)
			);
			
			TMR_CMP_regfile29 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(29),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(29)
			);
			TMR_CMP_regfile30 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(30),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(30)
			);
			
			TMR_CMP_regfile31 : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	WB_stage_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	regfile_wire(31),
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	regfile_int(0)(31)
			);
			
			
  --end generate Replicated_TMR_reg_file_gen;
  
  TMR_CMP_misaligned_err : TMR_sREG  -- no rst
			port map (
				tmr_sreg_i_en			=>	tmr_reg_IE_en,
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	misaligned_err_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	misaligned_err
			);
			
  TMR_CMP_pc_IE : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	not busy_IE,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	pc_IE_wire,
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	pc_IE_int
			);
	
  TMR_CMP_csr_instr_req : TMR_sREG
			port map (
				tmr_sreg_i_en			=>	tmr_reg_WB_en,
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	csr_instr_req_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	csr_instr_req
			);

  TMR_CMP_instr_rvalid_IE : TMR_sREG
			port map (
				tmr_sreg_i_en			=>	not busy_IE,
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	instr_rvalid_IE_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	instr_rvalid_IE_int
			);

  TMR_CMP_csr_addr_i : TMR_REG_rst_0
			generic map ( DataWidth => 12 )
			port map (
				tmr_reg_i_en			=>	tmr_reg_IE_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	csr_addr_i_wire,
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	csr_addr_i
			);
	
  TMR_CMP_csr_wdata_i : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	csr_wdata_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	csr_wdata_i_wire,
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	csr_wdata_i
			);
	
  TMR_CMP_csr_op_i : TMR_REG_rst_0
			generic map ( DataWidth => 3 )
			port map (
				tmr_reg_i_en			=>	tmr_reg_IE_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	csr_op_i_wire,
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	csr_op_i
			);
			
  TMR_CMP_jump_instr_lat : TMR_sREG
			port map (
				tmr_sreg_i_en			=>	'1',
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	jump_instr_lat_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	jump_instr_lat
			);
			
  TMR_CMP_branch_instr_lat : TMR_sREG
			port map (
				tmr_sreg_i_en			=>	'1',
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	branch_instr_lat_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	branch_instr_lat
			);
	
  TMR_CMP_harc_ID : TMR_iREG
			port map (
				tmr_ireg_i_en			=>	instr_gnt_i,
				tmr_ireg_i_clk			=>	clk_i,
				tmr_ireg_i_data			=>	harc_ID_wire,
				tmr_ireg_i_sync_rst_n	=>	'1',
				tmr_ireg_i_async_rst_n	=>	rst_ni,
				tmr_ireg_o_data			=>	harc_ID_int
			);

  TMR_CMP_harc_IE : TMR_iREG
			port map (
				tmr_ireg_i_en			=>	tmr_reg_ID_en,
				tmr_ireg_i_clk			=>	clk_i,
				tmr_ireg_i_data			=>	harc_IE_wire,
				tmr_ireg_i_sync_rst_n	=>	'1',
				tmr_ireg_i_async_rst_n	=>	rst_ni,
				tmr_ireg_o_data			=>	harc_IE_int
			);

  TMR_CMP_harc_to_csr : TMR_iREG -- no rst
			port map (
				tmr_ireg_i_en			=>	tmr_reg_IE_en,
				tmr_ireg_i_clk			=>	clk_i,
				tmr_ireg_i_data			=>	harc_to_csr_wire,
				tmr_ireg_i_sync_rst_n	=>	'1',
				tmr_ireg_i_async_rst_n	=>	rst_ni,
				tmr_ireg_o_data			=>	harc_to_csr
			);			

  TMR_CMP_instr_word_IE : TMR_REG -- no rst
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	not busy_IE,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	instr_word_IE_wire,
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	instr_word_IE_int
			);

  TMR_CMP_instr_rvalid_state : TMR_sREG
			port map (
				tmr_sreg_i_en			=>	'1',
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	instr_rvalid_state_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	instr_rvalid_state
			);
			
  TMR_CMP_pass_BEQ_ID : TMR_sREG -- no rst
			port map (
				tmr_sreg_i_en			=>	tmr_reg_ID_en,
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	pass_BEQ_ID_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	pass_BEQ_ID
			);
			
  TMR_CMP_pass_BNE_ID : TMR_sREG -- no rst
			port map (
				tmr_sreg_i_en			=>	tmr_reg_ID_en,
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	pass_BNE_ID_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	pass_BNE_ID
			);
			
  TMR_CMP_pass_BLT_ID : TMR_sREG -- no rst
			port map (
				tmr_sreg_i_en			=>	tmr_reg_ID_en,
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	pass_BLT_ID_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	pass_BLT_ID
			);
			
  TMR_CMP_pass_BLTU_ID : TMR_sREG -- no rst
			port map (
				tmr_sreg_i_en			=>	tmr_reg_ID_en,
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	pass_BLTU_ID_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	pass_BLTU_ID
			);
			
  TMR_CMP_pass_BGE_ID : TMR_sREG -- no rst
			port map (
				tmr_sreg_i_en			=>	tmr_reg_ID_en,
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	pass_BGE_ID_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	pass_BGE_ID
			);
			
  TMR_CMP_pass_BGEU_ID : TMR_sREG -- no rst
			port map (
				tmr_sreg_i_en			=>	tmr_reg_ID_en,
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	pass_BGEU_ID_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	pass_BGEU_ID
			);			

  TMR_CMP_WB_RD : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	tmr_reg_WB_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	WB_RD_wire,
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	WB_RD
			);	
			
  TMR_CMP_WB_RD_EN : TMR_sREG 
			port map (
				tmr_sreg_i_en			=>	tmr_reg_WB_en,
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	WB_RD_EN_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	WB_RD_EN
			);			

TMR_CMP_pc_WB : TMR_REG_rst_0  -- no rst
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	tmr_reg_IE_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	pc_WB_wire,
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	pc_WB
			);
			
TMR_CMP_pc_ID : TMR_REG_rst_0
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	instr_gnt_i,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	pc_ID_wire,
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	pc_ID
			);			

TMR_CMP_instr_word_WB : TMR_REG_rst_0 -- no rst
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	tmr_reg_IE_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	instr_word_WB_wire,
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	instr_word_WB
			);		

  TMR_CMP_instr_rvalid_WB : TMR_sREG -- no rst 
			port map (
				tmr_sreg_i_en			=>	tmr_reg_IE_en,
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	instr_rvalid_WB_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	instr_rvalid_WB
			);			

TMR_CMP_decoded_instruction_IE : TMR_REG_rst_0  --no rst
			generic map ( DataWidth => INSTR_SET_SIZE )
			port map (
				tmr_reg_i_en			=>	tmr_reg_ID_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	decoded_instruction_IE_wire,
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	decoded_instruction_IE
			);

  TMR_CMP_data_we_o_lat : TMR_sREG 
			port map (
				tmr_sreg_i_en			=>	'1',
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	data_we_o_lat_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	data_we_o_lat
			);
	
  TMR_CMP_amo_load_skip : TMR_sREG -- no rst
			port map (
				tmr_sreg_i_en			=>	tmr_reg_ID_en,
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	amo_load_skip_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	amo_load_skip
			);
			
  TMR_CMP_amo_load : TMR_sREG -- no rst
			port map (
				tmr_sreg_i_en			=>	tmr_reg_ID_en,
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	amo_load_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	amo_load
			);
			
  TMR_CMP_amo_store_lat : TMR_sREG 
			port map (
				tmr_sreg_i_en			=>	'1',
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	amo_store_lat_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	amo_store_lat
			);
			
  TMR_CMP_sw_mip : TMR_sREG -- no rst
			port map (
				tmr_sreg_i_en			=>	tmr_reg_ID_en,
				tmr_sreg_i_clk			=>	clk_i,
				tmr_sreg_i_data			=>	sw_mip_wire,
				tmr_sreg_i_sync_rst_n	=>	'1',
				tmr_sreg_i_async_rst_n	=>	rst_ni,
				tmr_sreg_o_data			=>	sw_mip
			);


  TMR_CMP_harc_WB : TMR_iREG	-- no rst
			port map (
				tmr_ireg_i_en			=>	tmr_reg_IE_en,
				tmr_ireg_i_clk			=>	clk_i,
				tmr_ireg_i_data			=>	harc_WB_wire,
				tmr_ireg_i_sync_rst_n	=>	'1',
				tmr_ireg_i_async_rst_n	=>	rst_ni,
				tmr_ireg_o_data			=>	harc_WB
			);					

  TMR_CMP_RS1_Data_IE : TMR_REG_rst_0  -- no rst
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	tmr_reg_ID_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	RS1_Data_IE_wire,
				tmr_reg_i_sync_rst_n		=>	'1',
				tmr_reg_i_async_rst_n		=>	rst_ni,
				tmr_reg_o_data			=>	RS1_Data_IE
			);
	
  TMR_CMP_RS2_Data_IE : TMR_REG_rst_0  -- no rst
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	tmr_reg_ID_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	RS2_Data_IE_wire,
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	RS2_Data_IE
			);
			
  TMR_CMP_RD_Data_IE : TMR_REG_rst_0 -- no rst
			generic map ( DataWidth => 32 )
			port map (
				tmr_reg_i_en			=>	tmr_reg_ID_en,
				tmr_reg_i_clk			=>	clk_i,
				tmr_reg_i_data			=>	RD_Data_IE_wire,
				tmr_reg_i_sync_rst_n	=>	'1',
				tmr_reg_i_async_rst_n	=>	rst_ni,
				tmr_reg_o_data			=>	RD_Data_IE
			);
			
------------------------------------------------------------------------- 
  assert THREAD_POOL_SIZE < 2**THREAD_ID_SIZE
    report "threading configuration not supported"
    severity error;
-------------------------------------------------------------------------
  load_err  <= data_gnt_i and data_err_i and not(data_we_o_wire);
  store_err <= data_gnt_i and data_err_i and data_we_o_wire;

  data_addr_o <= data_addr_internal_wire(31 downto 2) & "00";
  data_be_o <= to_stdlogicvector(to_bitvector(data_be_internal) sll
                                 to_integer(unsigned(data_addr_internal_wire(1 downto 0))));

  instr_addr_o <= pc_IF;


  debug_halted_o <= dbg_halted_o;


  core_busy_o <= '1' when (instr_rvalid_i or instr_rvalid_ID or instr_rvalid_IE_int or instr_rvalid_WB) = '1' and rst_ni = '1' else '0';


  fsm_IF_nextstate : process(all)
  begin
    if rst_ni = '0' then
      instr_req_o  <= '0';
      nextstate_IF <= normal_IF;
    else
      case tmr_b_voting(state_IF) is
        when normal_IF =>
          if busy_ID = '0' then
            instr_req_o <= '1';
            if instr_gnt_i = '1' then
              nextstate_IF <= normal_IF;
            else
              nextstate_IF <= waiting_IF;
            end if;
          else
            instr_req_o  <= '0';
            nextstate_IF <= normal_IF;
          end if;
        when waiting_IF =>
          if busy_ID = '0' then
            instr_req_o <= '1';
            if instr_gnt_i = '1' then
              nextstate_IF <= normal_IF;
            else
              nextstate_IF <= waiting_IF;
            end if;
          else
            instr_req_o  <= '0';
            nextstate_IF <= normal_IF;
          end if;

        when others =>
          nextstate_IF <= normal_IF;
          instr_req_o  <= '0';
      end case;
    end if;
  end process;

  fsm_IF_register_state : process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      state_IF <= tmr_FSM_b_assign(normal_IF);
    elsif rising_edge(clk_i) then
      state_IF <= tmr_FSM_b_assign(nextstate_IF);
    end if;
  end process;


  process(all)
  begin
    if instr_gnt_i = '1' and rst_ni = '1' then
        pc_ID_wire   <= pc_IF;
        harc_ID_wire <= harc_IF;
	elsif instr_gnt_i = '0' and rst_ni = '1' then
		pc_ID_wire   <= pc_ID;
		harc_ID_wire <= harc_ID;
	else
	    pc_ID_wire   <= (others => '0');
		harc_ID_wire <= 0;
    end if;
  end process;

  process(all)
  begin
    if rst_ni = '0' then
     instr_rvalid_state_wire <= '0';
    else
      instr_rvalid_state_wire <= busy_ID and (instr_rvalid_i or instr_rvalid_state);
    end if;
  end process;
  instr_rvalid_ID <= (instr_rvalid_i or instr_rvalid_state);

  instr_word_ID_lat  <= instr_rdata_i when instr_rvalid_i = '1' else (others => '0') when rst_ni = '0';
  pc_ID_lat          <= pc_ID         when instr_rvalid_ID = '1' else (others => '0') when rst_ni = '0';
  harc_ID_lat        <= harc_ID_int  when instr_rvalid_ID = '1' else 0 when rst_ni = '0';


  fsm_ID_aync : process(all) -- Da modificare

    variable OPCODE_wires  : std_logic_vector (6 downto 0);
    variable FUNCT3_wires  : std_logic_vector (2 downto 0);
    variable FUNCT7_wires  : std_logic_vector (6 downto 0);
    variable FUNCT12_wires : std_logic_vector (11 downto 0);

  begin

    OPCODE_wires  := OPCODE(instr_word_ID_lat);
    FUNCT3_wires  := FUNCT3(instr_word_ID_lat);
    FUNCT7_wires  := FUNCT7(instr_word_ID_lat);
    FUNCT12_wires := FUNCT12(instr_word_ID_lat);

	tmr_reg_ID_en <= '0';
	pc_IE_wire      				<= pc_IE;
    harc_IE_wire    				<= harc_IE;
    instr_rvalid_IE_wire 			<= instr_rvalid_IE;
	instr_word_IE_wire 				<= instr_word_IE;
	RS1_Data_IE_wire 				<= RS1_Data_IE;
	RS2_Data_IE_wire 				<= RS2_Data_IE;
	RD_Data_IE_wire  				<= RD_Data_IE;
	pass_BEQ_ID_wire  				<= pass_BEQ_ID;
	pass_BNE_ID_wire  				<= pass_BNE_ID;
	pass_BLT_ID_wire  				<= pass_BLT_ID;
	pass_BLTU_ID_wire 				<= pass_BLTU_ID;
	pass_BGE_ID_wire  				<= pass_BGE_ID;
	pass_BGEU_ID_wire 				<= pass_BGEU_ID;
	amo_load_skip_wire 				<= amo_load_skip;
	amo_load_wire     				<= amo_load;
	sw_mip_wire						<= sw_mip;
	decoded_instruction_IE_wire 	<= decoded_instruction_IE;	 

    if rst_ni = '0' then
     pc_IE_wire      				<= (others => '0');
     harc_IE_wire    				<= 0;
     instr_rvalid_IE_wire 			<= '0';
	 instr_word_IE_wire 			<= (others => '0');
	 RS1_Data_IE_wire 				<= (others => '0');
	 RS2_Data_IE_wire 				<= (others => '0');
	 RD_Data_IE_wire  				<= (others => '0');
	 pass_BEQ_ID_wire  				<= '0';
	 pass_BNE_ID_wire  				<= '0';
	 pass_BLT_ID_wire  				<= '0';
	 pass_BLTU_ID_wire 				<= '0';
	 pass_BGE_ID_wire  				<= '0';
	 pass_BGEU_ID_wire 				<= '0';
	 amo_load_skip_wire 			<= '0';
	 amo_load_wire     				<= '0';
	 sw_mip_wire					<= '0';
	 decoded_instruction_IE_wire 	<= (others => '0');
	 tmr_reg_ID_en <= '0';	 
	 
    else
      if busy_IE = '1' then
        null;
      elsif instr_rvalid_ID = '0' then
        instr_rvalid_IE_wire <= '0';
      elsif busy_IE = '0' and instr_rvalid_ID = '1' then
	tmr_reg_ID_en <= '1';
        instr_rvalid_IE_wire    <= '1';
        instr_word_IE_wire 		<= instr_word_ID_lat;
        pc_IE_wire         		<= pc_ID_lat;
        harc_IE_wire       		<= harc_ID_lat;

        RS1_Data_IE_wire 		<= regfile_int(harc_ID_lat)(rs1(instr_word_ID_lat));
        RS2_Data_IE_wire 		<= regfile_int(harc_ID_lat)(rs2(instr_word_ID_lat));
        RD_Data_IE_wire  		<= regfile_int(harc_ID_lat)(rd(instr_word_ID_lat));
        pass_BEQ_ID_wire   <= '0';
        pass_BNE_ID_wire   <= '0';
        pass_BLT_ID_wire   <= '0';
        pass_BLTU_ID_wire  <= '0';
        pass_BGE_ID_wire   <= '0';
        pass_BGEU_ID_wire  <= '0';
        amo_load_skip_wire <= '0';
        amo_load_wire      <= '0';
        sw_mip_wire        <= '0';
        if data_addr_internal_ID(31 downto 4) = x"0000FF0" then
          sw_mip_wire <= '1';
        end if;
        if (signed(regfile_int(harc_ID_lat)(rs1(instr_word_ID_lat))(31 downto 0)) = signed(regfile_int(harc_ID_lat)(rs2(instr_word_ID_lat))(31 downto 0))) then
          pass_BEQ_ID_wire <= '1';
        end if;
        if (signed(regfile_int(harc_ID_lat)(rs1(instr_word_ID_lat))(31 downto 0)) /= signed(regfile_int(harc_ID_lat)(rs2(instr_word_ID_lat))(31 downto 0))) then
          pass_BNE_ID_wire <= '1';
        end if;
        if (signed(regfile_int(harc_ID_lat)(rs1(instr_word_ID_lat))(31 downto 0)) < signed(regfile_int(harc_ID_lat)(rs2(instr_word_ID_lat))(31 downto 0))) then
          pass_BLT_ID_wire <= '1';
        end if;
        if (unsigned(regfile_int(harc_ID_lat)(rs1(instr_word_ID_lat))(31 downto 0)) < unsigned(regfile_int(harc_ID_lat)(rs2(instr_word_ID_lat))(31 downto 0))) then
          pass_BLTU_ID_wire <= '1';
        end if;
        if (signed(regfile_int(harc_ID_lat)(rs1(instr_word_ID_lat))(31 downto 0)) >= signed(regfile_int(harc_ID_lat)(rs2(instr_word_ID_lat))(31 downto 0))) then
          pass_BGE_ID_wire <= '1';
        end if;
        if (unsigned(regfile_int(harc_ID_lat)(rs1(instr_word_ID_lat))(31 downto 0)) >= unsigned(regfile_int(harc_ID_lat)(rs2(instr_word_ID_lat))(31 downto 0))) then
          pass_BGEU_ID_wire <= '1';
        end if;

        case OPCODE_wires is
          when OP_IMM =>
            if(rd(instr_word_ID_lat) /= 0) then
              case FUNCT3_wires is
                when ADDI =>
                  decoded_instruction_IE_wire <= ADDI_pattern;
                when SLTI =>
                  decoded_instruction_IE_wire <= SLTI_pattern;
                when SLTIU =>
                  decoded_instruction_IE_wire <= SLTIU_pattern;
                when ANDI =>
                  decoded_instruction_IE_wire <= ANDI_pattern;
                when ORI =>
                  decoded_instruction_IE_wire <= ORI_pattern;
                when XORI =>
                  decoded_instruction_IE_wire <= XORI_pattern;
                when SLLI =>
                  decoded_instruction_IE_wire <= SLLI_pattern;
                when SRLI_SRAI =>
                  case FUNCT7_wires is
                    when SRLI7 =>
                      decoded_instruction_IE_wire <= SRLI7_pattern;
                    when SRAI7 =>
                      decoded_instruction_IE_wire <= SRAI7_pattern;
                    when others =>
                      decoded_instruction_IE_wire <= ILL_pattern;
                  end case;
                when others =>
                  decoded_instruction_IE_wire <= ILL_pattern;
              end case;
            else
              decoded_instruction_IE_wire <= NOP_pattern;
            end if;
          when LUI =>
            if (rd(instr_word_ID_lat) /= 0) then
              decoded_instruction_IE_wire <= LUI_pattern;
            else
              decoded_instruction_IE_wire <= NOP_pattern;
            end if;
          when AUIPC =>
            if (rd(instr_word_ID_lat) /= 0) then
              decoded_instruction_IE_wire <= AUIPC_pattern;
            else
              decoded_instruction_IE_wire <= NOP_pattern;
            end if;
          when OP =>
            if (rd(instr_word_ID_lat) /= 0) then
              case FUNCT3_wires is
                when ADD_SUB =>
                  case FUNCT7_wires is
                    when ADD7 =>
                      decoded_instruction_IE_wire <= ADD7_pattern;
                    when SUB7 =>
                      decoded_instruction_IE_wire <= SUB7_pattern;
                    when others =>
                      decoded_instruction_IE_wire <= ILL_pattern;
                  end case;
                when SLT =>
                  decoded_instruction_IE_wire <= SLT_pattern;
                when SLTU =>
                  decoded_instruction_IE_wire <= SLTU_pattern;
                when ANDD =>
                  decoded_instruction_IE_wire <= ANDD_pattern;
                when ORR =>
                  decoded_instruction_IE_wire <= ORR_pattern;
                when XORR =>
                  decoded_instruction_IE_wire <= XORR_pattern;
                when SLLL =>
                  decoded_instruction_IE_wire <= SLLL_pattern;
                when SRLL_SRAA =>
                  case FUNCT7_wires is
                    when SRLL7 =>
                      decoded_instruction_IE_wire <= SRLL7_pattern;
                    when SRAA7 =>
                      decoded_instruction_IE_wire <= SRAA7_pattern;
                    when others =>
                      decoded_instruction_IE_wire <= ILL_pattern;
                  end case;
                when others =>
                  decoded_instruction_IE_wire <= ILL_pattern;
              end case;
            else
              decoded_instruction_IE_wire <= NOP_pattern;
            end if;

          when JAL =>
            decoded_instruction_IE_wire <= JAL_pattern;

          when JALR =>
            decoded_instruction_IE_wire <= JALR_pattern;

          when BRANCH =>
            case FUNCT3_wires is
              when BEQ =>
                decoded_instruction_IE_wire <= BEQ_pattern;
              when BNE =>
                decoded_instruction_IE_wire <= BNE_pattern;
              when BLT =>
                decoded_instruction_IE_wire <= BLT_pattern;
              when BLTU =>
                decoded_instruction_IE_wire <= BLTU_pattern;
              when BGE =>
                decoded_instruction_IE_wire <= BGE_pattern;
              when BGEU =>
                decoded_instruction_IE_wire <= BGEU_pattern;
              when others =>
                decoded_instruction_IE_wire <= ILL_pattern;
            end case;

          when LOAD =>
            if (rd(instr_word_ID_lat) /= 0) then
              case FUNCT3_wires is
                when LW =>
                  decoded_instruction_IE_wire <= LW_pattern;
                when LH =>
                  decoded_instruction_IE_wire <= LH_pattern;
                when LHU =>
                  decoded_instruction_IE_wire <= LHU_pattern;
                when LB =>
                  decoded_instruction_IE_wire <= LB_pattern;
                when LBU =>
                  decoded_instruction_IE_wire <= LBU_pattern;
                when others =>
                  decoded_instruction_IE_wire <= ILL_pattern;
              end case;
            else
              decoded_instruction_IE_wire <= NOP_pattern;
            end if;

          when STORE =>
            case FUNCT3_wires is
              when SW =>
                decoded_instruction_IE_wire <= SW_pattern;
              when SH =>
                decoded_instruction_IE_wire <= SH_pattern;
              when SB =>
                decoded_instruction_IE_wire <= SB_pattern;
              when others =>
                decoded_instruction_IE_wire <= ILL_pattern;
            end case;

          when MISC_MEM =>
            case FUNCT3_wires is
              when FENCE =>
                decoded_instruction_IE_wire <= FENCE_pattern;
              when FENCEI =>
                decoded_instruction_IE_wire <= FENCEI_pattern;
              when others =>
                decoded_instruction_IE_wire <= ILL_pattern;
            end case;

          when SYSTEM =>
            case FUNCT3_wires is
              when PRIV =>
                if (rs1(instr_word_ID_lat) = 0 and rd(instr_word_ID_lat) = 0) then
                  case FUNCT12_wires is
                    when ECALL =>
                      decoded_instruction_IE_wire <= ECALL_pattern;
                    when EBREAK =>
                      decoded_instruction_IE_wire <= EBREAK_pattern;
                    when mret =>
                      decoded_instruction_IE_wire <= MRET_pattern;
                    when WFI =>
                      decoded_instruction_IE_wire <= WFI_pattern;
                    when others =>
                      decoded_instruction_IE_wire <= ILL_pattern;
                  end case;
                else
                  decoded_instruction_IE_wire <= ILL_pattern;
                end if;
              when CSRRW =>
                decoded_instruction_IE_wire <= CSRRW_pattern;
              when CSRRS =>
                if(rd(instr_word_ID_lat) /= 0) then
                  decoded_instruction_IE_wire <= CSRRS_pattern;
                else
                  decoded_instruction_IE_wire <= NOP_pattern;
                end if;
              when CSRRC =>
                if(rd(instr_word_ID_lat) /= 0) then
                  decoded_instruction_IE_wire <= CSRRC_pattern;
                else
                  decoded_instruction_IE_wire <= NOP_pattern;
                end if;
              when CSRRWI =>
                decoded_instruction_IE_wire <= CSRRWI_pattern;
              when CSRRSI =>
                if(rd(instr_word_ID_lat) /= 0) then
                  decoded_instruction_IE_wire <= CSRRSI_pattern;
                else
                  decoded_instruction_IE_wire <= NOP_pattern;
                end if;
              when CSRRCI =>
                if(rd(instr_word_ID_lat) /= 0) then
                  decoded_instruction_IE_wire <= CSRRCI_pattern;
                else
                  decoded_instruction_IE_wire <= NOP_pattern;
                end if;
              when others =>
                decoded_instruction_IE_wire <= ILL_pattern;
            end case;

          when AMO =>
            case FUNCT3_wires is
              when SINGLE =>
                if(rd(instr_word_ID_lat) /= 0) then
                  amo_load_skip_wire          <= '0';
                  decoded_instruction_IE_wire <= AMOSWAP_pattern;
                  if amo_store = '1' then
                    amo_load_wire <= '0';
                  elsif amo_store = '0' then
                    amo_load_wire <= '1';
                  end if;
                elsif (rd(instr_word_ID_lat) = 0) then
                  decoded_instruction_IE_wire <= AMOSWAP_pattern;
                  amo_load_skip_wire          <= '1';
                end if;
              when others =>
                decoded_instruction_IE_wire <= ILL_pattern;
            end case;
          when others =>
            decoded_instruction_IE_wire <= ILL_pattern;
        end case;

      end if;
    end if;
  end process;

  fsm_ID_comb : process(all)
  begin
    if busy_IE = '1' then
      busy_ID <= '1';
    else 
      busy_ID <= '0';
    end if;  
  end process;

  data_addr_internal_ID <= std_logic_vector(signed(regfile_int(harc_ID_lat)(rs1(instr_word_ID_lat))) + signed(S_immediate(instr_word_ID_lat)));



  state_IE_int <= tmr_voting(state_IE,3);
  
  fsm_IE_aync : process(all) 

    variable row : line;

  begin
  
	  
	  WB_RD_wire 					<= WB_RD;
      WB_RD_EN_wire            		<= WB_RD_EN;
      csr_instr_req_wire       		<= csr_instr_req;
      csr_op_i_wire            		<= csr_op_i;
      csr_wdata_i_wire         		<= csr_wdata_i;
	  csr_wdata_en					<= '0';
      csr_addr_i_wire          		<= csr_addr_i;
	  branch_instr_lat_wire 		<= branch_instr_lat; 
      jump_instr_lat_wire   		<= jump_instr_lat;
      data_we_o_lat_wire    		<= data_we_o_lat;
      amo_store_lat_wire   			<= amo_store_lat;
	  instr_rvalid_WB_wire 			<= instr_rvalid_WB;
	  misaligned_err_wire   		<= misaligned_err;
	  harc_WB_wire 					<= harc_WB;
	  instr_word_WB_wire			<= instr_word_WB;
	  pc_except_value_wire 			<= pc_except_value;
	  harc_to_csr_wire				<= harc_to_csr;
	  pc_WB_wire					<= pc_WB;
	  
      for h in harc_range loop
	    flush_en(h)					<= '0';
        flush_cycle_count_wire(h) 	<= 0;--flush_cycle_count_wire(h);
      end loop;

    if rst_ni = '0' then
	  
      WB_RD_wire 				<= std_logic_vector(to_unsigned(0, 32));
      WB_RD_EN_wire            	<= '0';
      csr_instr_req_wire       <= '0';
      csr_op_i_wire            <= (others => '0');
      csr_wdata_i_wire         <= (others => '0');
	  csr_wdata_en			   <= '0';
      csr_addr_i_wire          <= (others => '0');
	  branch_instr_lat_wire 		<= '0'; 
      jump_instr_lat_wire   		<= '0';
      data_we_o_lat_wire    		<= '0';
      amo_store_lat_wire   			<= '0';
	  instr_rvalid_WB_wire 			<= '0';
	  misaligned_err_wire   		<= '0';
	  harc_WB_wire 					<= 0;
	  instr_word_WB_wire			<= (others => '0');
	  pc_except_value_wire 			<= (others =>(others => '0'));
	  harc_to_csr_wire				<= 0;
	  
      for h in harc_range loop
	    flush_en(h)					<= '0';
        flush_cycle_count_wire(h) 	<= 0;
      end loop;
    else
	
	  branch_instr_lat_wire  <= branch_instr;
      jump_instr_lat_wire    <= jump_instr;
      data_we_o_lat_wire          <= data_we_o_wire;
      amo_store_lat_wire          <= amo_store;
      for h in harc_range loop
        if taken_branch = '1' and harc_IE = h then
	    flush_en(h)				  <= '1';		
        flush_cycle_count_wire(h) <= NOP_POOL_SIZE;
        elsif flush_cycle_count(h) /= 0 and busy_IE = '0' then
		flush_en(h)				  <= '1';
          flush_cycle_count_wire(h) <= flush_cycle_count(h) - 1;
        end if;
      end loop;
	  
      case state_IE_int is
        when sleep =>
          null;
        when reset =>
          null;
        when first_boot =>
          null;
        when debug =>
          null;
        when normal =>
          if instr_rvalid_IE_int = '0' or flush_cycle_count(harc_IE) /=0 then
            instr_rvalid_WB_wire <= '0';
          elsif irq_pending(harc_IE) = '1' then
            instr_rvalid_WB_wire <= '0';
          else
            pc_WB_wire               <= pc_IE_int;
            instr_rvalid_WB_wire     <= '1';
            instr_word_WB_wire       <= instr_word_IE_int;
            harc_WB_wire             <= harc_IE_int;
            misaligned_err_wire      <= '0';
            -- pragma translate_off
            -- hwrite(row, pc_IE_int);
            -- write(row, '_');
            -- hwrite(row, instr_word_IE_int);
            -- write(row, "   " & to_string(now));
            -- writeline(file_handler, row);
            -- pragma translate_on


            if decoded_instruction_IE(ADDI_bit_position) = '1' then
              WB_RD_EN_wire <= '1';
              WB_RD_wire 	<= std_logic_vector(signed(RS1_Data_IE)+
                                                             signed(I_immediate(instr_word_IE_int)));
            end if;

            if decoded_instruction_IE(SLTI_bit_position) = '1' then
              if (signed(RS1_Data_IE) < signed (I_immediate(instr_word_IE))) then
                WB_RD_EN_wire       <= '1';
                WB_RD_wire 			<= std_logic_vector(to_unsigned(1, 32));
              else
                WB_RD_EN_wire       <= '1';
                WB_RD_wire 			<= std_logic_vector(to_unsigned(0, 32));
              end if;
            end if;

            if decoded_instruction_IE(SLTIU_bit_position) = '1' then
              if (unsigned(RS1_Data_IE) < unsigned (I_immediate(instr_word_IE))) then
                WB_RD_EN_wire       <= '1';
                WB_RD_wire 			<= std_logic_vector(to_unsigned(1, 32));
              else
                WB_RD_EN_wire       <= '1';
                WB_RD_wire 			<= std_logic_vector(to_unsigned(0, 32));
              end if;
            end if;

            if decoded_instruction_IE(ANDI_bit_position) = '1' then
              WB_RD_EN_wire         <= '1';
              WB_RD_wire 			<= RS1_Data_IE and I_immediate(instr_word_IE_int);
            end if;

            if decoded_instruction_IE(ORI_bit_position) = '1' then
              WB_RD_EN_wire         <= '1';
              WB_RD_wire 			<= RS1_Data_IE or I_immediate(instr_word_IE_int);
            end if;

            if decoded_instruction_IE(XORI_bit_position) = '1' then
              WB_RD_EN_wire         <= '1';
              WB_RD_wire 			<= RS1_Data_IE xor I_immediate(instr_word_IE_int);
            end if;

            if decoded_instruction_IE(SLLI_bit_position) = '1' then
              WB_RD_EN_wire 		<= '1';
              WB_RD_wire	<=
                to_stdlogicvector(to_bitvector(RS1_Data_IE)
                                  sll to_integer(unsigned(SHAMT(instr_word_IE_int))));
            end if;

            if decoded_instruction_IE(SRLI7_bit_position) = '1' then
              WB_RD_EN_wire 		<= '1';
              WB_RD_wire 	<=
                to_stdlogicvector(to_bitvector(RS1_Data_IE)
                                  srl to_integer(unsigned(SHAMT(instr_word_IE_int))));
            end if;

            if decoded_instruction_IE(SRAI7_bit_position) = '1' then
              WB_RD_EN_wire 		<= '1';
              WB_RD_wire 	<=
                to_stdlogicvector(to_bitvector(RS1_Data_IE)
                                  sra to_integer(unsigned(SHAMT(instr_word_IE_int))));
            end if;

            if decoded_instruction_IE(LUI_bit_position) = '1' then
              WB_RD_EN_wire         <= '1';
              WB_RD_wire 			<= U_immediate(instr_word_IE_int);
            end if;

            if decoded_instruction_IE(AUIPC_bit_position) = '1' then
              WB_RD_EN_wire 		<= '1';
              WB_RD_wire 			<= std_logic_vector(signed(U_immediate(instr_word_IE_int))
                                                             + signed(pc_IE_int));
            end if;

            if decoded_instruction_IE(ADD7_bit_position) = '1' then
              WB_RD_EN_wire 		<= '1';
              WB_RD_wire 			<= std_logic_vector(signed(RS1_Data_IE)
                                                             + signed(RS2_Data_IE));
            end if;

            if decoded_instruction_IE(SUB7_bit_position) = '1' then
              WB_RD_EN_wire 		<= '1';
              WB_RD_wire 			<= std_logic_vector(signed(RS1_Data_IE)
                                                             - signed(RS2_Data_IE));
            end if;

            if decoded_instruction_IE(SLT_bit_position) = '1' then
              WB_RD_EN_wire <= '1';
              if (signed(RS1_Data_IE) < signed (RS2_Data_IE)) then
                WB_RD_wire <= std_logic_vector(to_unsigned(1, 32));
              else
                WB_RD_wire <= std_logic_vector(to_unsigned(0, 32));
              end if;
            end if;

            if decoded_instruction_IE(SLTU_bit_position) = '1' then
              WB_RD_EN_wire <= '1';
              if (unsigned(RS1_Data_IE) < unsigned (RS2_Data_IE)) then
                WB_RD_wire <= std_logic_vector(to_unsigned(1, 32));
              else
                WB_RD_wire <= std_logic_vector(to_unsigned(0, 32));
              end if;
            end if;

            if decoded_instruction_IE(ANDD_bit_position) = '1' then
              WB_RD_EN_wire         <= '1';
              WB_RD_wire 			<= RS1_Data_IE and RS2_Data_IE;
            end if;

            if decoded_instruction_IE(ORR_bit_position) = '1' then
              WB_RD_EN_wire         <= '1';
              WB_RD_wire 			<= RS1_Data_IE or RS2_Data_IE;
            end if;

            if decoded_instruction_IE(XORR_bit_position) = '1' then
              WB_RD_EN_wire         <= '1';
              WB_RD_wire 			<= RS1_Data_IE xor RS2_Data_IE;
            end if;

            if decoded_instruction_IE(SLLL_bit_position) = '1' then
              WB_RD_EN_wire 		<= '1';
              WB_RD_wire 	<=
                to_stdlogicvector(to_bitvector(RS1_Data_IE)
                                  sll to_integer(unsigned(RS2_Data_IE
                                                          (4 downto 0))));
            end if;
            if decoded_instruction_IE(SRLL7_bit_position) = '1' then
              WB_RD_EN_wire 		<= '1';
              WB_RD_wire 	<=
                to_stdlogicvector(to_bitvector(RS1_Data_IE)
                                  srl to_integer(unsigned(RS2_Data_IE
                                                          (4 downto 0))));
            end if;

            if decoded_instruction_IE(SRAA7_bit_position) = '1' then
              WB_RD_EN_wire 		<= '1';
              WB_RD_wire 	<=
                to_stdlogicvector(to_bitvector(RS1_Data_IE)
                                  sra to_integer(unsigned(RS2_Data_IE
                                                          (4 downto 0))));
            end if;

            if decoded_instruction_IE(JAL_bit_position) = '1' or decoded_instruction_IE(JALR_bit_position) = '1' then
              if (rd(instr_word_IE_int) /= 0) then
                WB_RD_EN_wire       <= '1';
                WB_RD_wire 			<= std_logic_vector(unsigned(pc_IE_int) + "100");
              else
                WB_RD_EN_wire 		<= '0';
                null;
              end if;
            end if;



            if decoded_instruction_IE(BEQ_bit_position) = '1' then
              WB_RD_EN_wire 		<= '0';
              null;
            end if;

            if decoded_instruction_IE(BNE_bit_position) = '1' then
              WB_RD_EN_wire 		<= '0';
              null;
            end if;

            if decoded_instruction_IE(BLT_bit_position) = '1' then
              WB_RD_EN_wire 		<= '0';
              null;
            end if;

            if decoded_instruction_IE(BLTU_bit_position) = '1' then
              WB_RD_EN_wire 		<= '0';
              null;
            end if;

            if decoded_instruction_IE(BGE_bit_position) = '1' then
              WB_RD_EN_wire 		<= '0';
              null;
            end if;

            if decoded_instruction_IE(BGEU_bit_position) = '1' then
              WB_RD_EN_wire 		<= '0';
              null;
            end if;

            if decoded_instruction_IE(LW_bit_position) = '1' or (decoded_instruction_IE(AMOSWAP_bit_position) = '1' and amo_load = '1' and amo_load_skip = '0') then
              WB_RD_EN_wire 		<= '0';
			  
              if(data_addr_internal_wire(1 downto 0) = "00") then
                if (load_err = '1') then
                  WB_RD_EN_wire                     <= '0';
                  pc_except_value_wire(harc_IE_int) <= pc_IE_int;
				  csr_wdata_en			<= '1';
                  csr_wdata_i_wire                  <= LOAD_ERROR_EXCEPT_CODE;
                elsif (store_err = '1') then
                  WB_RD_EN_wire                     <= '0';
                  pc_except_value_wire(harc_IE_int) <= pc_IE_int;
				  csr_wdata_en			<= '1';
                  csr_wdata_i_wire                  <= STORE_ERROR_EXCEPT_CODE;
                end if;
              else
                pc_except_value_wire(harc_IE_int) 	<= pc_IE_int;
				csr_wdata_en			<= '1';
                csr_wdata_i_wire                    <= LOAD_MISALIGNED_EXCEPT_CODE;
                misaligned_err_wire                 <= '1';
              end if;
            end if;

            if decoded_instruction_IE(LH_bit_position) = '1' or decoded_instruction_IE(LHU_bit_position) = '1' then
              WB_RD_EN_wire 	<= '0';
			  
              if(data_addr_internal_wire(0) = '0') then
                if (load_err = '1') then
                  WB_RD_EN_wire                     <= '0';
                  pc_except_value_wire(harc_IE_int) <= pc_IE_int;
				  csr_wdata_en		<= '1';
                  csr_wdata_i_wire                  <= LOAD_ERROR_EXCEPT_CODE;
                elsif (store_err = '1') then
                  WB_RD_EN_wire                     <= '0';
                  pc_except_value_wire(harc_IE_int) <= pc_IE_int;
				  csr_wdata_en		<= '1';
                  csr_wdata_i_wire                  <= STORE_ERROR_EXCEPT_CODE;
                end if;
              else
                pc_except_value_wire(harc_IE_int) 	<= pc_IE_int;
				csr_wdata_en		<= '1';
                csr_wdata_i_wire                    <= LOAD_MISALIGNED_EXCEPT_CODE;
                misaligned_err_wire                 <= '1';
              end if;
            end if;

            if decoded_instruction_IE(LB_bit_position) = '1' or decoded_instruction_IE(LBU_bit_position) = '1' then
              WB_RD_EN_wire 	<= '0';
              if (load_err = '1') then
				csr_wdata_en		<= '1';
                WB_RD_EN_wire                     <= '0';
                pc_except_value_wire(harc_IE_int) <= pc_IE_int;
                csr_wdata_i_wire                  <= LOAD_ERROR_EXCEPT_CODE;
              elsif (store_err = '1') then
				csr_wdata_en		<= '1';
                WB_RD_EN_wire                     <= '0';
                pc_except_value_wire(harc_IE_int) <= pc_IE_int;
                csr_wdata_i_wire                  <= STORE_ERROR_EXCEPT_CODE;
              end if;
            end if;


            if decoded_instruction_IE(SW_bit_position) = '1' or (decoded_instruction_IE(AMOSWAP_bit_position) = '1' and (amo_store_lat = '1' or amo_load_skip = '1')) then
              WB_RD_EN_wire			<= '0';

              if sw_mip = '1' then
				csr_wdata_en		<= '1';
                csr_op_i_wire      	<= CSRRW;
                csr_instr_req_wire 	<= '1';
                csr_wdata_i_wire   	<= RS2_Data_IE;
                csr_addr_i_wire    	<= MIP_ADDR;

				for i in harc_range loop
                	if data_addr_internal_wire(3 downto 0) = std_logic_vector(to_unsigned((4*i),4)) then
                  	harc_to_csr_wire <= i;
                	end if;
				end loop;
						
              elsif(data_addr_internal_wire(1 downto 0) = "00") then
                if (load_err = '1') then
					csr_wdata_en		<= '1';
					pc_except_value_wire(harc_IE_int) <= pc_IE_int;
					csr_wdata_i_wire                  <= LOAD_ERROR_EXCEPT_CODE;
                elsif (store_err = '1') then
					csr_wdata_en		<= '1';
					pc_except_value_wire(harc_IE_int) <= pc_IE_int;
					csr_wdata_i_wire                  <= STORE_ERROR_EXCEPT_CODE;
                end if;
              else
				csr_wdata_en		<= '1';
                pc_except_value_wire(harc_IE_int) 	<= pc_IE_int;
                csr_wdata_i_wire                    <= STORE_MISALIGNED_EXCEPT_CODE;
                misaligned_err_wire                 <= '1';
              end if;
            end if;

            if decoded_instruction_IE(SH_bit_position) = '1' then
              WB_RD_EN_wire 	<= '0';
              if(data_addr_internal_wire(0) = '0') then
                if (load_err = '1') then
					csr_wdata_en		<= '1';
					pc_except_value_wire(harc_IE_int) <= pc_IE_int;
					csr_wdata_i_wire                  <= LOAD_ERROR_EXCEPT_CODE;
                elsif (store_err = '1') then
					csr_wdata_en		<= '1';
					pc_except_value_wire(harc_IE_int) <= pc_IE_int;
					csr_wdata_i_wire                  <= STORE_ERROR_EXCEPT_CODE;
                end if;
              else
				csr_wdata_en		<= '1';
                pc_except_value_wire(harc_IE_int) 	<= pc_IE_int;
                csr_wdata_i_wire                    <= STORE_MISALIGNED_EXCEPT_CODE;
                misaligned_err_wire                 <= '1';
              end if;
            end if;

            if decoded_instruction_IE(SB_bit_position) = '1' then
              WB_RD_EN_wire 	<= '0';
              if (load_err = '1') then
				csr_wdata_en		<= '1';
                pc_except_value_wire(harc_IE_int) <= pc_IE_int;
                csr_wdata_i_wire                  <= LOAD_ERROR_EXCEPT_CODE;
              elsif (store_err = '1') then
				csr_wdata_en		<= '1';
                pc_except_value_wire(harc_IE_int) <= pc_IE_int;
                csr_wdata_i_wire                  <= STORE_ERROR_EXCEPT_CODE;
              end if;
            end if;

            if decoded_instruction_IE(FENCE_bit_position) = '1' then
              WB_RD_EN_wire <= '0';
              null;
            end if;

            if decoded_instruction_IE(FENCEI_bit_position) = '1' then
              WB_RD_EN_wire <= '0';
              null;
            end if;

            if decoded_instruction_IE(ECALL_bit_position) = '1' then
              WB_RD_EN_wire                             <= '0';
              csr_wdata_en								<= '1';
			  csr_wdata_i_wire                          <= ECALL_EXCEPT_CODE;
              pc_except_value_wire(harc_IE_int) 		<= pc_IE_int;
            end if;

            if decoded_instruction_IE(EBREAK_bit_position) = '1' then
              WB_RD_EN_wire <= '0';
              null;
            end if;

            if decoded_instruction_IE(MRET_bit_position) = '1' then
              WB_RD_EN_wire <= '0';
              null;
            end if;

            if decoded_instruction_IE(WFI_bit_position) = '1' then
              WB_RD_EN_wire <= '0';
              null;
            end if;

            if decoded_instruction_IE(CSRRW_bit_position) = '1' then
              WB_RD_EN_wire      <= '0';
              csr_op_i_wire      <= FUNCT3(instr_word_IE_int);
              csr_instr_req_wire <= '1';
			  csr_wdata_en		 <= '1';
              csr_wdata_i_wire   <= RS1_Data_IE;
              csr_addr_i_wire    <= std_logic_vector(to_unsigned(to_integer(unsigned(CSR_ADDR(instr_word_IE_int))), 12));
              harc_to_csr_wire   <= harc_IE_int;
            end if;

            if decoded_instruction_IE(CSRRC_bit_position) = '1' or decoded_instruction_IE(CSRRS_bit_position) = '1' then
              WB_RD_EN_wire      <= '0';
              csr_op_i_wire      <= FUNCT3(instr_word_IE_int);
              csr_instr_req_wire <= '1';
			  csr_wdata_en		 <= '1';
              csr_wdata_i_wire   <= RS1_Data_IE;
              csr_addr_i_wire    <= std_logic_vector(to_unsigned(to_integer(unsigned(CSR_ADDR(instr_word_IE_int))), 12));
              harc_to_csr_wire   <= harc_IE_int;
            end if;

            if decoded_instruction_IE(CSRRWI_bit_position) = '1' then
              WB_RD_EN_wire      <= '0';
              csr_op_i_wire      <= FUNCT3(instr_word_IE_int);
              csr_instr_req_wire <= '1';
			  csr_wdata_en		<= '1';
              csr_wdata_i_wire   <= std_logic_vector(resize(to_unsigned(rs1(instr_word_IE_int), 5), 32));
              csr_addr_i_wire    <= std_logic_vector(to_unsigned(to_integer(unsigned(CSR_ADDR(instr_word_IE_int))), 12));
              harc_to_csr_wire   <= harc_IE_int;
            end if;

            if decoded_instruction_IE(CSRRSI_bit_position) = '1'or decoded_instruction_IE(CSRRCI_bit_position) = '1' then
              WB_RD_EN_wire      <= '0';
              csr_op_i_wire      <= FUNCT3(instr_word_IE_int);
              csr_instr_req_wire <= '1';
			  csr_wdata_en		<= '1';
              csr_wdata_i_wire   <= std_logic_vector(resize(to_unsigned(rs1(instr_word_IE_int), 5), 32));
              csr_addr_i_wire    <= std_logic_vector(to_unsigned(to_integer(unsigned(CSR_ADDR(instr_word_IE_int))), 12));
              harc_to_csr_wire   <= harc_IE_int;
            end if;

            if decoded_instruction_IE(ILL_bit_position) = '1' then
              WB_RD_EN_wire                             <= '0';
			  csr_wdata_en		<= '1';
              csr_wdata_i_wire                          <= ILLEGAL_INSN_EXCEPT_CODE;
              pc_except_value_wire(harc_IE_int) 		<= pc_IE_int;
            end if;

            if decoded_instruction_IE(NOP_bit_position) = '1' then
              WB_RD_EN_wire <= '0';
              null;
            end if;

          end if;

        when data_valid_waiting =>
          if instr_rvalid_IE_int = '0' then
            null;
          else
            if (load_err = '1') then
              WB_RD_EN_wire                             <= '0';
              pc_except_value_wire(harc_IE_int) 		<= pc_IE_int;
			  csr_wdata_en		<= '1';
              csr_wdata_i_wire                          <= LOAD_ERROR_EXCEPT_CODE;
            elsif (store_err = '1') then
              WB_RD_EN_wire                             <= '0';
              pc_except_value_wire(harc_IE_int) 		<= pc_IE_int;
			  csr_wdata_en		<= '1';
              csr_wdata_i_wire                          <= STORE_ERROR_EXCEPT_CODE;
            elsif (data_rvalid_i = '1' and (OPCODE(instr_word_IE_int) = LOAD or OPCODE(instr_word_IE_int) = AMO) and rd(instr_word_IE_int) /= 0) then
              if decoded_instruction_IE(LW_bit_position) = '1' or (decoded_instruction_IE(AMOSWAP_bit_position) = '1' and amo_load = '1') then
                if(data_addr_internal_wire(1 downto 0) = "00") then
                  WB_RD_EN_wire                 <= '1';
                  WB_RD_wire 					<= data_rdata_i;
                end if;
              end if;
              if decoded_instruction_IE(LH_bit_position) = '1' then
                case data_addr_internal_wire(1 downto 0) is
                  when "00" =>
                    WB_RD_EN_wire 	<= '1';
                    WB_RD_wire 		<=
                      std_logic_vector(resize(signed(data_rdata_i(15 downto 0)), 32));
                  when "01" =>
                    WB_RD_EN_wire 	<= '1';
                    WB_RD_wire 		<=
                      std_logic_vector(resize(signed(data_rdata_i(23 downto 8)), 32));
                  when "10" =>
                    WB_RD_EN_wire 	<= '1';
                    WB_RD_wire 		<=
                      std_logic_vector(resize(signed(data_rdata_i(31 downto 16)), 32));
                  when others =>
                    null;
                end case;
              end if;
              if decoded_instruction_IE(LHU_bit_position) = '1' then
                case data_addr_internal_wire(1 downto 0) is
                  when "00" =>
                    WB_RD_EN_wire 	<= '1';
                    WB_RD_wire 		<=
                      std_logic_vector(resize(unsigned(data_rdata_i(15 downto 0)), 32));
                  when "01" =>
                    WB_RD_EN_wire 	<= '1';
                    WB_RD_wire 		<=
                      std_logic_vector(resize(unsigned(data_rdata_i(23 downto 8)), 32));
                  when "10" =>
                    WB_RD_EN_wire 	<= '1';
                    WB_RD_wire 		<=
                      std_logic_vector(resize(unsigned(data_rdata_i(31 downto 16)), 32));
                  when others =>
                    null;
                end case;
              end if;
              if decoded_instruction_IE(LB_bit_position) = '1' then
                case data_addr_internal_wire(1 downto 0) is
                  when "00" =>
                    WB_RD_EN_wire 	<= '1';
                    WB_RD_wire 		<=
                      std_logic_vector(resize(signed(data_rdata_i(7 downto 0)), 32));
                  when "01" =>
                    WB_RD_EN_wire 	<= '1';
                    WB_RD_wire 		<=
                      std_logic_vector(resize(signed(data_rdata_i(15 downto 8)), 32));
                  when "10" =>
                    WB_RD_EN_wire 	<= '1';
                    WB_RD_wire		<=
                      std_logic_vector(resize(signed(data_rdata_i(23 downto 16)), 32));
                  when "11" =>
                    WB_RD_EN_wire 	<= '1';
                    WB_RD_wire 		<=
                      std_logic_vector(resize(signed(data_rdata_i(31 downto 24)), 32));
                  when others =>
                    null;
                end case;
              end if;
              if decoded_instruction_IE(LBU_bit_position) = '1' then
                case data_addr_internal_wire(1 downto 0) is
                  when "00" =>
                    WB_RD_EN_wire 	<= '1';
                    WB_RD_wire 		<=
                      std_logic_vector(resize(unsigned(data_rdata_i(7 downto 0)), 32));
                  when "01" =>
                    WB_RD_EN_wire 	<= '1';
                    WB_RD_wire 		<=
                      std_logic_vector(resize(unsigned(data_rdata_i(15 downto 8)), 32));
                  when "10" =>
                    WB_RD_EN_wire 	<= '1';
                    WB_RD_wire 		<=
                      std_logic_vector(resize(unsigned(data_rdata_i(23 downto 16)), 32));
                  when "11" =>
                    WB_RD_EN_wire 	<= '1';
                    WB_RD_wire 		<=
                      std_logic_vector(resize(unsigned(data_rdata_i(31 downto 24)), 32));
                  when others =>
                    WB_RD_EN_wire 	<= '0';
                    null;
                end case;
              end if;
            end if;
          end if;
          
        when data_grant_waiting =>
          if instr_rvalid_IE_int = '0' then
            null;
          else
            if (load_err = '1') then
              WB_RD_EN_wire                             <= '0';
              pc_except_value_wire(harc_IE_int) 		<= pc_IE_int;
			  csr_wdata_en		<= '1';
              csr_wdata_i_wire                          <= LOAD_ERROR_EXCEPT_CODE;
            elsif (store_err = '1') then
              WB_RD_EN_wire                             <= '0';
              pc_except_value_wire(harc_IE_int) 		<= pc_IE_int;
			  csr_wdata_en		<= '1';
              csr_wdata_i_wire                          <= STORE_ERROR_EXCEPT_CODE;
            end if;
          end if;
          
        when csr_instr_wait_state =>
          csr_instr_req_wire <= '0';
          if (csr_instr_done = '1' and csr_access_denied_o = '0') then
            if (rd(instr_word_IE_int) /= 0) then
              WB_RD_EN_wire				<= '1';
              WB_RD_wire 				<= csr_rdata_o;
            else
              WB_RD_EN_wire <= '0';
              null;
            end if;
          elsif (csr_instr_done = '1' and csr_access_denied_o = '1') then
            WB_RD_EN_wire               		<= '0';
			csr_wdata_en		<= '1';
            csr_wdata_i_wire            		<= ILLEGAL_INSN_EXCEPT_CODE;
            pc_except_value_wire(harc_IE_int) 	<= pc_IE_int;
          else
            WB_RD_EN_wire <= '0';
          end if;
	when others =>
	  null;
      end case;
    end if;
  end process;

  fsm_IE_comb : process(all)

    variable PC_offset_wires                  : replicated_32b_reg;
    variable data_addr_internal_wires         : std_logic_vector (31 downto 0);
    variable data_wdata_o_wires               : std_logic_vector (31 downto 0);
    variable data_be_internal_wires           : std_logic_vector (3 downto 0);
    variable data_we_o_wires                  : std_logic;
    variable absolute_jump_wires              : std_logic;
    variable busy_IE_wires                    : std_logic;
    variable set_except_condition_wires       : std_logic;
    variable set_branch_condition_wires       : std_logic;
    variable taken_branch_wires               : std_logic;
    variable set_mret_condition_wires         : std_logic;
    variable set_wfi_condition_wires          : std_logic;
    variable jump_instr_wires                 : std_logic;
    variable branch_instr_wires               : std_logic;
    variable ebreak_instr_wires               : std_logic;
    variable dbg_ack_i_wires                  : std_logic;
    variable data_valid_waiting_counter_wires : std_logic;
	variable WFI_Instr_wires				  : std_logic;
    variable data_req_o_wires                 : std_logic;
    variable served_irq_wires                 : replicated_bit;
    variable nextstate_IE_wires               : fsm_IE_states;

  begin

    data_addr_internal_wires         := (others => '0');
    data_wdata_o_wires               := (others => '0');
    data_be_internal_wires           := (others => '0');
	served_irq_wires				 := (others => '0');
    data_we_o_wires                  := '0';
    absolute_jump_wires              := '0';
    busy_IE_wires                    := '0';
    set_except_condition_wires       := '0';
    set_branch_condition_wires       := '0';
    set_wfi_condition_wires          := '0';    
    taken_branch_wires               := '0';
    set_mret_condition_wires         := '0';
    jump_instr_wires                 := '0';
    branch_instr_wires               := '0';
    ebreak_instr_wires               := '0';
    dbg_ack_i_wires                  := '0';
    data_valid_waiting_counter_wires := '0';
	WFI_Instr_wires                  := '0';
    data_req_o_wires                 := '0';
    amo_store                        <= '0';
    amo_load_lat                     <= '0';
    nextstate_IE_wires               := sleep;
    reset_state                      <= '0';
    tmr_reg_IE_en <= '0';
    tmr_reg_WB_en <= '0';

    if rst_ni = '0' then
      if fetch_enable_i = '1' then
        null;
      else
        busy_IE_wires := '1';
      end if;
      nextstate_IE_wires := normal;
	tmr_reg_IE_en <= '0';
	tmr_reg_WB_en <= '0';
    else
      case state_IE_int is
        when sleep =>
          if dbg_req_o = '1' then
            dbg_ack_i_wires    := '1';
            busy_IE_wires      := '1';
            nextstate_IE_wires := sleep;
          elsif irq_i = '1' or fetch_enable_i = '1' then
            nextstate_IE_wires := normal;
          else
            busy_IE_wires      := '1';
            nextstate_IE_wires := sleep;
          end if;

        when reset =>
          reset_state <= '1';
          if dbg_req_o = '1' then
            dbg_ack_i_wires    := '1';
            busy_IE_wires      := '1';
            nextstate_IE_wires := reset;
          elsif fetch_enable_i = '0' then
            nextstate_IE_wires := reset;
            busy_IE_wires      := '1';
          else
            nextstate_IE_wires := normal;
          end if;

        when first_boot =>
          nextstate_IE_wires := normal;

        when debug =>
          dbg_ack_i_wires := '1';
          if dbg_req_o = '0' then
            nextstate_IE_wires := normal;
          else
            nextstate_IE_wires := debug;
            busy_IE_wires      := '1';
          end if;

        when normal =>
          if instr_rvalid_IE_int = '0' or flush_cycle_count(harc_IE) /=0  then
            nextstate_IE_wires := normal;
          elsif irq_pending(harc_IE)= '1' then
              nextstate_IE_wires         := normal;
              served_irq_wires(harc_IE) := '1';
              taken_branch_wires         := '1';
              if decoded_instruction_IE(WFI_bit_position) = '1' then
				WFI_Instr_wires		 := '1';
			  end if;
          else
 	    tmr_reg_IE_en <= '1';
 	    tmr_reg_WB_en <= '1';

            if decoded_instruction_IE(ADDI_bit_position) = '1' or decoded_instruction_IE(SLTI_bit_position) = '1'
              or decoded_instruction_IE(SLTIU_bit_position) = '1' or decoded_instruction_IE(ANDI_bit_position) = '1'
              or decoded_instruction_IE(ORI_bit_position) = '1' or decoded_instruction_IE(XORI_bit_position) = '1'
              or decoded_instruction_IE(SLLI_bit_position) = '1' or decoded_instruction_IE(SRLI7_bit_position) = '1'
              or decoded_instruction_IE(SRAI7_bit_position) = '1' then
              nextstate_IE_wires := normal;
            end if;

            if decoded_instruction_IE(LUI_bit_position) = '1' or decoded_instruction_IE(AUIPC_bit_position) = '1' then
              nextstate_IE_wires := normal;
            end if;

            if decoded_instruction_IE(ADD7_bit_position) = '1' or decoded_instruction_IE(SUB7_bit_position) = '1'
              or decoded_instruction_IE(SLT_bit_position) = '1' or decoded_instruction_IE(SLTU_bit_position) = '1'
              or decoded_instruction_IE(ANDD_bit_position) = '1' or decoded_instruction_IE(ORR_bit_position) = '1'
              or decoded_instruction_IE(XORR_bit_position) = '1' or decoded_instruction_IE(SLLL_bit_position) = '1'
              or decoded_instruction_IE(SRLL7_bit_position) = '1' or decoded_instruction_IE(SRAA7_bit_position) = '1' then
              nextstate_IE_wires := normal;
            end if;

            if decoded_instruction_IE(FENCE_bit_position) = '1' or decoded_instruction_IE(FENCEI_bit_position) = '1' then
              nextstate_IE_wires := normal;
            end if;

            if decoded_instruction_IE(JAL_bit_position) = '1' then
              nextstate_IE_wires                   := normal;
              jump_instr_wires                     := '1';
              set_branch_condition_wires           := '1';
              taken_branch_wires                   := '1';
              PC_offset_wires(harc_IE_int) := UJ_immediate(instr_word_IE_int);
            end if;

            if decoded_instruction_IE(JALR_bit_position) = '1' then
              nextstate_IE_wires         := normal;
              set_branch_condition_wires := '1';
              taken_branch_wires         := '1';
              PC_offset_wires(harc_IE_int) := std_logic_vector(signed(RS1_Data_IE)
                                                                       + signed(I_immediate(instr_word_IE_int)))
                                                      and X"FFFFFFFE";
              jump_instr_wires    := '1';
              absolute_jump_wires := '1';
            end if;

            if decoded_instruction_IE(BEQ_bit_position) = '1' then
              nextstate_IE_wires                   := normal;
              branch_instr_wires                   := '1';
              PC_offset_wires(harc_IE_int) := SB_immediate(instr_word_IE_int);
              if pass_BEQ_ID = '1' then
                set_branch_condition_wires := '1';
                taken_branch_wires         := '1';
              end if;
            end if;

            if decoded_instruction_IE(BNE_bit_position) = '1' then
              nextstate_IE_wires                   := normal;
              branch_instr_wires                   := '1';
              PC_offset_wires(harc_IE_int) := SB_immediate(instr_word_IE_int);
              if pass_BNE_ID = '1' then
                set_branch_condition_wires := '1';
                taken_branch_wires         := '1';
              end if;
            end if;

            if decoded_instruction_IE(BLT_bit_position) = '1' then
              nextstate_IE_wires                   := normal;
              branch_instr_wires                   := '1';
              PC_offset_wires(harc_IE_int) := SB_immediate(instr_word_IE_int);
              if pass_BLT_ID = '1' then
                set_branch_condition_wires := '1';
                taken_branch_wires         := '1';
              end if;
            end if;

            if decoded_instruction_IE(BLTU_bit_position) = '1' then
              nextstate_IE_wires                   := normal;
              branch_instr_wires                   := '1';
              PC_offset_wires(harc_IE_int) := SB_immediate(instr_word_IE_int);
              if pass_BLTU_ID = '1' then
                set_branch_condition_wires := '1';
                taken_branch_wires         := '1';
              end if;
            end if;

            if decoded_instruction_IE(BGE_bit_position) = '1' then
              nextstate_IE_wires                   := normal;
              branch_instr_wires                   := '1';
              PC_offset_wires(harc_IE_int) := SB_immediate(instr_word_IE_int);
              if pass_BGE_ID = '1' then
                set_branch_condition_wires := '1';
                taken_branch_wires         := '1';
              end if;
            end if;

            if decoded_instruction_IE(BGEU_bit_position) = '1' then
              nextstate_IE_wires                   := normal;
              branch_instr_wires                   := '1';
              PC_offset_wires(harc_IE_int) := SB_immediate(instr_word_IE_int);
              if pass_BGEU_ID = '1' then
                set_branch_condition_wires := '1';
                taken_branch_wires         := '1';
              end if;
            end if;

            if decoded_instruction_IE(LW_bit_position) = '1' or (decoded_instruction_IE(AMOSWAP_bit_position) = '1' and amo_store_lat = '0' and amo_load_skip = '0') then
              if amo_load = '0' then
                data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE) + signed(I_immediate(instr_word_IE_int)));
              elsif amo_load = '1' then
                data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE));
                amo_load_lat             <= '1';
              end if;
              data_be_internal_wires := "1111";
              data_req_o_wires       := '1';
              data_we_o_wires        := '0';
              if(data_addr_internal_wires(1 downto 0) = "00") then
                if (load_err = '1') then
                  nextstate_IE_wires         := normal;
                  set_except_condition_wires := '1';
                  taken_branch_wires         := '1';
                elsif data_gnt_i = '1' then
                  nextstate_IE_wires := data_valid_waiting;
                  busy_IE_wires      := '1';
                else
                  nextstate_IE_wires := data_grant_waiting;
                  busy_IE_wires      := '1';
                end if;
              else
                set_except_condition_wires := '1';
                taken_branch_wires         := '1';
                busy_IE_wires              := '1';
              end if;
            end if;

            if decoded_instruction_IE(LH_bit_position) = '1' or decoded_instruction_IE(LHU_bit_position) = '1' then  -- LH instruction
              data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE) + signed(I_immediate(instr_word_IE_int)));
              data_req_o_wires         := '1';
              data_we_o_wires          := '0';
              data_be_internal_wires   := "0011";
              if(data_addr_internal_wires(0) = '0') then
                if (load_err = '1') then
                  nextstate_IE_wires         := normal;
                  set_except_condition_wires := '1';
                  taken_branch_wires         := '1';
                elsif data_gnt_i = '1' then
                  nextstate_IE_wires := data_valid_waiting;
                  busy_IE_wires      := '1';
                else
                  nextstate_IE_wires := data_grant_waiting;
                  busy_IE_wires      := '1';
                end if;
              else
                set_except_condition_wires := '1';
                taken_branch_wires         := '1';
                busy_IE_wires              := '1';
              end if;
            end if;

            if decoded_instruction_IE(LB_bit_position) = '1' or decoded_instruction_IE(LBU_bit_position) = '1' then  -- LB instruction
              data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE) + signed(I_immediate(instr_word_IE_int)));
              data_req_o_wires         := '1';
              data_we_o_wires          := '0';
              data_be_internal_wires   := "0001";
              if (load_err = '1') then
                nextstate_IE_wires         := normal;
                set_except_condition_wires := '1';
                taken_branch_wires         := '1';
              elsif data_gnt_i = '1' then
                nextstate_IE_wires := data_valid_waiting;
                busy_IE_wires      := '1';
              else
                nextstate_IE_wires := data_grant_waiting;
                busy_IE_wires      := '1';
              end if;
            end if;

            if decoded_instruction_IE(SW_bit_position) = '1' or (decoded_instruction_IE(AMOSWAP_bit_position) = '1' and (amo_store_lat = '1' or amo_load_skip = '1')) then
              if amo_store_lat = '0' and amo_load_skip = '0'  then
                data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE) + signed(S_immediate(instr_word_IE_int)));
              elsif amo_store_lat = '1' or amo_load_skip = '1' then
                data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE));
                amo_load_lat             <= '0';
                amo_store                <= '1';
              end if;
              data_we_o_wires        := '1';
              data_be_internal_wires := "1111";
              if(data_addr_internal_wires(1 downto 0) = "00") then
                data_wdata_o_wires := RS2_Data_IE(31 downto 0);
                data_req_o_wires   := '1';
                if sw_mip = '1' then
                  busy_IE_wires      := '1';
                  nextstate_IE_wires := csr_instr_wait_state;
                elsif (store_err = '1') then
                  nextstate_IE_wires         := normal;
                  set_except_condition_wires := '1';
                  taken_branch_wires         := '1';
                elsif data_gnt_i = '1' then
                  nextstate_IE_wires := data_valid_waiting;
                  busy_IE_wires      := '1';
                else
                  nextstate_IE_wires := data_grant_waiting;
                  busy_IE_wires      := '1';
                end if;
              else
                set_except_condition_wires := '1';
                taken_branch_wires         := '1';
                busy_IE_wires              := '1';
              end if;
            end if;

            if decoded_instruction_IE(SH_bit_position) = '1' then
              data_we_o_wires := '1';
              data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE)
                                                           + signed(S_immediate(instr_word_IE_int)));
              case data_addr_internal_wires(1 downto 0) is
                when "00" =>
                  data_wdata_o_wires := RS2_Data_IE(31 downto 0);
                when "10" =>
                  data_wdata_o_wires := RS2_Data_IE(15 downto 0) & std_logic_vector(to_unsigned(0, 16));
                when others =>
                  null;
              end case;
              data_be_internal_wires := "0011";
              if(data_addr_internal_wires(0) = '0') then
                data_req_o_wires := '1';
                if (store_err = '1') then
                  nextstate_IE_wires         := normal;
                  set_except_condition_wires := '1';
                  taken_branch_wires         := '1';
                elsif data_gnt_i = '1' then
                  nextstate_IE_wires := data_valid_waiting;
                  busy_IE_wires      := '1';
                else
                  nextstate_IE_wires := data_grant_waiting;
                  busy_IE_wires      := '1';
                end if;
              else
                set_except_condition_wires := '1';
                taken_branch_wires         := '1';
                busy_IE_wires              := '1';
              end if;
            end if;

            if decoded_instruction_IE(SB_bit_position) = '1' then
              data_we_o_wires := '1';
              data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE)
                                                           + signed(S_immediate(instr_word_IE_int)));
              case data_addr_internal_wires(1 downto 0) is
                when "00" =>
                  data_wdata_o_wires := RS2_Data_IE(31 downto 0);
                when "01" =>
                  data_wdata_o_wires := RS2_Data_IE(23 downto 0) & std_logic_vector(to_unsigned(0, 8));
                when "10" =>
                  data_wdata_o_wires := RS2_Data_IE(15 downto 0) & std_logic_vector(to_unsigned(0, 16));
                when "11" =>
                  data_wdata_o_wires := RS2_Data_IE(7 downto 0) & std_logic_vector(to_unsigned(0, 24));
                when others =>
                  null;
              end case;
              data_req_o_wires       := '1';
              data_be_internal_wires := "0001";
              if (store_err = '1') then
                nextstate_IE_wires         := normal;
                set_except_condition_wires := '1';
                taken_branch_wires         := '1';
              elsif data_gnt_i = '1' then
                nextstate_IE_wires := data_valid_waiting;
                busy_IE_wires      := '1';
              else
                nextstate_IE_wires := data_grant_waiting;
                busy_IE_wires      := '1';
              end if;
            end if;

            if decoded_instruction_IE(CSRRW_bit_position) = '1' or decoded_instruction_IE(CSRRWI_bit_position) = '1' then
              nextstate_IE_wires := csr_instr_wait_state;
              busy_IE_wires      := '1';
            end if;

            if decoded_instruction_IE(CSRRC_bit_position) = '1' or decoded_instruction_IE(CSRRCI_bit_position) = '1'
              or decoded_instruction_IE(CSRRS_bit_position) = '1' or decoded_instruction_IE(CSRRSI_bit_position) = '1' then
              nextstate_IE_wires := csr_instr_wait_state;
              busy_IE_wires      := '1';
            end if;

            if decoded_instruction_IE(ECALL_bit_position) = '1' then
              nextstate_IE_wires         := normal;
              set_except_condition_wires := '1';
              taken_branch_wires         := '1';
            end if;

            if decoded_instruction_IE(EBREAK_bit_position) = '1' then
              ebreak_instr_wires := '1';
              nextstate_IE_wires := normal;
            end if;

            if decoded_instruction_IE(MRET_bit_position) = '1' then
			  set_mret_condition_wires := '1';
              taken_branch_wires       := '1';
              if fetch_enable_i = '0' then
                nextstate_IE_wires := sleep;
				busy_IE_wires      := '1';
              else
                nextstate_IE_wires := normal;
              end if;
            end if;

            if decoded_instruction_IE(WFI_bit_position) = '1' then
              if MSTATUS(harc_IE)(3) = '1' then
                set_wfi_condition_wires  := '1';
                taken_branch_wires       := '1';
              end if;
              nextstate_IE_wires := normal;
            end if;

            if decoded_instruction_IE(ILL_bit_position) = '1' then
              nextstate_IE_wires         := normal;
              set_except_condition_wires := '1';
              taken_branch_wires         := '1';
            end if;

            if decoded_instruction_IE(NOP_bit_position) = '1' then
              nextstate_IE_wires := normal;
            end if;

            if dbg_req_o = '1' then
              nextstate_IE_wires := debug;
              dbg_ack_i_wires    := '1';
              busy_IE_wires      := '1';
            end if;

          end if;

        when data_grant_waiting =>
	  tmr_reg_WB_en <= '1';
          data_req_o_wires := '1';
          if data_we_o_lat = '1' then
            if amo_store_lat = '0' then
              data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE)
                                                           + signed(S_immediate(instr_word_IE_int)));
              data_wdata_o_wires := RD_Data_IE(31 downto 0);
            else
              data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE));
              data_wdata_o_wires       := RS2_Data_IE(31 downto 0);
            end if;
          else
            if amo_load = '0' then
              data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE) + signed(I_immediate(instr_word_IE_int)));
            else
              data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE));
              amo_store                <= '1';
            end if;
          end if;

          if (load_err = '1') then
            nextstate_IE_wires         := normal;
            set_except_condition_wires := '1';
            taken_branch_wires         := '1';
          elsif (store_err = '1') then
            nextstate_IE_wires         := normal;
            set_except_condition_wires := '1';
            taken_branch_wires         := '1';
          elsif data_gnt_i = '1' then
            nextstate_IE_wires := data_valid_waiting;
            busy_IE_wires      := '1';
          else
            nextstate_IE_wires := data_grant_waiting;
            busy_IE_wires      := '1';
          end if;

        when data_valid_waiting =>
	  tmr_reg_WB_en <= '1';
          if data_we_o_lat = '1' then
            if amo_store_lat = '0' then
              data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE)
                                                           + signed(S_immediate(instr_word_IE_int)));
              data_wdata_o_wires := RD_Data_IE(31 downto 0);
            else
              data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE));
              data_wdata_o_wires       := RS2_Data_IE(31 downto 0);
            end if;
          else
            if amo_load = '0' then
              data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE) + signed(I_immediate(instr_word_IE_int)));
            else
              data_addr_internal_wires := std_logic_vector(signed(RS1_Data_IE));
              amo_store                <= '1';
              amo_load_lat             <= '1';
            end if;
          end if;

          if (load_err = '1') then
            nextstate_IE_wires         := normal;
            set_except_condition_wires := '1';
            taken_branch_wires         := '1';
          elsif (store_err = '1') then
            nextstate_IE_wires         := normal;
            set_except_condition_wires := '1';
            taken_branch_wires         := '1';
          elsif data_rvalid_i = '1' then
            if dbg_req_o = '1' then
              nextstate_IE_wires := debug;
              dbg_ack_i_wires    := '1';
              busy_IE_wires      := '1';
            else
              nextstate_IE_wires := normal;
              if amo_load_lat = '1' then
                busy_IE_wires := '1';
              end if;
            end if;
          else
            nextstate_IE_wires := data_valid_waiting;
            busy_IE_wires      := '1';
          end if;

        when csr_instr_wait_state =>
	  tmr_reg_WB_en <= '1';
          if (csr_instr_done = '0') then
            nextstate_IE_wires := csr_instr_wait_state;
            busy_IE_wires      := '1';
          elsif (csr_instr_done = '1' and csr_access_denied_o = '1') then
            nextstate_IE_wires         := normal;
            set_except_condition_wires := '1';
            taken_branch_wires         := '1';
          else
            nextstate_IE_wires := normal;
          end if;
	when others =>
	  null;
      end case;
    end if;

    PC_offset                  <= PC_offset_wires;
    data_addr_internal_wire    <= data_addr_internal_wires;
    data_wdata_o               <= data_wdata_o_wires;
    data_be_internal           <= data_be_internal_wires;
    data_we_o_wire             <= data_we_o_wires;
    absolute_jump              <= absolute_jump_wires;
    busy_IE                    <= busy_IE_wires;
    set_except_condition       <= set_except_condition_wires;
    set_branch_condition       <= set_branch_condition_wires;
    served_irq                 <= served_irq_wires;
    taken_branch               <= taken_branch_wires;
    set_mret_condition         <= set_mret_condition_wires;    
    set_wfi_condition          <= set_wfi_condition_wires;
    jump_instr                 <= jump_instr_wires;
    branch_instr               <= branch_instr_wires;
    ebreak_instr               <= ebreak_instr_wires;
    dbg_ack_i                  <= dbg_ack_i_wires;
    nextstate_IE               <= nextstate_IE_wires;
    data_valid_waiting_counter <= data_valid_waiting_counter_wires;
	WFI_Instr				   <= WFI_Instr_wires;
    data_req_o_wire_top        <= data_req_o_wires;
  end process;



  fsm_IE_state : process(clk_i, rst_ni)
  begin
    
    if rst_ni = '0' then
	  state_IE	<= tmr_FSM_assign(reset,3);
    elsif rising_edge(clk_i) then
      state_IE	<= tmr_FSM_assign(nextstate_IE,3);
    end if;
  end process;


fsm_WB_async : process(all)
begin

	WB_stage_en  <= '0';
	regfile_wire <= regfile(0);
	
	if rst_ni = '0' then
      for index in 0 to 31
      loop
			regfile_wire(index) <= (others => '0'); 
	  end loop;	
		
	elsif instr_rvalid_WB = '1' and WB_RD_EN = '1' and harc_WB = 0 then
		WB_stage_en 			 <= '1';
		regfile_wire(rd(instr_word_WB)) <= WB_RD;
	else
		WB_stage_en 			 <= '0';
		regfile_wire <= regfile(0);
   	end if;
	
  end process;

  
fsm_WB_seq_rf2 : process(clk_i, rst_ni)
begin
    if rst_ni = '0' then
      for index in 0 to 31
	  loop
          regfile_int(1)(index) <= std_logic_vector(to_unsigned(0, 32));  
		  regfile_int(2)(index) <= std_logic_vector(to_unsigned(0, 32));  
      end loop;
    elsif rising_edge(clk_i) then
      if instr_rvalid_WB = '1' and WB_RD_EN = '1' then
		if harc_WB = 1 then
        regfile_int(1)(rd(instr_word_WB)) <= WB_RD;
		elsif harc_WB = 2 then
		regfile_int(2)(rd(instr_word_WB)) <= WB_RD;
		end if;
      end if;
    end if;
  end process;
  
  
end Pipe;
