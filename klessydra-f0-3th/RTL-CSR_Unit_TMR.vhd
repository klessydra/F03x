-- RTL_CSR_Unit_TMR
-- Version:		0.3.2
-- All the CSR TMR




library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use std.textio.all;

use work.riscv_klessydra.all;
use work.thread_parameters_klessydra.all;

entity CSR_Unit is
  port (
    pc_IE                      : in  std_logic_vector(31 downto 0);
    harc_IE                    : in  harc_range;
    harc_to_csr                : in  harc_range;
    instr_word_IE              : in  std_logic_vector(31 downto 0);
    served_except_condition    : in  replicated_bit;
    served_mret_condition      : in  replicated_bit;
    served_irq                 : in  replicated_bit;
    pc_except_value            : in  replicated_32b_reg;
    dbg_req_o                  : in  std_logic;
    data_addr_internal         : in  std_logic_vector(31 downto 0);
    jump_instr                 : in  std_logic;
    branch_instr               : in  std_logic;
    data_valid_waiting_counter : in  std_logic;
    set_branch_condition       : in  std_logic;
    csr_instr_req              : in  std_logic;
    misaligned_err             : in  std_logic;
    WFI_Instr                  : in  std_logic;
    csr_wdata_i                : in  std_logic_vector (31 downto 0);
    csr_op_i                   : in  std_logic_vector (2 downto 0);
    csr_addr_i                 : in  std_logic_vector (11 downto 0);
    csr_instr_done             : out std_logic;
    csr_access_denied_o        : out std_logic;
    csr_rdata_o                : out std_logic_vector (31 downto 0);
    MSTATUS                    : out replicated_32b_reg;
    MEPC                       : out replicated_32b_reg;
    MCAUSE                     : out replicated_32b_reg;
    MIP                        : out replicated_32b_reg;
    MTVEC                      : out replicated_32b_reg;
    fetch_enable_i             : in  std_logic;
    clk_i                      : in  std_logic;
    rst_ni                     : in  std_logic;
    cluster_id_i               : in  std_logic_vector(5 downto 0);
    instr_rvalid_i             : in  std_logic;
    data_we_o_wire_top         : in  std_logic;
    data_req_o_wire_top        : in  std_logic;
    data_gnt_i                 : in  std_logic;
    irq_i                      : in  std_logic;
    irq_id_i                   : in  std_logic_vector(4 downto 0);
    irq_id_o                   : out std_logic_vector(4 downto 0);
    irq_ack_o                  : out std_logic
    );
end entity;


architecture CSR of CSR_Unit is
--
  
--
  signal pc_IE_replicated	: replicated_32b_reg;	
	
  signal PCCRs       : replicated_32b_reg;
  signal PCER        : replicated_32b_reg;
  signal PCMR        : replicated_32b_reg;
  signal MCPUID      : replicated_32b_reg;
  signal MIMPID      : replicated_32b_reg;
  signal MHARTID     : replicated_32b_reg;
  signal MIRQ        : replicated_32b_reg;
  signal MBADADDR    : replicated_32b_reg;

  signal MCYCLE        : replicated_32b_reg;
  signal MINSTRET      : replicated_32b_reg;
  signal MHPMCOUNTER3  : replicated_32b_reg;
  signal MCYCLEH       : replicated_32b_reg;
  signal MINSTRETH     : replicated_32b_reg;
  signal MHPMEVENT3    : replicated_bit;



  signal csr_instr_req_replicated       	: replicated_bit;
  signal csr_instr_done_replicated      	: replicated_bit;
  signal csr_instr_done_replicated_out  	: replicated_bit;
  signal csr_access_denied_o_replicated 	: replicated_bit;
  signal csr_access_denied_o_replicated_out : replicated_bit;
  signal csr_rdata_o_replicated         	: replicated_32b_reg;
  signal csr_rdata_o_replicated_out     	: replicated_32b_reg;

  signal MSTATUS_internal     	: replicated_32b_reg;
  signal MEPC_internal          : replicated_32b_reg;
  signal MCAUSE_internal        : replicated_32b_reg;
  signal MIP_internal           : replicated_32b_reg;
  signal MTVEC_internal         : replicated_32b_reg;
  
  -----------------------------------------------------------------
  -- TMR enable signals
  -----------------------------------------------------------------
  signal MSTATUS_en				: replicated_bit;
  signal MEPC_en				: replicated_bit;
  signal MCAUSE_en				: replicated_bit;
  signal MIP_en					: replicated_bit;
  signal MTVEC_en				: replicated_bit;
  signal csr_rdata_en			: replicated_bit;
  signal csr_instr_done_en		: replicated_bit;
  signal irq_id_en				: std_logic;
  
  
  signal irq_ack_o_wire			: std_logic;
  signal irq_ack_o_internal     : std_logic;
  signal irq_id_i_int			: std_logic_vector (4 downto 0);
  signal trap_hndlr         	: replicated_bit;
  
 -------------------------------------------------------------------
 -- TMR 
 -------------------------------------------------------------------
  signal reg_err : std_logic;
  constant DataWidth : integer := 32 ;
  
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
  
component TMR_REG_rst_0 is
    generic (DataWidth : integer );
    port (
        tmr_reg_i_en            : in std_logic;
        tmr_reg_i_clk           : in std_logic;
        tmr_reg_i_data          : in std_logic_vector(DataWidth-1 downto 0);
        tmr_reg_i_sync_rst_n    : in std_logic;
        tmr_reg_i_async_rst_n   : in std_logic;
        tmr_reg_o_data          : out std_logic_vector(DataWidth-1 downto 0)
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
  
  component STD_REG is
	generic (DataWidth : integer);
	port (
		std_reg_i_en			: in std_logic;
		std_reg_i_clk			: in std_logic;
		std_reg_i_data			: in std_logic_vector(DataWidth-1 downto 0);
		std_reg_o_data			: out std_logic_vector(DataWidth-1 downto 0)
	);
  end component;
  
begin

  TMR_CMP_irq_id : TMR_REG_rst_0
	generic map ( DataWidth => 5 )
	port map (
		tmr_reg_i_en			=>	irq_id_en,
		tmr_reg_i_clk			=>	clk_i,
		tmr_reg_i_data			=>	irq_id_i_int,
		tmr_reg_i_sync_rst_n	=>	'1',
		tmr_reg_i_async_rst_n	=>	rst_ni,
		tmr_reg_o_data			=>	irq_id_o
	);

  sTMR_CMP_irq_ack_o : TMR_sREG
	port map (
		tmr_sreg_i_en			=>	'1',
		tmr_sreg_i_clk			=>	clk_i,
		tmr_sreg_i_data			=>	irq_ack_o_wire,
		tmr_sreg_i_sync_rst_n	=>	'1',
		tmr_sreg_i_async_rst_n	=>	rst_ni,
		tmr_sreg_o_data			=>	irq_ack_o_internal
	);
	
  CSR_updating_logic : for h in harc_range generate

	TMR_CMP_MSTATUS : TMR_REG
	generic map ( DataWidth => DataWidth )
	port map (
		tmr_reg_i_en			=>	MSTATUS_en(h),
		tmr_reg_i_clk			=>	clk_i,
		tmr_reg_i_data			=>	MSTATUS_internal(h),
		tmr_reg_i_sync_rst_n	=>	'1',
		tmr_reg_i_async_rst_n	=>	rst_ni,
		tmr_reg_o_data			=>	MSTATUS(h)	
	);

	
	TMR_CMP_MEPC : TMR_REG_rst_0
	generic map ( DataWidth => DataWidth )
	port map (
		tmr_reg_i_en			=>	MEPC_en(h),
		tmr_reg_i_clk			=>	clk_i,
		tmr_reg_i_data			=>	MEPC_internal(h),
		tmr_reg_i_sync_rst_n	=>	'1',
		tmr_reg_i_async_rst_n	=>	rst_ni,
		tmr_reg_o_data			=>	MEPC(h)	
	);
	
	TMR_CMP_MCAUSE : TMR_REG_rst_0
	generic map ( DataWidth => DataWidth )
	port map (
		tmr_reg_i_en			=>	MCAUSE_en(h),
		tmr_reg_i_clk			=>	clk_i,
		tmr_reg_i_data			=>	MCAUSE_internal(h),
		tmr_reg_i_sync_rst_n	=>	'1',
		tmr_reg_i_async_rst_n	=>	rst_ni,
		tmr_reg_o_data			=>	MCAUSE(h)
	);
	
	TMR_CMP_MTVEC : TMR_REG_rst_0
	generic map ( DataWidth => DataWidth )
	port map (
		tmr_reg_i_en			=>	MTVEC_en(h),
		tmr_reg_i_clk			=>	clk_i,
		tmr_reg_i_data			=>	MTVEC_internal(h),
		tmr_reg_i_sync_rst_n	=>	'1',
		tmr_reg_i_async_rst_n	=>	'1',
		tmr_reg_o_data			=>	MTVEC(h)
	);
	
	TMR_CMP_MIP : TMR_REG_rst_0
	generic map ( DataWidth => DataWidth )
	port map (
		tmr_reg_i_en			=>	MIP_en(h),
		tmr_reg_i_clk			=>	clk_i,
		tmr_reg_i_data			=>	MIP_internal(h),
		tmr_reg_i_sync_rst_n	=>	'1',
		tmr_reg_i_async_rst_n	=>	rst_ni,
		tmr_reg_o_data			=>	MIP(h)
	);
	
	sTMR_CMP_csr_instr_done : TMR_sREG
	port map (
		tmr_sreg_i_en			=>	csr_instr_done_en(h),
		tmr_sreg_i_clk			=>	clk_i,
		tmr_sreg_i_data			=>	csr_instr_done_replicated(h),
		tmr_sreg_i_sync_rst_n	=>	'1',
		tmr_sreg_i_async_rst_n	=>	rst_ni,
		tmr_sreg_o_data			=>	csr_instr_done_replicated_out(h)
	);
	
	TMR_CMP_csr_rdata_o : TMR_REG_rst_0
	generic map ( DataWidth => DataWidth )
	port map (
		tmr_reg_i_en			=>	csr_rdata_en(h),
		tmr_reg_i_clk			=>	clk_i,
		tmr_reg_i_data			=>	csr_rdata_o_replicated(h),
		tmr_reg_i_sync_rst_n	=>	'1',
		tmr_reg_i_async_rst_n	=>	rst_ni,
		tmr_reg_o_data			=>	csr_rdata_o_replicated_out(h)
	);
	
	sTMR_CMP_csr_access_denied : TMR_sREG
	port map (
		tmr_sreg_i_en			=>	'1',
		tmr_sreg_i_clk			=>	clk_i,
		tmr_sreg_i_data			=>	csr_access_denied_o_replicated(h),
		tmr_sreg_i_sync_rst_n	=>	'1',
		tmr_sreg_i_async_rst_n	=>	rst_ni,
		tmr_sreg_o_data			=>	csr_access_denied_o_replicated_out(h)
	);
	
    MCPUID(h) <= std_logic_vector(to_unsigned(256, 32));
    MIMPID(h) <= std_logic_vector(to_unsigned(32768, 32));
    MHARTID(h) <= std_logic_vector(resize(unsigned(cluster_id_i) &
                                          to_unsigned(harc_IE, THREAD_ID_SIZE), 32));

    MIRQ(h) <= "0000000000000000000000000" & irq_id_i & "00";
	pc_IE_replicated(h) <= pc_IE when h = harc_IE else (others => '0'); -- test
    csr_instr_req_replicated(h) <= '1' when csr_instr_req = '1' and harc_to_csr = h else '0';
    trap_hndlr(h)               <= '1' when pc_IE_replicated(h) = MTVEC_RESET_VALUE(h)  else '0' when (pc_IE_replicated(h) = MEPC_internal(h)) or (pc_IE_replicated(h) = std_logic_vector(unsigned(MEPC_internal(h)) + 4));

    CSR_unit_op : process(clk_i,rst_ni)

      variable MIP_3_wire : replicated_bit;

    begin

      if rst_ni = '0' then
        PCER(h)                           <= PCER_RESET_VALUE(h);
        MCYCLE(h)                         <= x"00000000";
        MINSTRET(h)                       <= x"00000000";
        MHPMCOUNTER3(h)                   <= x"00000000";
        MCYCLEH(h)                        <= x"00000000";
        MINSTRETH(h)                      <= x"00000000";
        MHPMEVENT3(h)                     <= PCER_RESET_VALUE(h)(2);

      elsif rising_edge(clk_i) then

		if served_irq(h) = '1' and MIP(h)(11) = '1' then
			null;
		  
		elsif served_irq(h) = '1' and MIP(h)(3) = '1' then
			null;
		  
		elsif served_irq(h) = '1' and MIP(h)(7) = '1' then
			null;
			
		elsif served_except_condition(h) = '1' then
			if misaligned_err = '1' then
				MBADADDR(h) <= data_addr_internal;
			end if;

		elsif served_mret_condition(h) = '1' then
			null;

		elsif(csr_instr_done_replicated_out(h) = '1') then
			null;
		
        elsif csr_instr_req_replicated(h) = '1' then
          if (csr_op_i /= "000" and csr_op_i /= "100") then
            case csr_addr_i is
              when MCYCLE_addr =>
                case csr_op_i is
                  when CSRRW|CSRRWI =>
                    MCYCLE(h)        <= csr_wdata_i;
                  when CSRRS|CSRRSI =>
                    if(rs1(instr_word_IE) /= 0) then
                      MCYCLE(h) <= (MCYCLE(h) or csr_wdata_i);
                    end if;
                  when CSRRC|CSRRCI =>
                    if(rs1(instr_word_IE) /= 0) then
                      MCYCLE(h) <= (MCYCLE(h) and not(csr_wdata_i));
                    end if;
                  when others =>
                    null;
                end case;

              when MINSTRET_addr =>
                case csr_op_i is
                  when CSRRW|CSRRWI =>
                    MINSTRET(h)      <= csr_wdata_i;
                  when CSRRS|CSRRSI =>
                    if(rs1(instr_word_IE) /= 0) then
                      MINSTRET(h) <= (MINSTRET(h) or csr_wdata_i);
                    end if;
                  when CSRRC|CSRRCI =>
                    if(rs1(instr_word_IE) /= 0) then
                      MINSTRET(h) <= (MINSTRET(h) and not(csr_wdata_i));
                    end if;
                  when others =>
                    null;
                end case;

              when MCYCLEH_addr =>
                case csr_op_i is
                  when CSRRW|CSRRWI =>
                    MCYCLEH(h)       <= csr_wdata_i;
                  when CSRRS|CSRRSI =>
                    if(rs1(instr_word_IE) /= 0) then
                      MCYCLEH(h) <= (MCYCLEH(h) or csr_wdata_i);
                    end if;
                  when CSRRC|CSRRCI =>
                    if(rs1(instr_word_IE) /= 0) then
                      MCYCLEH(h) <= (MCYCLEH(h) and not(csr_wdata_i));
                    end if;
                  when others =>
                    null;
                end case;

              when MINSTRETH_addr =>
                case csr_op_i is
                  when CSRRW|CSRRWI =>
                    MINSTRETH(h) <= csr_wdata_i;
                  when CSRRS|CSRRSI =>
                    if(rs1(instr_word_IE) /= 0) then
                      MINSTRETH(h) <= (MINSTRETH(h) or csr_wdata_i);
                    end if;
                  when CSRRC|CSRRCI =>
                    if(rs1(instr_word_IE) /= 0) then
                      MINSTRETH(h) <= (MINSTRETH(h) and not(csr_wdata_i));
                    end if;
                  when others =>
                    null;
                end case;

              when MHPMCOUNTER3_addr =>
                case csr_op_i is
                  when CSRRW|CSRRWI =>
                    MHPMCOUNTER3(h)           <= csr_wdata_i;
                  when CSRRS|CSRRSI =>
                    if(rs1(instr_word_IE) /= 0) then
                      MHPMCOUNTER3(h) <= (MHPMCOUNTER3(h) or csr_wdata_i);
                    end if;
                  when CSRRC|CSRRCI =>
                    if(rs1(instr_word_IE) /= 0) then
                      MHPMCOUNTER3(h) <= (MHPMCOUNTER3(h) and not(csr_wdata_i));
                    end if;
                  when others =>
                    null;
                end case;


              when PCER_addr =>
                case csr_op_i is
                  when CSRRW|CSRRWI =>
                    PCER(h)          		  <= csr_wdata_i;
                    MHPMEVENT3(h)             <= csr_wdata_i(2);

                  when CSRRS|CSRRSI =>
                    if(rs1(instr_word_IE) /= 0) then
                      PCER(h)      			<= (PCER(h) or csr_wdata_i);
                      MHPMEVENT3(h)  		<= (PCER(h)(2) or csr_wdata_i(2));

                    end if;
                  when CSRRC|CSRRCI =>
                    if(rs1(instr_word_IE) /= 0) then
                      PCER(h)				<= (PCER(h) and not(csr_wdata_i));
                      MHPMEVENT3(h)  		<= (PCER(h)(2) and not (csr_wdata_i(2)));
                    end if;
                  when others =>
                    null;
                end case;

              when MHPMEVENT3_addr =>
                case csr_op_i is
                  when CSRRW|CSRRWI =>
                    MHPMEVENT3(h)             <= csr_wdata_i(2);
                    PCER(h)(2)       		  <= csr_wdata_i(2);
                  when CSRRS|CSRRSI =>
                    if(rs1(instr_word_IE) /= 0) then
                      MHPMEVENT3(h) 		<= (MHPMEVENT3(h) or csr_wdata_i(2));
                      PCER(h)(2)   			<= (PCER(h)(2) or csr_wdata_i(2));
                    end if;
                  when CSRRC|CSRRCI =>
                    if(rs1(instr_word_IE) /= 0) then
                      MHPMEVENT3(h) <= (MHPMEVENT3(h) and not(csr_wdata_i(2)));
                    end if;
                  when others =>
                    null;
                end case;

              when others =>
                null;
            end case;
          else
            null;
          end if;
        end if;


        if dbg_req_o = '0' then
          if (PCER(h)(0) = '1'
              and not(csr_instr_req = '1'
                      and (csr_addr_i = (MCYCLE_addr)
                           or csr_addr_i = MCYCLEH_addr)
                      and (csr_op_i = CSRRWI
                           or csr_op_i = CSRRW
                           or (csr_op_i = CSRRS and rs1(instr_word_IE) /= 0)
                           or (csr_op_i = CSRRSI and rs1(instr_word_IE) /= 0)
                           or (csr_op_i = CSRRC and rs1(instr_word_IE) /= 0)
                           or (csr_op_i = CSRRCI and rs1(instr_word_IE) /= 0)
                           )
                      )
              )
          then
            if(MCYCLE(h) = x"FFFFFFFF") then
              MCYCLEH(h) <= std_logic_vector(unsigned(MCYCLEH(h))+1);
              MCYCLE(h)  <= x"00000000";
            else
              MCYCLE(h) <= std_logic_vector(unsigned(MCYCLE(h))+1);
            end if;
          end if;

          if (PCER(h)(1) = '1'
              and not(csr_instr_req = '1'
                      and (csr_addr_i = (MINSTRET_addr)
                           or csr_addr_i = MINSTRETH_addr)
                      and (csr_op_i = CSRRWI
                           or csr_op_i = CSRRW
                           or (csr_op_i = CSRRS and rs1(instr_word_IE) /= 0)
                           or (csr_op_i = CSRRSI and rs1(instr_word_IE) /= 0)
                           or (csr_op_i = CSRRC and rs1(instr_word_IE) /= 0)
                           or (csr_op_i = CSRRCI and rs1(instr_word_IE) /= 0)
                           )
                      )
              )
          then
            if(instr_rvalid_i = '1') then
              if (MINSTRET(h) = x"FFFFFFFF") then
                MINSTRETH(h) <= std_logic_vector(unsigned(MINSTRETH(h))+1);
                MINSTRET(h)  <= x"00000000";
              else
                MINSTRET(h) <= std_logic_vector(unsigned(MINSTRET(h))+1);
              end if;
            end if;
          end if;

          if (PCER(h)(2) = '1') then
            if (((data_req_o_wire_top = '1' and data_gnt_i = '0') and data_valid_waiting_counter = '0') or ((not(data_req_o_wire_top = '1' and data_gnt_i = '0')) and data_valid_waiting_counter = '1')) then
              MHPMCOUNTER3(h) <= std_logic_vector(unsigned(MHPMCOUNTER3(h))+1);
            elsif((data_req_o_wire_top = '1' and data_gnt_i = '0') and (data_valid_waiting_counter = '1')) then
              MHPMCOUNTER3(h) <= std_logic_vector(unsigned(MHPMCOUNTER3(h))+2);
            end if;
          end if;

        end if;
      end if;
    end process;

---OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO-	
---OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO-	
---OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO-	
---OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO-
	
	CSR_unit_async_op : process(all)
		
	begin
		MSTATUS_internal(h)			<= (others => '0');
		MSTATUS_internal(h)(3)		<= MSTATUS(h)(3);
		MSTATUS_internal(h)(7)		<= MSTATUS(h)(7);
		MEPC_internal(h)            <= MEPC(h);    
		MCAUSE_internal(h)          <= MCAUSE(h);  
		MTVEC_internal(h)           <= MTVEC(h);   
		MIP_internal(h)             <= MIP(h);     
		MSTATUS_en(h)				<= '0';
		MEPC_en(h)					<= '0';
		MCAUSE_en(h)				<= '0';
		MIP_en(h)					<= '0';
		MTVEC_en(h)					<= '0';
	    csr_rdata_en(h)             <= '0';
		csr_instr_done_en(h)        <= '0';
		
		
		  csr_instr_done_replicated(h)      <= csr_instr_done_replicated_out(h);
          csr_access_denied_o_replicated(h) <= csr_access_denied_o_replicated_out(h);
          csr_rdata_o_replicated(h)         <= csr_rdata_o_replicated_out(h);

		if rst_ni = '0' then
          MSTATUS_internal(h)		    	<= MSTATUS_RESET_VALUE;
          MEPC_internal(h)                  <= MEPC_RESET_VALUE;
          MCAUSE_internal(h)                <= MCAUSE_RESET_VALUE;
          MTVEC_internal(h)                 <= MTVEC_RESET_VALUE(h);
		  MIP_internal(h)                   <= MIP_RESET_VALUE;
		  MSTATUS_en(h)						<= '0';
		  MEPC_en(h)						<= '0';
		  MCAUSE_en(h)						<= '0';
		  MIP_en(h)							<= '0';
		  MTVEC_en(h)						<= '1';
		  csr_rdata_en(h)             		<= '0';
		  csr_instr_done_en(h)              <= '0';
		  
		  
	    else
			
			if served_irq(h) = '1' and MIP(h)(11) = '1' then
			  MSTATUS_en(h)  <= '1';
			  MCAUSE_en(h)   <= '1';
			  MCAUSE_internal(h) <= "1" & std_logic_vector(to_unsigned(11, 31));
			  if trap_hndlr(h) = '0' then
			    MEPC_en(h) <= '1';
				MEPC_internal(h) <= pc_IE;
			  end if;     
			  if WFI_Instr = '1' then
				MCAUSE_internal(h)(30) <= '1';
			  else
				MCAUSE_internal(h)(30) <= '0';
			  end if;
			  MSTATUS_internal(h)(3) <= '0';
			  MSTATUS_internal(h)(7) <= MSTATUS(h)(3);
			elsif served_irq(h) = '1' and MIP(h)(3) = '1' then
			  MIP_en(h)				<= '1';
			  MCAUSE_en(h) 			<= '1';
			  MSTATUS_en(h)  		<= '1';
			  MCAUSE_internal(h) 	<= "1" & std_logic_vector(to_unsigned(3, 31));
			  MIP_internal(h)(3) 	<= '0';
			  if trap_hndlr(h) = '0' then
			    MEPC_en(h) <= '1';
				MEPC_internal(h) <= pc_IE;
			  end if;     
			  if WFI_Instr = '1' then
				MCAUSE_internal(h)(30) <= '1';
			  else
				MCAUSE_internal(h)(30) <= '0';
			  end if;
			  MSTATUS_internal(h)(3) <= '0';
			  MSTATUS_internal(h)(7) <= MSTATUS(h)(3);
			elsif served_irq(h) = '1' and MIP(h)(7) = '1' then
			  MCAUSE_en(h) 			<= '1';
			  MSTATUS_en(h)  		<= '1';
			  MCAUSE_internal(h) 	<= "1" & std_logic_vector(to_unsigned(7, 31));
			  if trap_hndlr(h) = '0' then
			    MEPC_en(h) <= '1';
				MEPC_internal(h) <= pc_IE;
			  end if;   
			  if WFI_Instr = '1' then
				MCAUSE_internal(h)(30) <= '1';
			  else
				MCAUSE_internal(h)(30) <= '0';
			  end if;
			  MSTATUS_internal(h)(3) <= '0';
			  MSTATUS_internal(h)(7) <= MSTATUS(h)(3);
			  
			elsif served_except_condition(h) = '1' then
			  MCAUSE_en(h) 				<= '1';
			  MEPC_en(h) 				<= '1';
			  MSTATUS_en(h)  			<= '1';
			  MCAUSE_internal(h)     	<= csr_wdata_i;
			  MEPC_internal(h)       	<= pc_except_value(h);
			  MSTATUS_internal(h)(3) 	<= '0';
			  MSTATUS_internal(h)(7) 	<= '1';

			elsif served_mret_condition(h) = '1' then
			  MSTATUS_en(h)  		 <= '1';
			  MSTATUS_internal(h)(7) <= '1';
			  MSTATUS_internal(h)(3) <= MSTATUS(h)(7);

			elsif(csr_instr_done_replicated_out(h) = '1') then
			  csr_instr_done_en(h)              <= '1';
			  csr_instr_done_replicated(h)      <= '0';
			  csr_access_denied_o_replicated(h) <= '0';
			elsif csr_instr_req_replicated(h) = '1' then
			  csr_instr_done_en(h)         <= '1';
			  csr_instr_done_replicated(h) <= '1';
			  if (csr_op_i /= "000" and csr_op_i /= "100") then
				case csr_addr_i is
				  when MSTATUS_addr =>
					csr_rdata_en(h) <= '1';
					case csr_op_i is
					  when CSRRW|CSRRWI =>
					    MSTATUS_en(h)  				<= '1';
						csr_rdata_o_replicated(h) 	<= MSTATUS(h);
						MSTATUS_internal(h)(7)      <= csr_wdata_i(7);
						MSTATUS_internal(h)(3)      <= csr_wdata_i(3);
					  when CSRRS|CSRRSI =>
						csr_rdata_o_replicated(h) <= MSTATUS(h);
						if(rs1(instr_word_IE) /= 0) then
						  MSTATUS_en(h)  		 <= '1';
						  MSTATUS_internal(h)(7) <= (MSTATUS(h)(7) or csr_wdata_i(7));
						  MSTATUS_internal(h)(3) <= (MSTATUS(h)(3) or csr_wdata_i(3));
						end if;
					  when CSRRC|CSRRCI =>
						csr_rdata_o_replicated(h) <= MSTATUS(h);
						if(rs1(instr_word_IE) /= 0) then
						  MSTATUS_en(h)  			<= '1';
						  MSTATUS_internal(h)(7) 	<= MSTATUS(h)(7) and (not csr_wdata_i(7));
						  MSTATUS_internal(h)(3) 	<= MSTATUS(h)(3) and (not csr_wdata_i(3));
						end if;
					  when others =>
						null;
					end case;
				  when MIP_addr =>
				    csr_rdata_en(h) <= '1';
					case csr_op_i is
					  when CSRRW|CSRRWI =>
					    MIP_en(h)				<= '1';
						csr_rdata_o_replicated(h) <= (11 => MIP(h)(11), 7 => MIP(h)(7), 3 => MIP(h)(3), others => '0');
						MIP_internal(h)(3)             <= csr_wdata_i(3);
					  when CSRRS|CSRRSI =>
						csr_rdata_o_replicated(h) <= (11 => MIP(h)(11), 7 => MIP(h)(7), 3 => MIP(h)(3), others => '0');
						if(rs1(instr_word_IE) /= 0) then
						  MIP_en(h)				<= '1';
						  MIP_internal(h)(3) 	<= (MIP(h)(3) or csr_wdata_i(3));
						end if;
					  when CSRRC|CSRRCI =>
						csr_rdata_o_replicated(h) <= (11 => MIP(h)(11), 7 => MIP(h)(7), 3 => MIP(h)(3), others => '0');
						if(rs1(instr_word_IE) /= 0) then
						  MIP_en(h)				<= '1';
						  MIP_internal(h)(3) 	<= (MIP(h)(3) and (not csr_wdata_i(3)));
						end if;
					  when others =>
						null;
					end case;
				  when MEPC_addr =>
					csr_rdata_en(h) <= '1';
					case csr_op_i is
					  when CSRRW|CSRRWI =>
					    MEPC_en(h) <= '1';
						csr_rdata_o_replicated(h) <= MEPC(h);
						MEPC_internal(h)              <= csr_wdata_i;
					  when CSRRS|CSRRSI =>
						csr_rdata_o_replicated(h) <= MEPC(h);
						if(rs1(instr_word_IE) /= 0) then
						  MEPC_en(h) <= '1';
						  MEPC_internal(h) <= (MEPC(h) or csr_wdata_i);
						end if;
					  when CSRRC|CSRRCI =>
						csr_rdata_o_replicated(h) <= MEPC(h);
						if(rs1(instr_word_IE) /= 0) then
						  MEPC_en(h) <= '1';
						  MEPC_internal(h) <= (MEPC(h) and not(csr_wdata_i));
						end if;
					  when others =>
						null;
					end case;
				  when MTVEC_addr =>
				    csr_rdata_en(h) <= '1';
					case csr_op_i is
					  when CSRRW|CSRRWI =>
					    MTVEC_en(h)               	<= '1';
						csr_rdata_o_replicated(h) 	<= MTVEC(h);
						MTVEC_internal(h)         <= csr_wdata_i;					
					  when CSRRS|CSRRSI =>
						csr_rdata_o_replicated(h) <= MTVEC(h);
						if(rs1(instr_word_IE) /= 0) then
						  MTVEC_en(h)       <= '1';
						  MTVEC_internal(h) <= (MTVEC(h) or csr_wdata_i);
						end if;
					  when CSRRC|CSRRCI =>
						csr_rdata_o_replicated(h) <= MTVEC(h);
						if(rs1(instr_word_IE) /= 0) then
						  MTVEC_en(h)       <= '1';
						  MTVEC_internal(h) <= (MTVEC(h) and not(csr_wdata_i));
						end if;
					  when others =>
						null;
					end case;
				  when MCAUSE_addr =>
				    csr_rdata_en(h) <= '1';
					case csr_op_i is
					  when CSRRW|CSRRWI =>
					  	MCAUSE_en(h) <= '1';
						csr_rdata_o_replicated(h)  <= MCAUSE(h);
						MCAUSE_internal(h)(31)         <= csr_wdata_i(31);
						MCAUSE_internal(h)(4 downto 0) <= csr_wdata_i(4 downto 0);
					  when CSRRS|CSRRSI =>
						csr_rdata_o_replicated(h) <= MCAUSE(h);
						if(rs1(instr_word_IE) /= 0) then
						  MCAUSE_en(h) <= '1';
						  MCAUSE_internal(h)(31)         <= (MCAUSE(h)(31) or csr_wdata_i(31));
						  MCAUSE_internal(h)(4 downto 0) <= (MCAUSE(h)(4 downto 0) or csr_wdata_i(4 downto 0));
						end if;
					  when CSRRC|CSRRCI =>
						csr_rdata_o_replicated(h) <= MCAUSE(h);
						if(rs1(instr_word_IE) /= 0) then
						  MCAUSE_en(h) <= '1';
						  MCAUSE_internal(h)(4 downto 0) <= (MCAUSE(h)(4 downto 0) and not(csr_wdata_i(4 downto 0)));
						  MCAUSE_internal(h)(31)         <= (MCAUSE(h)(31) and not(csr_wdata_i(31)));
						end if;
					  when others =>
						null;
					end case;
				  when MCPUID_addr =>
					case csr_op_i is
					  when CSRRC|CSRRS|CSRRCI|CSRRSI =>
						if(rs1(instr_word_IE) = 0) then
						  csr_rdata_en(h) <= '1';
						  csr_rdata_o_replicated(h) <= MCPUID(h);
						else
						  csr_access_denied_o_replicated(h) <= '1';
						end if;
					  when CSRRW|CSRRWI =>
						csr_access_denied_o_replicated(h) <= '1';
					  when others =>
						null;
					end case;
				  when MIMPID_addr =>
					case csr_op_i is
					  when CSRRC|CSRRS|CSRRCI|CSRRSI =>
						if(rs1(instr_word_IE) = 0) then
						  csr_rdata_en(h) <= '1';
						  csr_rdata_o_replicated(h) <= MIMPID(h);
						else
						  csr_access_denied_o_replicated(h) <= '1';
						end if;
					  when CSRRW|CSRRWI =>
						csr_access_denied_o_replicated(h) <= '1';
					  when others =>
						null;
					end case;
				  when MHARTID_addr =>
					case csr_op_i is
					  when CSRRC|CSRRS|CSRRCI|CSRRSI =>
						if(rs1(instr_word_IE) = 0) then
						  csr_rdata_en(h) <= '1';
						  csr_rdata_o_replicated(h) <= MHARTID(h);
						else
						  csr_access_denied_o_replicated(h) <= '1';
						end if;
					  when CSRRW|CSRRWI =>
						csr_access_denied_o_replicated(h) <= '1';
					  when others =>
						null;
					end case;
				  when MIRQ_addr =>
					case csr_op_i is
					  when CSRRC|CSRRS|CSRRCI|CSRRSI =>
						if(rs1(instr_word_IE) = 0) then
						  csr_rdata_en(h) <= '1';
						  csr_rdata_o_replicated(h) <= MIRQ(h);
						else
						  csr_access_denied_o_replicated(h) <= '1';
						end if;
					  when CSRRW|CSRRWI =>
						csr_access_denied_o_replicated(h) <= '1';
					  when others =>
						null;
					end case;
				  when BADADDR_addr =>
					case csr_op_i is
					  when CSRRC|CSRRS|CSRRCI|CSRRSI =>
						if(rs1(instr_word_IE) = 0) then
						  csr_rdata_en(h) <= '1';
						  csr_rdata_o_replicated(h) <= MBADADDR(h);
						else
						  csr_access_denied_o_replicated(h) <= '1';
						end if;
					  when CSRRW|CSRRWI =>
						csr_access_denied_o_replicated(h) <= '1';
					  when others =>
						null;
					end case;
					
				  when MCYCLE_addr =>
				    csr_rdata_en(h) <= '1';
					case csr_op_i is
					  when CSRRW|CSRRWI =>
						csr_rdata_o_replicated(h) <= MCYCLE(h);
					  when CSRRS|CSRRSI =>
						csr_rdata_o_replicated(h) <= MCYCLE(h);
					  when CSRRC|CSRRCI =>
						csr_rdata_o_replicated(h) <= MCYCLE(h);
					  when others =>
						null;
					end case;
					
				  when MINSTRET_addr =>
				    csr_rdata_en(h) <= '1';
					case csr_op_i is
					  when CSRRW|CSRRWI =>
						csr_rdata_o_replicated(h) <= std_logic_vector(unsigned(MINSTRET(h))-1);
					  when CSRRS|CSRRSI =>
						csr_rdata_o_replicated(h) <= std_logic_vector(unsigned(MINSTRET(h))-1);
					  when CSRRC|CSRRCI =>
						csr_rdata_o_replicated(h) <= std_logic_vector(unsigned(MINSTRET(h))-1);
					  when others =>
						null;
					end case;
	  
				  when MCYCLEH_addr =>
				    csr_rdata_en(h) <= '1';
					case csr_op_i is
					  when CSRRW|CSRRWI =>
						csr_rdata_o_replicated(h) <= MCYCLEH(h);
					  when CSRRS|CSRRSI =>
						csr_rdata_o_replicated(h) <= MCYCLEH(h);
					  when CSRRC|CSRRCI =>
						csr_rdata_o_replicated(h) <= MCYCLEH(h);
					  when others =>
						null;
					end case;
	  
				  when MINSTRETH_addr =>
				    csr_rdata_en(h) <= '1';
					case csr_op_i is
					  when CSRRW|CSRRWI =>
						if(MINSTRET(h) = x"00000000" and MINSTRETH(h) /= x"00000000") then
						  csr_rdata_o_replicated(h) <= std_logic_vector(unsigned(MINSTRETH(h))-1);
						else
						  csr_rdata_o_replicated(h) <= MINSTRETH(h);
						end if;
					  when CSRRS|CSRRSI =>
						if(MINSTRET(h) = x"00000000" and MINSTRETH(h) /= x"00000000") then
						  csr_rdata_o_replicated(h) <= std_logic_vector(unsigned(MINSTRETH(h))-1);
						else
						  csr_rdata_o_replicated(h) <= MINSTRETH(h);
						end if;
					  when CSRRC|CSRRCI =>
						if(MINSTRET(h) = x"00000000" and MINSTRETH(h) /= x"00000000") then
						  csr_rdata_o_replicated(h) <= std_logic_vector(unsigned(MINSTRETH(h))-1);
						else
						  csr_rdata_o_replicated(h) <= MINSTRETH(h);
						end if;
					  when others =>
						null;
					end case;
	  
				  when MHPMCOUNTER3_addr =>
				    csr_rdata_en(h) <= '1';
					case csr_op_i is
					  when CSRRW|CSRRWI =>
						csr_rdata_o_replicated(h) <= MHPMCOUNTER3(h);
					  when CSRRS|CSRRSI =>
						csr_rdata_o_replicated(h) <= MHPMCOUNTER3(h);
					  when CSRRC|CSRRCI =>
						csr_rdata_o_replicated(h) <= MHPMCOUNTER3(h);
					  when others =>
						null;
				  end case;
	  
				  when PCER_addr =>
				    csr_rdata_en(h) <= '1';
					case csr_op_i is
					  when CSRRW|CSRRWI =>
						csr_rdata_o_replicated(h) <= PCER(h);
					  when CSRRS|CSRRSI =>
						csr_rdata_o_replicated(h) <= PCER(h);
					  when CSRRC|CSRRCI =>
						csr_rdata_o_replicated(h) <= PCER(h);
					  when others =>
						null;
					end case;

				  when MHPMEVENT3_addr =>
				    csr_rdata_en(h) <= '1';
					case csr_op_i is
					  when CSRRW|CSRRWI =>
						csr_rdata_o_replicated(h) <= (2 => MHPMEVENT3(h), others => '0');
					  when CSRRS|CSRRSI =>
						csr_rdata_o_replicated(h) <= (2 => MHPMEVENT3(h), others => '0');
					  when CSRRC|CSRRCI =>
						csr_rdata_o_replicated(h) <= (2 => MHPMEVENT3(h), others => '0');
					  when others =>
						null;
					end case;
					
				  when others =>
				    csr_rdata_en(h) <= '1';
					csr_rdata_o_replicated(h) <= (others => '0');
				end case;
			  else
				null;
			  end if;
			end if;	
			if h = 0 and unsigned(irq_id_i) >= 28 and irq_i = '1' then
			    MIP_en(h)			<= '1';
				MIP_internal(h)(7) 	<= '1';
			else
			    MIP_en(h)		   	<= '1';
				MIP_internal(h)(7) 	<= '0';
			end if;
			if h = 0 and unsigned(irq_id_i) < 28 and irq_i = '1' then
			    MIP_en(h)			<= '1';
				MIP_internal(h)(11) <= '1';
			else
			    MIP_en(h)			<= '1';
				MIP_internal(h)(11) <= '0';
			end if;
		end if;
	end process;
  end generate CSR_updating_logic;

  process(all)
    variable wire1, wire2 : std_logic;
  begin
    wire1 := '0'; wire2 := '0';
    for h in harc_range loop
      wire1 := wire1 or csr_instr_done_replicated_out(h);
      wire2 := wire2 or csr_access_denied_o_replicated_out(h);
    end loop;
    csr_instr_done      <= wire1;
    csr_access_denied_o <= wire2;
  end process;

  csr_rdata_o <= csr_rdata_o_replicated_out(harc_IE);

  irq_ack_o <= irq_ack_o_internal;
  
  irq_ack_manager : process(all)
    variable wire1 : std_logic;
  begin
      irq_id_i_int   <= irq_id_o;
	  irq_id_en      <= '0';
      wire1 := '0';
      for h in harc_range loop
        wire1 := wire1 or served_irq(h);
      end loop;
      case irq_ack_o_internal is
        when '0' =>
          if wire1 = '0' then
            irq_ack_o_wire <= '0';
          else
		    irq_id_en      <= '1';
            irq_ack_o_wire <= '1';
            irq_id_i_int       <= irq_id_i;
          end if;
        when '1' =>
          irq_ack_o_wire <= '0';
        when others => 
		  irq_id_i_int 		<= irq_id_o;
		  irq_ack_o_wire 	<= irq_ack_o_internal;
      end case;
	  
  end process irq_ack_manager;


end CSR;
