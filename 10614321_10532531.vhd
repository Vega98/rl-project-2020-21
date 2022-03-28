----------------------------------------------------------------------------------
-- Engineer: Davide Franchi / Francisco Nieto
-- 
-- Create Date: 07/31/2021 05:54:26 PM
-- Design Name: 
-- Module Name: project_reti_logiche 
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.NUMERIC_STD.ALL;

entity project_reti_logiche is
    port(
        i_clk       : in std_logic;
        i_rst       : in std_logic;
        i_start     : in std_logic;
        i_data      : in std_logic_vector(7 downto 0);
        o_address   : out std_logic_vector(15 downto 0);
        o_done      : out std_logic;
        o_en        : out std_logic;
        o_we        : out std_logic;
        o_data      : out std_logic_vector(7 downto 0)
        );
end project_reti_logiche;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.NUMERIC_STD.ALL;

entity addr_counter is
    port(
        i_clk       : in std_logic;
        i_rst       : in std_logic;
        i_max_addr  : in std_logic_vector(15 downto 0);         --Indirizzo massimo consentito, dato in input al counter
        i_enable    : in std_logic;                             --Attiva counter
        o_addr      : out std_logic_vector(15 downto 0);        --Segnale incrementato dal counter che esprime l'indirizzo su cui si deve lavorare
        o_maxed     : out std_logic                             --Segnale portato alto quando il contatore ha raggiunto l'indirizzo massimo consentito
        );
end addr_counter;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.NUMERIC_STD.ALL;

entity shift_calculator is
    port(
        i_clk       : in std_logic;
        i_rst       : in std_logic;
        i_data      : in std_logic_vector(7 downto 0);          --Pixel in input
        i_enable    : in std_logic;                             --Segnale di attivazione del modulo
        o_shift_val : out std_logic_vector(7 downto 0);         --Valore dello shift calcolato
        o_min_pixel : out std_logic_vector(7 downto 0)          --Valore del pixel minimo trovato
        );
end shift_calculator;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.NUMERIC_STD.ALL;

entity FF is
    port(
        i_clk       : in std_logic;
        i_load      : in std_logic;             --Segnale di sovrascrittura
        X           : in std_logic;             --Dato in ingresso
        Y           : out std_logic             --Dato memorizzato
        );
 end FF;
 
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.NUMERIC_STD.ALL;

entity N_reg is
    generic ( N : integer );
    port(
        i_clk       : in std_logic;
        i_load      : in std_logic;
        X           : in std_logic_vector(N-1 downto 0);
        Y           : out std_logic_vector(N-1 downto 0)
        );
end N_reg;


architecture addr_counter_arc of addr_counter is
    signal temp_addr : std_logic_vector(15 downto 0);
begin
    --Contatore
    count: process(i_clk, i_rst, i_enable)
    begin
        if(i_rst = '1') then
            temp_addr <= "0000000000000010";                    --Segnale di reset mi riporta all'indirizzo del primo pixel
            o_maxed <= '0';                    
        elsif( rising_edge(i_clk) and i_enable='1') then
            if(temp_addr = i_max_addr+1) then
                temp_addr <= "0000000000000010";                --Quando raggiungo il massimo torno all'indirizzo del primo pixel
                o_maxed <= '1';                                 --alzando il segnale o_maxed per avvertire gli altri moduli
            else
                temp_addr <= temp_addr + "0000000000000001";    --Incrementa di uno l'indirizzo
            end if;
        end if;
     end process;
     o_addr <= temp_addr;

end addr_counter_arc;


architecture shift_calculator_arc of shift_calculator is
    signal temp_max, temp_min               : std_logic_vector(7 downto 0);
    signal temp_shift                       : std_logic_vector(7 downto 0);
begin
    --Confronto pixel per determinare quale sia il max_value
    max: process(i_clk, i_rst, i_enable )
    begin
        if(i_rst = '1') then
            temp_max <= "00000000";
        elsif(rising_edge(i_clk) and i_enable = '1') then
            if(to_integer(unsigned(i_data)) > to_integer(unsigned(temp_max))) then temp_max <= i_data;      --Se il pixel è maggiore di temp_max, sostituisci temp_max con quello in input
            end if;
        end if;
    end process;
    
    --Confronto pixel per determinare quale sia il min_value
    min: process(i_clk, i_rst, i_enable)
    begin
        if (i_rst = '1') then
            temp_min <= "11111111";
        elsif (rising_edge(i_clk) and i_enable = '1') then
            if(to_integer(unsigned(i_data)) < to_integer(unsigned(temp_min))) then temp_min <= i_data; end if;
        end if;
    end process;
    
    --Calcola lo shift value con temp_max e temp_min
    shift_calculation: process(i_clk, i_rst, i_enable)
    variable delta_val : integer;
    begin
        if(i_rst = '1') then 
            temp_shift <= "00000000";
            delta_val := 0;
        elsif(rising_edge(i_clk) and i_enable = '1') then
            delta_val := to_integer(unsigned(temp_max - temp_min)); --Casting ad integer di delta_value per fare il controllo a soglia
 
            --Implementazione della funzione 8-FLOOR(LOG2(DELTA_VALUE+1)) con un costrutto case
            case delta_val + 1 is
                when 1=> --1
                    temp_shift <= "00001000";
                when 2 to 3 => --da 2 a 3
                    temp_shift <= "00000111";--7
                when 4 to 7 => --da 4 a 7
                    temp_shift <= "00000110";--6
                when 8 to 15 => --da 8 a 15
                    temp_shift <= "00000101";--5
                when 16 to 31 => --da 16 a 31
                    temp_shift <= "00000100";--4
                when 32 to 63 => --da 32 a 63
                    temp_shift <= "00000011";--3
                when 64 to 127 => --da 64 a 127
                    temp_shift <= "00000010";--2
                when 128 to 255 => --da 127 a 255
                    temp_shift <= "00000001";--1
                when 256 => --256
                    temp_shift <= "00000000";--0
                when others =>
                    temp_shift <= "ZZZZZZZZ"; --error
              end case;
          end if;
        end process;
 
        --Aggiorna i segnali di output con i segnali temporanei
        o_shift_val <= temp_shift;
        o_min_pixel <= temp_min;
 
end shift_calculator_arc;

architecture FF_arc of FF is
begin
    flipflop: process ( i_clk, i_load )
    begin
       if ( rising_edge(i_clk) and i_load = '1' ) then
            Y <= X;  
       end if;
    end process;
end FF_arc;

architecture N_reg_arc of N_reg is
begin
    reg: process ( i_clk, i_load )
    begin
        if ( rising_edge(i_clk) and i_load = '1' ) then
            Y <= X;  
        end if;
    end process;
end N_reg_arc;

architecture project_reti_logiche_arc of project_reti_logiche is

    --Dichiarazione dei component
    
    component addr_counter is
    port(
        i_clk       : in std_logic;
        i_rst       : in std_logic;
        i_max_addr  : in std_logic_vector(15 downto 0);         
        i_enable    : in std_logic;
        o_addr      : out std_logic_vector(15 downto 0);        
        o_maxed     : out std_logic                             
        );
    end component;
    
    component shift_calculator is
    port(
        i_clk       : in std_logic;
        i_rst       : in std_logic;
        i_data      : in std_logic_vector(7 downto 0);        
        i_enable    : in std_logic;
        o_shift_val : out std_logic_vector(7 downto 0);          
        o_min_pixel : out std_logic_vector(7 downto 0)
        );
    end component;
    
    component FF is
    port(
        i_clk       : in std_logic;
        i_load      : in std_logic;
        X           : in std_logic;
        Y           : out std_logic 
        );
    end component;
    
    component N_reg is
    generic ( N : integer );
    port(
        i_clk       : in std_logic;
        i_load      : in std_logic;
        X           : in std_logic_vector(N-1 downto 0);
        Y           : out std_logic_vector(N-1 downto 0)
        );
    end component;
    
    --Dichiarazione dei segnali necessari all'architettura
               
    signal cols, rows, shift_val, t_data, t_min, o_tmp, bugfix_input, bugfix                                                                : std_logic_vector(7 downto 0) := "00000000";
    signal o_rdone, o_wdone, r_cnt_enable, r_cnt_enable_checked, w_cnt_enable, w_cnt_enable_checked, i_shf_enable, i_shifter_enable, t_done, t_en, t_we, shifter_enable, reset, inhibitor   : std_logic := '0';
    signal pixels_vectorized, pixels_double, r_addr, w_addr, t_address                                                                      : std_logic_vector(15 downto 0) := "0000000000000000";
    type status is (RST, IDLE, WAIT_START, S1, S2, S3, STALE_STATE, S4, PAUSE_COUNTERS, S5, S6, CALC_PIX);
    signal PS, NS                                                                                                                           : status := IDLE;
    signal flag : std_logic_vector(1 downto 0) := "11";
    
    --Segnali per la gestione di registri e flip-flop
    
    signal load_reset, load_r_cnt, load_w_cnt, load_cols, load_rows, reset_input, r_cnt_input, w_cnt_input, load_pixels, load_pixels_doub, load_bugfix : std_logic := '0';
    signal cols_input, rows_input                                                                                                                      : std_logic_vector(7 downto 0) := (others => '0');
    signal pixels_input, pixels_doub_input                                                                                                             : std_logic_vector(15 downto 0) := (others => '0');
                                              
    --Istanziazione dei moduli e mappatura dei segnali
    
begin  
    
    C_READ: addr_counter
        port map(i_clk => i_clk, i_rst => reset, i_max_addr => pixels_vectorized, i_enable => r_cnt_enable_checked, o_addr => r_addr, o_maxed => o_rdone);
        
    C_WRITE: addr_counter
        port map(i_clk => i_clk, i_rst => reset, i_max_addr => pixels_double, i_enable => w_cnt_enable_checked, o_addr => w_addr, o_maxed => o_wdone);
        
    S_CALC: shift_calculator
        port map(i_clk => i_clk, i_rst => reset, i_data => i_data, i_enable => i_shf_enable, o_shift_val => shift_val, o_min_pixel => t_min);
           
    RST_FF: FF
        port map(i_clk => i_clk, i_load => load_reset, X => reset_input, Y => reset);
        
    R_CNT_FF: FF
        port map(i_clk => i_clk, i_load => load_r_cnt, X => r_cnt_input, Y => r_cnt_enable);
    
    W_CNT_FF: FF
        port map(i_clk => i_clk, i_load => load_w_cnt, X => w_cnt_input, Y => w_cnt_enable);
        
    COLS_REG: N_reg
        generic map ( N => 8 )
        port map(i_clk => i_clk, i_load => load_cols, X => cols_input, Y => cols);
        
    ROWS_REG: N_reg
        generic map ( N => 8 )
        port map(i_clk => i_clk, i_load => load_rows, X => rows_input, Y => rows);
        
    --Questo registro esiste solo per fixare un bug
    BUG_FIX: N_reg
        generic map ( N => 8 )
        port map(i_clk => i_clk, i_load => load_bugfix, X => bugfix_input, Y => bugfix);
        
    PIXELS_REG: N_reg
        generic map ( N => 16 )
        port map(i_clk => i_clk, i_load => load_pixels, X => pixels_input, Y => pixels_vectorized);
        
    PIXELS_DOUB_REG: N_reg
        generic map ( N => 16 )
        port map(i_clk => i_clk, i_load => load_pixels_doub, X => pixels_doub_input, Y => pixels_double);
   
    --Processo che implementa la macchina a stati
    fsm: process ( PS, i_start, o_rdone, o_wdone, i_clk, i_data, cols, rows, pixels_vectorized, r_addr)
    begin
    if ( rising_edge(i_clk) ) then
    
        -- Assegnamento standard dei segnali di gestione dei registri
        load_bugfix <= '0';
        inhibitor <= '0';
        load_pixels <= '0';
        load_pixels_doub <= '0';
        load_rows <= '0';
        load_cols <= '0';
        load_w_cnt <= '0';
        load_r_cnt <= '0';
        load_reset <= '0';
        r_cnt_input <= '0';
        w_cnt_input <= '0';
        reset_input <= '0';
        bugfix_input <= (others => '0');
        cols_input <= (others => '0');
        rows_input <= (others => '0');
        pixels_input <= (others => '0');
        pixels_doub_input <= (others => '0');
        
            case PS is
                when RST =>
                    
                    load_reset <= '1';
                    reset_input <= '1';
                    NS <= IDLE;
               
                when IDLE =>
                
                    load_reset <= '1';
                    reset_input <= '0';
                    if (i_start = '0') then
                        NS <= IDLE;
                    else
                        NS <= WAIT_START;
                    end if;
                    
                when WAIT_START =>
                
                     load_cols <= '1';
                     cols_input <= i_data;
                     NS <= S1;
                     
                when S1 =>
                
                    load_rows <= '1';
                    rows_input <= i_data;
                    NS <= S2;
                    
                when S2 =>
                
                     load_bugfix <= '1';
                     bugfix_input <= i_data;
                     load_r_cnt <= '1';
                     r_cnt_input <= '1';
                     load_w_cnt <= '1';
                     w_cnt_input <= '1';
                     NS <= CALC_PIX;
                        
                when CALC_PIX =>
                    
                        load_pixels <= '1';
                        load_pixels_doub <= '1';
                        pixels_input <= std_logic_vector(to_unsigned( to_integer(unsigned(cols)) * to_integer(unsigned(rows)), 16));
                        pixels_doub_input <= std_logic_vector(to_unsigned( 2 * to_integer(unsigned(cols)) * to_integer(unsigned(rows)), 16));
                        i_shf_enable <= '1';
                        NS <= S3;
                    
                when S3 =>
                
                    if (pixels_vectorized = "0000000000000000") then NS <= S6;
                    elsif (pixels_vectorized = "0000000000000001") then NS <= STALE_STATE; else
                        if ( o_rdone = '0' ) then
                            if ( r_addr = pixels_vectorized ) then
                                load_w_cnt <= '1';
                                w_cnt_input <= '0';
                                load_r_cnt <= '1';
                                r_cnt_input <= '0';   
                            end if; 
                            NS <= S3;
                        else
                            NS <= STALE_STATE;
                        end if;
                    end if;
                    
                when STALE_STATE =>
                
                    NS <= S4;
                    
                when S4 =>
                
                    i_shf_enable <= '0'; 
                    shifter_enable <= '1';
                    NS <= PAUSE_COUNTERS;
                    load_w_cnt <= '1';
                    w_cnt_input <= '1';
                    load_r_cnt <= '1';
                    r_cnt_input <= '1';
                   
                when PAUSE_COUNTERS =>
                
                    inhibitor <= '1';
                    load_r_cnt <= '1';
                    r_cnt_input <= '0';
                    load_w_cnt <= '1';
                    w_cnt_input <= '0';
                    NS <= S5;
                    
                when S5 =>
                
                    if ( o_wdone = '0' ) then
                        NS <= S4;
                    else
                        NS <= S6;
                    end if;
                    
                when S6 =>
                
                    if ( i_start = '1' ) then
                        NS <= S6;
                    else    
                        NS <= RST;
                    end if;
                    
                when others =>
                
                    NS <= RST;
                    
              end case;
            end if;
          end process;
          
    --Inibitore per assicurarsi che durante la scrittura i contatori restano fermi
    
    r_cnt_enable_checked <= r_cnt_enable and ( not inhibitor );
    w_cnt_enable_checked <= r_cnt_enable and ( not inhibitor );
    
    --Processo che implementa il modulo shifter
    
    shifter: process ( i_clk, shifter_enable )
        variable temp : unsigned(15 downto 0);
        begin
           if ( rising_edge (i_clk) and shifter_enable = '1' ) then    
            temp := shift_left("00000000" & unsigned(i_data - t_min), to_integer(unsigned(shift_val)));
            if (temp > "0000000011111111") then o_tmp <= "11111111";
            else o_tmp <= std_logic_vector(temp(7 downto 0)); end if;
          end if;
        end process;       
    
       --Output selector
       
     with o_wdone select t_data <= --uscita temporanea collegata a o_data
        --"11111111" when "10", --255 qui serve allo shifter
        o_tmp when '0',
        bugfix when others;
        
     with PS select t_address <=
        "0000000000000001" when WAIT_START,
        r_addr when S1,
        r_addr when S2,
        r_addr when CALC_PIX,
        r_addr when S3,
        r_addr when STALE_STATE,
        w_addr when PAUSE_COUNTERS,
        w_addr when S4,
        r_addr when S5,
        "0000000000000000" when others;
          
     with PS select t_done <=
        '1' when S6,
        '0' when others;
          
     with PS select t_en <=
        '1' when others;
          
     with PS select t_we <=
        '1' when PAUSE_COUNTERS,
        '0' when others;
        
   
       --Registro stati e output
       state_output: process ( i_clk )
          begin
             if( rising_edge( i_clk ) ) then
                if ( i_rst = '1' and i_start = '0') then
                    PS <= RST;
                    o_address <= "0000000000000000";
                    o_done <= '0';
                    o_en <= '0';
                    o_we <= '0';
                    o_data <= "00000000";
                else
                    PS <= NS;
                    o_address <= t_address;
                    o_done <= t_done;
                    o_en <= t_en;
                    o_we <= t_we;
                    o_data <= t_data;
                end if;
             end if;
          end process;
       

end project_reti_logiche_arc;