with Ada.Interrupts.Names;
with Ada.Unchecked_Conversion;
with System.OS_Interface;
with System.STM32F4; use System.STM32F4;
with System.STM32F4.GPIO; use System.STM32F4.GPIO;

with System.STM32F4.Reset_Clock_Control;
use System.STM32F4.Reset_Clock_Control;

with System.STM32F4.USART;
use System.STM32F4.USART;

package body Serial_Port is

   pragma Warnings (Off,
                    "*types for unchecked conversion have different sizes");

   function Bits_To_Char is
      new Ada.Unchecked_Conversion (Source => Bits_9, Target => Character);

   function Char_To_Bits is
      new Ada.Unchecked_Conversion (Source => Character, Target => Bits_9);

   -- A thread-safe buffer to store data coming from the serial port.
   protected Serial_Buffer is
      pragma Interrupt_Priority;

   private
      Receive_Buffer : String (1..Receive_Buffer_Size) := (others => ASCII.NUL);
      Buffered_Chars : Natural := 0;

      procedure Interrupt_Handler;
      pragma Attach_Handler
         (Interrupt_Handler,
          Ada.Interrupts.Names.USART2_Interrupt);

      procedure Read_Until(Buffer : out String; End_Char : in Character; Chars_Read : out Natural);

   end Serial_Buffer;

   -- Calculates the whole portion of the divider value.
   function USART_Mantissa(Baud_Rate : in Natural) return Mantissa is
   begin
      return Mantissa (System.OS_Interface.Ticks_Per_Second / (16 * Baud_Rate));
   end USART_Mantissa;

   -- Calculates the fractional part of the divider value. This is 16 times the fractional
   -- component, encoded into 4 bits.
   function USART_Fraction(Baud_Rate : in Natural) return Fraction is
      Remainder : constant Natural
        := System.OS_Interface.Ticks_Per_Second rem (16 * Baud_Rate);
   begin
      return Fraction (Remainder);
   end USART_Fraction;

   procedure Enable(Baud_Rate : in Natural) is
   begin
      -- Enable clock for USART2
      RCC.RCC_APB1ENR.USART2_Clock_Enable := True;

      -- Configure PA2 & 3 as alternate function, for USART2
      GPIOA.MODER(0..4) := (others => GPIO.Mode_AF);
      GPIOA.AFRL(0..4) := (others => GPIO.AF_USART2);
      -- Set the word size and number of stop/parity bits.
      -- We use 8n1 here.
      USART2.CR2.Stop_Bit := ONE_STOP_BIT;
      -- Configure the BRR register to set the baud rate.
      -- The mantissa goes in the leftmost 12 bits, the fraction in the remaining 4.
      USART2.BRR := Baud_Rate_Register'(DIV_Mantissa => 16#445#,
                                        DIV_Fraction => 16#c#);
--        USART2.BRR.DIV_Mantissa := 16#445#;
--        USART2.BRR.DIV_Fraction := 16#c#;

      -- Turn on receiver and transmitter, & enable RX interrupts. This will also send an empty
      -- frame as a side effect of turning on TE.
      USART2.CR1 := Control_Register_1'(Send_Break => False,
                                        Receiver_WakeUp => False,
                                        Receiver_Enable => True,
                                        Transmitter_Enable => True,
                                        IDLE_Interrupt_Enable => False,
                                        RXNE_Interrupt_Enable => True,
                                        Transmission_Complete_Interrupt => False,
                                        TXE_Interrupt_Enable => False,
                                        PE_Interrupt_Enable => False,
                                        Parity_Selection => ODD,
                                        Parity_Control_Enable => False,
                                        WakeUp_Method => IDLE_LINE,
                                        Length => EIGHT_BITS,
                                        USART_Enable => True,
                                        Oversampling_Mode => OVERSAMPLING_BY_8);
   end;

   procedure Read(Buffer : out String; Characters_Read : out Natural) is
   begin
      Serial_Buffer.Read_Until(Buffer, ASCII.NUL, Characters_Read);
   end Read;

   procedure Read_Line(Buffer : out String; Characters_Read : out Natural) is
   begin
      Serial_Buffer.Read_Until(Buffer, ASCII.LF, Characters_Read);
   end Read_Line;

   procedure Write(Message : in String) is
   begin
      for Char of Message loop
         Write(Char);
      end loop;
   end Write;

   procedure Write(Message : in Character) is
   begin
      -- Step 1: Wait until TXE bit is clear. This indicates that the previous frame has been
      -- transmitted and we can safely write our data to the data register.
      loop
         exit when USART2.SR.Transmit_Data_Register_Empty;
      end loop;
      -- Step 2: Write the character to the data register to transmit it.
      USART2.DR.Data_Value := Char_To_Bits(Message);
   end Write;

   procedure Write_Line(Message : in String) is
   begin
      Write(Message);
      Write(ASCII.LF);
   end Write_Line;

   protected body Serial_Buffer is

      -- Interrupt handler which is called when new data is received by the serial port.
      procedure Interrupt_Handler is
         Received_Char : Character := ' ';
      begin
         -- read from DR clears Interrupt in USART
         Received_Char := Bits_To_Char (USART2.DR.Data_Value);

         -- If the buffer is full, we'll simply drop the read characters until room is available.
         if Buffered_Chars < Receive_Buffer'Length then
            Receive_Buffer(Receive_Buffer'First + Buffered_Chars) := Received_Char;
            Buffered_Chars := Buffered_Chars + 1;
         end if;
      end Interrupt_Handler;

      procedure Read_Until(Buffer : out String; End_Char : in Character; Chars_Read : out Natural)
      is
         Buffered_Char : Character;
         Read_Chars : Natural := 0;
      begin
         -- Read all of the chars into the given buffer until we hit the end char or run out.
         Read_Chars := 0;
         for I in 0..(Buffered_Chars - 1) loop
            Buffered_Char := Receive_Buffer(Receive_Buffer'First + I);
            exit when Buffered_Char = End_Char;
            Buffer(Buffer'First + I) := Buffered_Char;
            Read_Chars := Read_Chars + 1;
         end loop;
         -- Shift the serial buffer so that the remaining characters are at the front.
         for I in Read_Chars..Buffered_Chars loop
            Receive_Buffer(Receive_Buffer'First + I - Read_Chars) :=
              Receive_Buffer(Receive_Buffer'First + I);
         end loop;

         -- Null terminate the buffer.
         Buffered_Chars := Buffered_Chars - Read_Chars;
         Receive_Buffer(Receive_Buffer'First + Buffered_Chars) := ASCII.NUL;
         Chars_Read := Read_Chars;
      end Read_Until;

   end Serial_Buffer;

end Serial_Port;
