------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with LEDs;          use LEDs;
with Button;        use Button;
with Serial_Port;
with Ada.Real_Time; use Ada.Real_Time;

with System.STM32F4;
use System.STM32F4;

with System.STM32F4.Reset_Clock_Control;
use System.STM32F4.Reset_Clock_Control;

with System.STM32F4.Timers.GeneralPurpose;
use System.STM32F4.Timers.GeneralPurpose;

with Ada.Interrupts;
with Ada.Interrupts.Names;

package body Driver is

   function Delay_Period(Period : in Blink_Period) return Time_Span is
   begin
      case Period is
         when Long => return Milliseconds(2500);
         when Medium => return Milliseconds(1000);
         when Short => return Milliseconds(500);
      end case;
   end Delay_Period;

   protected Timer is
      procedure start;
      procedure interrupt;
      pragma Attach_Handler (interrupt, Ada.Interrupts.Names.TIM4_Interrupt);
   private
      flag : Boolean := False;
   end Timer;

   protected body Timer is
      procedure Start is
      begin
--           RCC.APB1ENR := RCC.APB1ENR or 2#10#;
         RCC.RCC_APB1ENR.TIM4_Clock_Enable := True;

         TIM4.TIM_DIER := DMA_Interrupt_Enable_Register'(Update_Interrupt => True,
                                                         Others => False);

         TIM4.TIM_ARR := 16#FFFF#;
         TIM4.TIM_PSC := 641;
--           TIM2.TIM_CCER := Capture_Compare_Enable_Register'(CC1E => True,
--                                                             CC1P => LOW,
--                                                             CC1NP => False,
--                                                             CC2E => False,
--                                                             CC2P => LOW,
--                                                             CC2NP => False,
--                                                             CC3E => False,
--                                                             CC3P => LOW,
--                                                             CC3NP => False,
--                                                             CC4E => False,
--                                                             CC4P => LOW,
--                                                             CC4NP => False);
--           TIM2.TIM_CCMR1.CC1_Element :=
--             Capture_Compare_Element'(Mode  => OUTPUT,
--                                      OCFE  => False,
--                                      OCPE  => False,
--                                      OCM   => TOGGLE,
--                                      OCCE  => False);

--           TIM2.TIM_CCRs (1).Low_Value := 16#FFFF#;
         TIM4.TIM_CR1 :=
           Control_Register_1'(
                               CKD => IDENTICAL,
                               Autoreload_Preload_Enable => False,
                               CMS => EDGE_ALIGNED,
                               Counter_Enable => True,
                               Update_Disable => False,
                               One_Pulse_Mode => False,
                               Direction => UPCOUNTING,
                               Update_Request_Source => True);

         flag := True;
      end Start;

      procedure interrupt is
      begin
          TIM4.TIM_SR.Update_Interrupt_Flag := False;

         if not flag then
            All_On;
            flag := True;
         else
            All_Off;
            flag := False;
         end if;
      end interrupt;
   end Timer;

   task body LED_Controller is
      Next_Start : Time := Clock;
      Light_On : Boolean := False;

   begin

      All_Off;
      Timer.start;
--        loop
--           if not Light_On then
--              All_On;
--              Light_On := True;
--           else
--              All_Off;
--              Light_On := False;
--           end if;
--
--           -- Note: Tasks having the same priority need yield control to each other or risk locking
--           --  each other out of the processor. Using a delay accomplishes this.
--           Next_Start := Next_Start + Delay_Period(Button.Blink_Speed);
--           delay until Next_Start;
--        end loop;
   end LED_Controller;

   -- A task which echoes back any characters read on the serial port.
   task body Serial_Controller is
      Next_Check : Time := Clock;
      Check_Interval : constant Time_Span := Milliseconds(100);
      Line_Buffer : String(1..512);
      Characters_Read : Natural := 0;
   begin
      Serial_Port.Enable(2_400);
      Serial_Port.Write_Line("Serial online!");
      loop
         begin
            Serial_Port.Read(Line_Buffer, Characters_Read);
            if Characters_Read > 0 then
               Serial_Port.Write(Line_Buffer(Line_Buffer'First..(Line_Buffer'First + Characters_Read)));
            end if;
            Next_Check := Next_Check + Check_Interval;
            delay until Next_Check;
         exception
            when Constraint_Error =>
            null; -- Ignore constraint errors
         end;
      end loop;
   end Serial_Controller;

end Driver;
