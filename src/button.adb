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

with Ada.Interrupts.Names;
with System.STM32F4; use System.STM32F4;

with System.STM32F4.GPIO; use System.STM32F4.GPIO;

with System.STM32F4.Reset_Clock_Control;
use System.STM32F4.Reset_Clock_Control;

with System.STM32F4.Timers.GeneralPurpose;
use System.STM32F4.Timers.GeneralPurpose;

with System.STM32F4.External_Interrupts;
use System.STM32F4.External_Interrupts;

with System.STM32F4.System_Configuration;
use System.STM32F4.System_Configuration;

with Ada.Real_Time; use Ada.Real_Time;

package body Button is

   protected Button is
      pragma Interrupt_Priority;

      function Blink_Speed return Blink_Period;

   private
      procedure Interrupt_Handler;
      pragma Attach_Handler
         (Interrupt_Handler,
          Ada.Interrupts.Names.EXTI15_10_Interrupt);

      Current_Speed : Blink_Period := Short;
      Last_Time : Time := Clock;
   end Button;

   Debounce_Time : constant Time_Span := Milliseconds (500);

   protected body Button is

      function Blink_Speed return Blink_Period is
      begin
         return Current_Speed;
      end Blink_Speed;

      procedure Interrupt_Handler is
         Now : constant Time := Clock;
      begin
         --  Clear interrupt
         EXTI.PR (13) := True;

         --  Debouncing
         if Now - Last_Time >= Debounce_Time then
            -- Send an event on TIM3
            TIM3.TIM_EGR.Update_Generation := True;

            case Current_Speed is
               when Long => Current_Speed := Short;
               when Medium => Current_Speed := Long;
               when Short => Current_Speed := Medium;
            end case;

            Last_Time := Now;
         end if;
      end Interrupt_Handler;

   end Button;

   function Blink_Speed return Blink_Period is
   begin
      return Button.Blink_Speed;
   end Blink_Speed;

   procedure Initialize is
      begin
      --  Enable clock for GPIOC
      RCC.RCC_AHB1ENR.GPIOC_Clock_Enable := True;
      RCC.RCC_APB2ENR.Sys_Config_Clock_Enable := True;

      --  Configure PC13
      GPIOC.MODER (13) := Mode_IN;
      GPIOC.PUPDR (13) := No_Pull;

      --  Select PC13 for EXTI13
      -- See Page 139 of the RM0383 datasheet
      SYSCFG.SYSCFG_EXTICR(4).Sources(1) := PORT_C;

      --  Interrupt on falling edge
      EXTI.FTSR (13) := True;
      EXTI.RTSR (13) := False;

      -- Disable all other interrupts but ours
      EXTI.IMR := (13 => True, others => False);

      --  Enable clock for TIM3
      RCC.RCC_APB1ENR.TIM3_Clock_Enable := True;

   end Initialize;

begin
   Initialize;
end Button;
